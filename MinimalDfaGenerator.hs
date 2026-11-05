-- ============================================================================
-- GERADOR DE AFD LIVRE DE ESTADOS INALCANÇÁVEIS E MORTOS
-- Linguagens Formais e Autômatos - Ciência da Computação - UFFS, Campus Chapecó
-- Autores: Arthur Antonio Bertoglio e Davi Augusto Marangoni Mocva
--
-- Etapas:
--   arquivo -> AFND (carga) -> AFD (determinização)
--           -> AFD minimizado (remove inalcançáveis e mortos)
--           -> AFD completo (estado de erro)
--
-- Execução: runghc MinimalDfaGenerator.hs <arquivo-de-entrada.txt>
--
-- Exemplo do enunciado, usado nos comentários:
--   se
--   entao
--   senao
--   <S> ::= a<A> | e<A> | i<A> | o<A> | u<A>
--   <A> ::= a<A> | e<A> | i<A> | o<A> | u<A> | ε
--
-- NFA = AFND e DFA = AFD. "Livro": Aho et al., Compilers, 2. ed.
-- "Apostila": Scheffel, Linguagens Formais e Autômatos.
-- ============================================================================
module Main (main) where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, nub, partition, transpose, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, stderr, stdout, utf8, utf8_bom)


-- ============================================================================
-- 1. TIPOS
--
-- Tabela de transições: dicionário de (estado, símbolo) para o destino. Uma chave
-- ausente é uma célula vazia.
-- ============================================================================

type StateName = String
type Symbol = Char

-- | Célula do AFND: conjunto de destinos. Exemplo: (S, s) -> {A, H}.
type TransitionMap = Map.Map (StateName, Symbol) (Set.Set StateName)

-- | Estado do AFD: conjunto de estados do AFND. Exemplo: {A,H}.
type DfaState = Set.Set StateName

-- | Célula do AFD: no máximo um destino.
type DfaTransitionMap = Map.Map (DfaState, Symbol) DfaState

data Nfa = Nfa
    { nfaStates :: [StateName]
    , nfaStartState :: StateName
    , nfaAlphabet :: [Symbol]
    , nfaTransitions :: TransitionMap
    , nfaFinalStates :: Set.Set StateName
    }

data Dfa = Dfa
    { dfaStates :: [DfaState]
    , dfaStartState :: DfaState
    , dfaAlphabet :: [Symbol]
    , dfaTransitions :: DfaTransitionMap
    , dfaFinalStates :: Set.Set DfaState
    }

-- | Estado de erro: o conjunto vazio, pois nenhum estado do AFND está ativo nele
-- A determinização nunca gera esse conjunto.
errorState :: DfaState
errorState = Set.empty


-- ============================================================================
-- 2. LEITURA DA ENTRADA
--
-- Separa o arquivo em tokens e gramáticas e rejeita produções inválidas.
-- ============================================================================

-- | Alternativa de uma produção regular: ε, a ou a<B>.
data Alt
    = Epsilon
    | TermOnly Symbol
    | TermToNonTerminal Symbol String

data Production = Production
    { pLhs :: String
    , pAlts :: [Alt]
    }

data Input = Input
    { inputTokens :: [String]
    , inputGrammars :: [[Production]]
    }

-- | Cada linha vira token ou produção, sem espaços nem \r nas pontas.
parseInput :: String -> Either String Input
parseInput contents = do
    let inputs = filter (not . null) (map trim (lines contents))
        (grammarLines, tokens) = partition isProductionLine inputs
    productions <- mapM parseProduction grammarLines
    Right Input {inputTokens = tokens, inputGrammars = groupGrammars productions}

-- | Produção começa com um não-terminal (<nome>). Operadores como <, <= e <> não têm
-- nome válido e continuam sendo tokens.
isProductionLine :: String -> Bool
isProductionLine ('<' : rest) =
    case span isNameChar rest of
        (_ : _, '>' : _) -> True
        _ -> False
isProductionLine _ = False

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- | Cada novo <S> abre uma gramática, e os não-terminais de cada uma ficam separados.
-- Produções anteriores ao primeiro <S> entram na primeira gramática.
groupGrammars :: [Production] -> [[Production]]
groupGrammars [] = []
groupGrammars productions =
    case break isStartProduction productions of
        (beforeStart, []) -> [beforeStart]
        (beforeStart, start : afterStart) ->
            let (body, others) = break isStartProduction afterStart
             in (beforeStart ++ start : body) : groupGrammars others
  where
    isStartProduction production = pLhs production == "S"

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy sep xs =
    case break (== sep) xs of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitBy sep rest

-- | Lê <A> ::= a<A> | ε: separa no ::= e depois em cada |.
parseProduction :: String -> Either String Production
parseProduction line =
    let (lhsPart, rhsPartRaw) = breakSubstring "::=" line
     in if null rhsPartRaw
            then Left ("Invalid grammar line (missing ::=): " ++ line)
            else do
                lhs <- parseNonTerminal (trim lhsPart)
                let rhsPart = trim (drop 3 rhsPartRaw)
                alts <- mapM parseAlt (map trim (splitBy '|' rhsPart))
                Right Production {pLhs = lhs, pAlts = alts}
  where
    breakSubstring needle haystack = search [] haystack
      where
        search _ [] = (haystack, [])
        search acc remaining@(x : xs)
            | needle `isPrefixOf` remaining = (reverse acc, remaining)
            | otherwise = search (x : acc) xs

parseNonTerminal :: String -> Either String String
parseNonTerminal text =
    case text of
        '<' : xs ->
            case reverse xs of
                '>' : revName ->
                    let name = reverse revName
                     in if null name || not (all isNameChar name)
                            then Left ("Invalid non-terminal: " ++ text)
                            else Right name
                _ -> Left ("Invalid non-terminal: " ++ text)
        _ -> Left ("Invalid non-terminal: " ++ text)

-- | O primeiro caractere é o terminal, e o resto é vazio ou um não-terminal. Assim,
-- ab<A> é rejeitada: a alternativa só pode ter um terminal.
parseAlt :: String -> Either String Alt
parseAlt "ε" = Right Epsilon
parseAlt "" = Left "Empty alternative in grammar"
parseAlt (symbol : restChars) =
    case trim restChars of
        "" -> Right (TermOnly symbol)
        rest -> TermToNonTerminal symbol <$> parseNonTerminal rest


-- ============================================================================
-- 3. CARGA: TOKENS E GRAMÁTICAS VIRAM UM ÚNICO AFND
--
-- Só o estado inicial é compartilhado; os demais são exclusivos de um token ou
-- gramática. O indeterminismo surge quando duas sentenças começam com o mesmo
-- símbolo: "se" e "senao" fazem a célula S/s ter os destinos A e H.
-- ============================================================================

-- | AFND em construção. bsCounter define o nome do próximo estado.
data BuildState = BuildState
    { bsStates :: [StateName]
    , bsTransitions :: TransitionMap
    , bsFinalStates :: Set.Set StateName
    , bsCounter :: Int
    }

initialState :: StateName
initialState = "S"

initialBuildState :: BuildState
initialBuildState = BuildState
    { bsStates = [initialState]
    , bsTransitions = Map.empty
    , bsFinalStates = Set.empty
    , bsCounter = 0
    }

-- | Carrega os tokens e depois as gramáticas. O alfabeto mantém a ordem de aparição,
-- que é a ordem das colunas no enunciado.
buildNfa :: Input -> Either String Nfa
buildNfa input = do
    let tokens = inputTokens input
        grammars = inputGrammars input
        stAfterTokens = foldl (flip buildTokenPath) initialBuildState tokens
    stFinal <- foldM buildGrammar stAfterTokens grammars
    Right
        Nfa
            { nfaStates = bsStates stFinal
            , nfaStartState = initialState
            , nfaAlphabet = nub (concat tokens ++ concatMap grammarSymbols grammars)
            , nfaTransitions = bsTransitions stFinal
            , nfaFinalStates = bsFinalStates stFinal
            }

grammarSymbols :: [Production] -> [Symbol]
grammarSymbols productions = concatMap altSymbol (concatMap pAlts productions)
  where
    altSymbol Epsilon = []
    altSymbol (TermOnly sym) = [sym]
    altSymbol (TermToNonTerminal sym _) = [sym]

-- | Nomes dos estados: 0 = A, 25 = Z, 26 = AA, 27 = AB.
stateLabelFromIndex :: Int -> String
stateLabelFromIndex n = reverse (go n)
  where
    go x =
        let (q, r) = x `divMod` 26
            ch = toEnum (fromEnum 'A' + r) :: Char
         in ch : if q == 0 then [] else go (q - 1)

-- | Cria um estado novo. O nome S é pulado porque é o do estado inicial.
allocateState :: BuildState -> (StateName, BuildState)
allocateState st
    | candidate == initialState = allocateState next
    | otherwise = (candidate, next {bsStates = bsStates st ++ [candidate]})
  where
    candidate = stateLabelFromIndex (bsCounter st)
    next = st {bsCounter = bsCounter st + 1}

-- | Mais de um destino na mesma célula é o que torna o autômato indeterminístico.
addTransition :: StateName -> Symbol -> StateName -> BuildState -> BuildState
addTransition from symbol to st =
    let key = (from, symbol)
        nextDests = Set.insert to (Map.findWithDefault Set.empty key (bsTransitions st))
     in st {bsTransitions = Map.insert key nextDests (bsTransitions st)}

markFinal :: StateName -> BuildState -> BuildState
markFinal stateName st = st {bsFinalStates = Set.insert stateName (bsFinalStates st)}

-- | Token: caminho a partir de S com um estado novo por símbolo, e o último é final.
-- "se" vira S -s-> A -e-> *B.
buildTokenPath :: String -> BuildState -> BuildState
buildTokenPath token st0 =
    let (endState, stN) =
            foldl
                (\(current, st) ch ->
                    let (next, st1) = allocateState st
                        st2 = addTransition current ch next st1
                     in (next, st2)
                )
                (initialState, st0)
                token
     in markFinal endState stN

-- | Gramática: cada não-terminal vira um estado novo, exceto <S>, que é o inicial.
-- No exemplo, <A> vira M:
--
--   A ::= a<B>  ->  transição de A para B com a
--   A ::= a     ->  transição de A para um estado novo e final
--   A ::= ε     ->  A vira final
--
-- Uma produção que volta para <S> aponta para uma cópia privada dele. Uma aresta
-- entrando no inicial compartilhado misturaria as sentenças: com o token ok e
-- <S> ::= a<S> | b, a palavra aok seria aceita sem pertencer a nenhuma delas.
buildGrammar :: BuildState -> [Production] -> Either String BuildState
buildGrammar st0 productions = foldM applyProduction st1 productions
  where
    nonTerminals = nub (map pLhs productions)
    -- A cópia de <S> só existe se alguma produção volta para ele.
    startIsRecursive = any goesToStart (concatMap pAlts productions)
    privateStates = filter (\nt -> nt /= "S" || startIsRecursive) nonTerminals
    -- Não-terminal -> estado, local a esta gramática.
    (ntMap, st1) = foldl mapNonTerminal (Map.empty, st0) privateStates

    goesToStart (TermToNonTerminal _ "S") = True
    goesToStart _ = False

    mapNonTerminal (m, st) nt =
        let (newState, st') = allocateState st
         in (Map.insert nt newState m, st')

    lookupNtState nt =
        case Map.lookup nt ntMap of
            Just s -> Right s
            Nothing -> Left ("Unknown non-terminal: <" ++ nt ++ ">")

    -- Produções de <S> saem do inicial e também da cópia, se ela existir.
    sourceStates "S" = initialState : maybe [] pure (Map.lookup "S" ntMap)
    sourceStates nt = maybe [] pure (Map.lookup nt ntMap)

    applyProduction st production =
        foldM
            (\acc fromState -> foldM (applyAlt fromState) acc (pAlts production))
            st
            (sourceStates (pLhs production))

    applyAlt fromState st alt =
        case alt of
            Epsilon -> Right (markFinal fromState st)
            TermOnly sym ->
                let (finalState, st') = allocateState st
                    st2 = addTransition fromState sym finalState st'
                 in Right (markFinal finalState st2)
            TermToNonTerminal sym nt -> do
                toState <- lookupNtState nt
                Right (addTransition fromState sym toState st)


-- ============================================================================
-- 4. DETERMINIZAÇÃO
--
-- Construção de subconjuntos: cada estado do AFD é
-- o conjunto de estados em que o AFND pode estar após ler o mesmo prefixo. Lendo s
-- em S, o AFND vai para A ou H, então o AFD ganha {S} -s-> {A,H}.
--
-- Não há ε-closure: uma produção A ::= ε só marca A como final, então o AFND não
-- tem transições ε.
-- ============================================================================

-- | Busca em largura a partir de {S}, que só gera subconjuntos alcançáveis. Um subconjunto
-- é final se contém um estado final do AFND: {C,M} é final por causa de M.
buildDfa :: Nfa -> Dfa
buildDfa nfa =
    let alphabet = nfaAlphabet nfa
        startState = Set.singleton (nfaStartState nfa)
        (orderedStates, transitions) = discover [startState] (Set.singleton startState) [] Map.empty
        finals = Set.fromList (filter isFinal orderedStates)
        isFinal dfaState = not . Set.null $ Set.intersection dfaState (nfaFinalStates nfa)
     in Dfa
            { dfaStates = orderedStates
            , dfaStartState = startState
            , dfaAlphabet = alphabet
            , dfaTransitions = transitions
            , dfaFinalStates = finals
            }
  where
    -- Processa a fila de subconjuntos descobertos; cada um vira uma linha da tabela.
    discover [] _ ordered transitions = (ordered, transitions)
    discover (current : queue) visited ordered transitions =
        let (queue', visited', transitions') =
                foldl
                    (step current)
                    (queue, visited, transitions)
                    (nfaAlphabet nfa)
         in discover queue' visited' (ordered ++ [current]) transitions'

    -- Preenche uma célula. Destino vazio deixa a célula vazia; destino novo entra na fila.
    step current (queue, visited, transitions) symbol =
        let next = move current symbol
         in if Set.null next
                then (queue, visited, transitions)
                else
                    let transitions' = Map.insert (current, symbol) next transitions
                     in if Set.member next visited
                            then (queue, visited, transitions')
                            else (queue ++ [next], Set.insert next visited, transitions')

    -- move(T, a): união dos destinos de todos os estados de T com o símbolo a.
    move dfaState symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (stateName, symbol) (nfaTransitions nfa)
            | stateName <- Set.toList dfaState
            ]


-- ============================================================================
-- 5. MINIMIZAÇÃO
--
-- eliminar inacessíveis, eliminar mortos e
-- construir classes de equivalência. O enunciado proíbe o terceiro, então estados
-- são removidos e nunca fundidos. Fundir juntaria os finais de tokens diferentes,
-- como {G} (entao) e {L} (senao), e perderia o token reconhecido.
--
-- Uma passada de cada remoção basta, em qualquer ordem: sucessor de alcançável é
-- alcançável e antecessor de vivo é vivo.
-- ============================================================================

-- | Busca para a frente a partir do estado inicial. Na saída da determinização não
-- acha nada; fica explícita para o requisito não depender de como o AFD foi gerado.
removeUnreachableStates :: Dfa -> Dfa
removeUnreachableStates dfa =
    restrictTo (reachableFrom (neighbours forward) [dfaStartState dfa]) dfa
  where
    forward = [(from, to) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]

-- | Busca para trás a partir dos finais: quem não é visitado está morto. O inicial
-- sempre fica, mesmo morto, porque a linguagem vazia ainda precisa de um AFD.
removeDeadStates :: Dfa -> Dfa
removeDeadStates dfa =
    restrictTo (Set.insert (dfaStartState dfa) live) dfa
  where
    backward = [(to, from) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]
    live = reachableFrom (neighbours backward) (Set.toList (dfaFinalStates dfa))

neighbours :: [(DfaState, DfaState)] -> DfaState -> [DfaState]
neighbours edges =
    let table = Map.fromListWith (++) [(from, [to]) | (from, to) <- edges]
     in \state -> Map.findWithDefault [] state table

-- | Busca em grafo usada pelas duas remoções, que mudam só o início e o sentido das
-- arestas. Estados já vistos são ignorados, então ciclos não atrapalham.
reachableFrom :: (DfaState -> [DfaState]) -> [DfaState] -> Set.Set DfaState
reachableFrom next = go Set.empty
  where
    go seen [] = seen
    go seen (state : pending)
        | Set.member state seen = go seen pending
        | otherwise = go (Set.insert state seen) (next state ++ pending)

-- | Remove os estados fora de keep e suas transições. As células que apontavam para
-- eles ficam vazias e depois vão para o estado de erro.
restrictTo :: Set.Set DfaState -> Dfa -> Dfa
restrictTo keep dfa =
    dfa
        { dfaStates = filter kept (dfaStates dfa)
        , dfaTransitions =
            Map.filterWithKey (\(from, _) to -> kept from && kept to) (dfaTransitions dfa)
        , dfaFinalStates = Set.intersection keep (dfaFinalStates dfa)
        }
  where
    kept state = Set.member state keep


-- ============================================================================
-- 6. ESTADO DE ERRO
--
-- Enunciado: "acrescentar um último estado final". Células vazias apontam para ele,
-- e suas transições voltam para ele. A tabela fica completa.
-- ============================================================================

-- | O erro é final, logo não é um estado morto. Ser
-- final aqui não significa aceitar: a leitura para e informa o erro.
--
-- Sem células vazias, o AFD não muda: o estado de erro seria inalcançável.
addErrorState :: Dfa -> Dfa
addErrorState dfa
    | null unmappedCells = dfa
    | otherwise =
        dfa
            { dfaStates = dfaStates dfa ++ [errorState]
            , dfaTransitions = Map.union (dfaTransitions dfa) errorTransitions
            , dfaFinalStates = Set.insert errorState (dfaFinalStates dfa)
            }
  where
    alphabet = dfaAlphabet dfa
    unmappedCells =
        [ (state, symbol)
        | state <- dfaStates dfa
        , symbol <- alphabet
        , Map.notMember (state, symbol) (dfaTransitions dfa)
        ]
    errorLoops = [(errorState, symbol) | symbol <- alphabet]
    errorTransitions = Map.fromList [(cell, errorState) | cell <- unmappedCells ++ errorLoops]


-- ============================================================================
-- 7. IMPRESSÃO DAS TABELAS
--
-- Linhas são estados, colunas são símbolos e * marca os estados finais.
-- ============================================================================

renderNfaTable :: Nfa -> String
renderNfaTable nfa = renderTable (nfaAlphabet nfa) (map row (nfaStates nfa))
  where
    row state = renderStateCell (Set.member state (nfaFinalStates nfa)) state : map (cell state) (nfaAlphabet nfa)
    cell state symbol =
        intercalate "," (Set.toList (Map.findWithDefault Set.empty (state, symbol) (nfaTransitions nfa)))

renderDfaTable :: Dfa -> String
renderDfaTable dfa = renderTable (dfaAlphabet dfa) (map row (dfaStates dfa))
  where
    row state =
        renderStateCell (Set.member state (dfaFinalStates dfa)) (renderDfaState state)
            : map (cell state) (dfaAlphabet dfa)
    cell state symbol = maybe "" renderDfaState (Map.lookup (state, symbol) (dfaTransitions dfa))

-- | Estado do AFD com o nome dos estados do AFND que ele representa, como {A,H}.
renderDfaState :: DfaState -> String
renderDfaState state
    | state == errorState = "ERR"
    | otherwise = "{" ++ intercalate "," (Set.toList state) ++ "}"

renderStateCell :: Bool -> String -> String
renderStateCell isFinal name = if isFinal then '*' : name else name

renderTable :: [Symbol] -> [[String]] -> String
renderTable alphabet rowCells =
    let headerCells = "δ" : map pure alphabet
        widths = map (maximum . map length) (transpose (headerCells : rowCells))
        header = renderPaddedRow widths headerCells
        rows = map (renderPaddedRow widths) rowCells
        divider = replicate (length header) '-'
     in unlines (header : divider : rows)

renderPaddedRow :: [Int] -> [String] -> String
renderPaddedRow widths cells = intercalate " | " (zipWith padRight widths cells)

padRight :: Int -> String -> String
padRight width text = text ++ replicate (width - length text) ' '


-- ============================================================================
-- 8. PROGRAMA PRINCIPAL
-- ============================================================================

main :: IO ()
main = do
    -- ε e δ não podem depender do locale da máquina.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs

    case args of
        [filePath] -> readAndProcessFile filePath
        _ -> die "Usage: runghc MinimalDfaGenerator.hs <input-file.txt>"

readAndProcessFile :: FilePath -> IO ()
readAndProcessFile filePath = do
    contents <- readInputFile filePath

    case parseInput contents >>= buildNfa of
        Left err -> die err
        Right nfa -> printPipeline nfa

-- | utf8_bom descarta o BOM de arquivos salvos no Windows.
readInputFile :: FilePath -> IO String
readInputFile filePath = do
    handle <- openFile filePath ReadMode
    hSetEncoding handle utf8_bom
    hGetContents handle

printPipeline :: Nfa -> IO ()
printPipeline nfa = do
    let dfa = buildDfa nfa
        reachableDfa = removeUnreachableStates dfa
        minimizedDfa = removeDeadStates reachableDfa
        minimizedWithError = addErrorState minimizedDfa
        -- Estado do AFND fora de todos os subconjuntos não é alcançável a partir de S.
        unreachableNfaStates = nfaStates nfa \\ Set.toList (Set.unions (dfaStates dfa))
    putStrLn "NFA:"
    putStrLn (renderNfaTable nfa)
    putStrLn "DFA:"
    putStrLn ("Unreachable NFA states left out: " ++ renderStateList unreachableNfaStates)
    putStrLn ""
    putStrLn (renderDfaTable dfa)
    putStrLn "Minimized DFA:"
    putStrLn ("Unreachable states removed: " ++ renderRemovedStates dfa reachableDfa)
    putStrLn ("Dead states removed: " ++ renderRemovedStates reachableDfa minimizedDfa)
    putStrLn ""
    putStrLn (renderDfaTable minimizedDfa)
    putStrLn "Minimized DFA + Error state:"
    putStrLn (renderDfaTable minimizedWithError)

renderRemovedStates :: Dfa -> Dfa -> String
renderRemovedStates before after =
    renderStateList (map renderDfaState (dfaStates before \\ dfaStates after))

renderStateList :: [String] -> String
renderStateList [] = "none"
renderStateList names = intercalate ", " names
