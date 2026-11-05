-- ============================================================================
-- GERADOR DE AFD LIVRE DE ESTADOS INALCANÇÁVEIS E MORTOS
--
-- Trabalho de Linguagens Formais e Autômatos
-- Ciência da Computação - UFFS, Campus Chapecó
-- Autores: Arthur Antonio Bertoglio e Davi Augusto Marangoni Mocva
--
--
-- O QUE O PROGRAMA FAZ
--
-- Lê um arquivo com sentenças (tokens e gramáticas regulares) e gera um
-- Autômato Finito Determinístico, passando por estas etapas:
--
--   arquivo --> AFND --> AFD --> AFD minimizado --> AFD com estado de erro
--            carga    determi-   remoção de         células vazias
--                     nização    inalcançáveis      apontam para o
--                                e mortos           estado de erro
--
-- O programa imprime a tabela de transições de cada etapa. A última tabela é o
-- resultado final.
--
--
-- COMO EXECUTAR
--
--   runghc MinimalDfaGenerator.hs <arquivo-de-entrada.txt>
--
-- Só é preciso ter o GHC instalado. As bibliotecas usadas já vêm com ele.
--
--
-- FORMATO DO ARQUIVO DE ENTRADA
--
-- Cada linha é um token ou uma produção de gramática em BNF. Linhas em branco
-- são ignoradas. Este é o exemplo do enunciado, usado nos comentários abaixo:
--
--   se
--   entao
--   senao
--
--   <S> ::= a<A> | e<A> | i<A> | o<A> | u<A>
--   <A> ::= a<A> | e<A> | i<A> | o<A> | u<A> | ε
--
-- Cada linha <S> abre uma nova gramática, então um arquivo pode ter várias.
--
--
-- ÍNDICE
--
--   1. Tipos ................ como os autômatos são representados
--   2. Leitura da entrada ... do texto do arquivo para tokens e produções
--   3. Carga ................ tokens e gramáticas viram um único AFND
--   4. Determinização ....... do AFND para o AFD
--   5. Minimização .......... remoção de estados inalcançáveis e mortos
--   6. Estado de erro ....... preenchimento das células vazias
--   7. Impressão ............ montagem das tabelas
--   8. Programa principal ... junta todas as etapas
--
--
-- GLOSSÁRIO
--
--   NFA = AFND e DFA = AFD (os nomes no código usam as siglas em inglês)
--   Token: sentença escrita direto no arquivo, como "se"
--   Terminal: símbolo que aparece na palavra, como o "a" de a<A>
--   Não-terminal: variável da gramática, como <A>
--   Célula: posição da tabela de transições, dada por (estado, símbolo)
--
-- Nos comentários, "livro" é Aho et al., Compilers: Principles, Techniques, and
-- Tools, 2. ed., e "apostila" é a apostila de LFA de Roberto M. Scheffel.
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
-- O enunciado deixa livre a estrutura de dados do autômato. Aqui a tabela de
-- transições é um dicionário (Map): a chave é a célula (estado, símbolo).
-- Quem não aparece no dicionário é uma célula vazia.
-- ============================================================================

type StateName = String
type Symbol = Char

-- | Tabela de transições do AFND. O valor é um conjunto porque no AFND uma célula
-- pode ter vários destinos. No exemplo, a célula ("S", 's') guarda {"A", "H"}.
type TransitionMap = Map.Map (StateName, Symbol) (Set.Set StateName)

-- | Um estado do AFD é o conjunto de estados do AFND que ele representa, como
-- manda o teorema da determinização. Exemplo: o estado {A,H}.
type DfaState = Set.Set StateName

-- | Tabela de transições do AFD: cada célula tem no máximo um destino.
type DfaTransitionMap = Map.Map (DfaState, Symbol) DfaState

-- | Autômato finito não determinístico (AFND). Os campos correspondem à definição
-- formal: estados, estado inicial, alfabeto, transições e estados finais.
data Nfa = Nfa
    { nfaStates :: [StateName]
    , nfaStartState :: StateName
    , nfaAlphabet :: [Symbol]
    , nfaTransitions :: TransitionMap
    , nfaFinalStates :: Set.Set StateName
    }

-- | Autômato finito determinístico (AFD).
data Dfa = Dfa
    { dfaStates :: [DfaState]
    , dfaStartState :: DfaState
    , dfaAlphabet :: [Symbol]
    , dfaTransitions :: DfaTransitionMap
    , dfaFinalStates :: Set.Set DfaState
    }

-- | O estado de erro é o conjunto vazio: nenhum estado do AFND está ativo nele.
-- O livro (p. 170) descreve o conjunto vazio da mesma forma. A determinização
-- nunca gera esse conjunto, então ele não se confunde com nenhum outro estado.
-- Na impressão, ele aparece como ERR.
errorState :: DfaState
errorState = Set.empty


-- ============================================================================
-- 2. LEITURA DA ENTRADA
--
-- Transforma o texto do arquivo em uma lista de tokens e uma lista de gramáticas.
--
-- Muitas funções aqui devolvem Either String: Left traz uma mensagem de erro e
-- Right traz o resultado. Em um bloco do com Either, o primeiro Left interrompe
-- todo o resto. É assim que um erro no arquivo chega até o usuário.
-- ============================================================================

-- | Uma alternativa do lado direito de uma produção. São as três formas permitidas
-- em uma gramática regular:
--
--   ε    -> Epsilon
--   a    -> TermOnly 'a'
--   a<B> -> TermToNonTerminal 'a' "B"
data Alt
    = Epsilon
    | TermOnly Symbol
    | TermToNonTerminal Symbol String

-- | Uma linha de gramática. @<A> ::= a<A> | ε@ vira o lado esquerdo pLhs = "A" e
-- as duas alternativas em pAlts.
data Production = Production
    { pLhs :: String
    , pAlts :: [Alt]
    }

-- | Tudo o que foi lido do arquivo. Cada gramática é uma lista de produções, e
-- inputGrammars é a lista de gramáticas.
data Input = Input
    { inputTokens :: [String]
    , inputGrammars :: [[Production]]
    }

-- | Separa as linhas em tokens e produções. Os dois tipos podem aparecer misturados
-- no arquivo.
--
-- O trim remove os espaços nas pontas de cada linha e também o \r que os arquivos
-- do Windows têm no fim das linhas. O mapM aplica parseProduction em todas as
-- linhas e falha se alguma delas falhar.
parseInput :: String -> Either String Input
parseInput contents = do
    let inputs = filter (not . null) (map trim (lines contents))
        (grammarLines, tokens) = partition isProductionLine inputs
    productions <- mapM parseProduction grammarLines
    Right Input {inputTokens = tokens, inputGrammars = groupGrammars productions}

-- | Uma linha é produção quando começa com um não-terminal, como @<S>@. Os operadores
-- @<@, @<=@ e @<>@ não têm um nome válido entre < e >, então continuam sendo tokens.
-- O enunciado cita operadores, por isso não basta olhar se a linha começa com <.
--
-- O span separa o início da string formado por caracteres de nome. O padrão
-- (_ : _, '>' : _) exige ao menos um caractere de nome seguido de >.
isProductionLine :: String -> Bool
isProductionLine ('<' : rest) =
    case span isNameChar rest of
        (_ : _, '>' : _) -> True
        _ -> False
isProductionLine _ = False

-- | Caracteres permitidos no nome de um não-terminal.
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- | Divide as produções em gramáticas: cada novo @<S>@ abre uma nova gramática. Isso
-- permite que duas gramáticas usem o mesmo nome, como @<A>@, sem misturar seus
-- estados.
--
-- O break corta a lista na primeira produção de @<S>@. Depois, ele é usado de novo
-- para achar o próximo @<S>@, e tudo entre os dois forma uma gramática. As linhas
-- que vêm antes do primeiro @<S>@ também entram nessa gramática.
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

-- | Remove os espaços do fim e do início. Como dropWhile só atua no começo da string,
-- ela é invertida para tratar o final.
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Divide uma string em um separador: "a|b" com '|' vira ["a", "b"].
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy sep xs =
    case break (== sep) xs of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitBy sep rest

-- | Interpreta uma linha de gramática, como @<A> ::= a<A> | ε@: corta no ::=, lê o
-- não-terminal da esquerda, divide a direita em | e lê cada alternativa.
parseProduction :: String -> Either String Production
parseProduction line =
    let (lhsPart, rhsPartRaw) = breakSubstring "::=" line
     in if null rhsPartRaw
            then Left ("Invalid grammar line (missing ::=): " ++ line)
            else do
                lhs <- parseNonTerminal (trim lhsPart)
                -- drop 3 tira o próprio ::= do início do lado direito.
                let rhsPart = trim (drop 3 rhsPartRaw)
                alts <- mapM parseAlt (map trim (splitBy '|' rhsPart))
                Right Production {pLhs = lhs, pAlts = alts}
  where
    -- Corta o texto em duas partes, no ponto onde o trecho procurado aparece. A
    -- segunda parte começa com o trecho e fica vazia quando ele não existe.
    breakSubstring needle haystack = search [] haystack
      where
        search _ [] = (haystack, [])
        -- O nome remaining@(x : xs) dá acesso à lista inteira e às suas partes.
        search acc remaining@(x : xs)
            | needle `isPrefixOf` remaining = (reverse acc, remaining)
            | otherwise = search (x : acc) xs

-- | Extrai o nome de um não-terminal: "<A>" vira "A". Para conferir o > do final, a
-- string é invertida. Tudo fora desse formato é erro.
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

-- | Interpreta uma alternativa. O primeiro caractere é o terminal e o resto é vazio
-- ou um não-terminal. Com isso, ab<A> dá erro: gramática regular só aceita um
-- terminal por alternativa. O <$> aplica o construtor ao resultado de
-- parseNonTerminal, se ele não for um erro.
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
-- Enunciado: "apenas o estado inicial é compartilhado entre diferentes
-- tokens/gramáticas". Todos os outros estados pertencem a um único token ou
-- gramática.
--
-- O indeterminismo nasce aqui: quando duas sentenças começam com o mesmo símbolo,
-- uma célula de S ganha dois destinos. No exemplo, "se" e "senao" fazem a célula
-- S/s ter os destinos A e H.
--
-- Resultado no exemplo (igual à tabela do enunciado):
--
--   δ  | s   | e   | n | t | a | o | i | u
--   S  | A,H | C,M |   |   | M | M | M | M
--   A  |     | B   |   |   |   |   |   |
--   *B |     |     |   |   |   |   |   |      <- fim do token "se"
--   ...
--   *M |     | M   |   |   | M | M | M | M    <- <A> da gramática
-- ============================================================================

-- | O AFND em construção. Haskell não altera valores: cada função abaixo recebe um
-- BuildState e devolve um novo, com a mudança aplicada. bsCounter é o número usado
-- para dar nome ao próximo estado.
data BuildState = BuildState
    { bsStates :: [StateName]
    , bsTransitions :: TransitionMap
    , bsFinalStates :: Set.Set StateName
    , bsCounter :: Int
    }

initialState :: StateName
initialState = "S"

-- | Começa só com o estado inicial, sem nenhuma transição.
initialBuildState :: BuildState
initialBuildState = BuildState
    { bsStates = [initialState]
    , bsTransitions = Map.empty
    , bsFinalStates = Set.empty
    , bsCounter = 0
    }

-- | Carrega primeiro todos os tokens e depois todas as gramáticas.
--
-- O foldl aplica uma função a cada item, passando o resultado adiante. O foldM faz o
-- mesmo, mas cada passo pode falhar: o primeiro Left interrompe tudo. As gramáticas
-- precisam dele porque podem ter erros, e os tokens não.
--
-- O nub remove repetidos e mantém a ordem da primeira aparição. Assim, as colunas
-- saem na mesma ordem da tabela do enunciado: s e n t a o i u.
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

-- | Lista os terminais usados nas produções de uma gramática.
grammarSymbols :: [Production] -> [Symbol]
grammarSymbols productions = concatMap altSymbol (concatMap pAlts productions)
  where
    altSymbol Epsilon = []
    altSymbol (TermOnly sym) = [sym]
    altSymbol (TermToNonTerminal sym _) = [sym]

-- | Transforma um número em nome de estado, como as colunas de uma planilha: 0 = A,
-- 25 = Z, 26 = AA, 27 = AB. O go gera as letras da última para a primeira, por isso
-- o resultado é invertido no final.
stateLabelFromIndex :: Int -> String
stateLabelFromIndex n = reverse (go n)
  where
    go x =
        let (q, r) = x `divMod` 26
            ch = toEnum (fromEnum 'A' + r) :: Char
         in ch : if q == 0 then [] else go (q - 1)

-- | Cria um estado novo e devolve seu nome junto com o BuildState atualizado. O nome S
-- é pulado porque pertence ao estado inicial. É por isso que, em autômatos grandes,
-- a sequência de nomes vai de R direto para T.
allocateState :: BuildState -> (StateName, BuildState)
allocateState st
    | candidate == initialState = allocateState next
    | otherwise = (candidate, next {bsStates = bsStates st ++ [candidate]})
  where
    candidate = stateLabelFromIndex (bsCounter st)
    next = st {bsCounter = bsCounter st + 1}

-- | Acrescenta um destino à célula (from, symbol), mantendo os que já existem.
addTransition :: StateName -> Symbol -> StateName -> BuildState -> BuildState
addTransition from symbol to st =
    let key = (from, symbol)
        nextDests = Set.insert to (Map.findWithDefault Set.empty key (bsTransitions st))
     in st {bsTransitions = Map.insert key nextDests (bsTransitions st)}

markFinal :: StateName -> BuildState -> BuildState
markFinal stateName st = st {bsFinalStates = Set.insert stateName (bsFinalStates st)}

-- | Carrega um token: cria um caminho a partir de S, com um estado novo para cada
-- símbolo, e marca o último como final. O token "se" vira S -s-> A -e-> *B.
--
-- O foldl percorre os caracteres carregando o par (estado atual, BuildState).
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

-- | Carrega uma gramática. Cada não-terminal ganha um estado novo, o que permite que
-- duas gramáticas usem o mesmo nome, como @<A>@, sem compartilhar estados. No
-- exemplo, @<A>@ vira o estado M.
--
-- Cada alternativa vira transição assim (apostila, Teorema 3.2, p. 22):
--
--   A ::= a<B>  ->  transição de A para B com o símbolo a
--   A ::= a     ->  transição de A para um estado novo e final
--   A ::= ε     ->  A vira estado final
--
-- @<S>@ é o estado inicial compartilhado. Quando uma produção volta para @<S>@, ela
-- aponta para uma cópia privada dele. Sem isso, a leitura poderia seguir para os
-- tokens das outras sentenças: com o token ok e @<S> ::= a<S> | b@, a palavra aok
-- seria aceita, e ela não pertence a nenhuma das duas sentenças.
buildGrammar :: BuildState -> [Production] -> Either String BuildState
buildGrammar st0 productions = foldM applyProduction st1 productions
  where
    nonTerminals = nub (map pLhs productions)
    -- Alguma produção volta para <S>? Só nesse caso a cópia é necessária.
    startIsRecursive = any goesToStart (concatMap pAlts productions)
    privateStates = filter (\nt -> nt /= "S" || startIsRecursive) nonTerminals
    -- ntMap diz qual estado representa cada não-terminal desta gramática. Por ser
    -- criado aqui, ele não é visto pelas outras gramáticas.
    (ntMap, st1) = foldl mapNonTerminal (Map.empty, st0) privateStates

    goesToStart (TermToNonTerminal _ "S") = True
    goesToStart _ = False

    mapNonTerminal (m, st) nt =
        let (newState, st') = allocateState st
         in (Map.insert nt newState m, st')

    -- Usar um não-terminal que nenhuma produção define é um erro de entrada.
    lookupNtState nt =
        case Map.lookup nt ntMap of
            Just s -> Right s
            Nothing -> Left ("Unknown non-terminal: <" ++ nt ++ ">")

    -- De quais estados saem as transições de uma produção. As de <S> saem do inicial
    -- e, se ela existir, também da cópia privada.
    sourceStates "S" = initialState : maybe [] pure (Map.lookup "S" ntMap)
    sourceStates nt = maybe [] pure (Map.lookup nt ntMap)

    -- Dois foldM aninhados: para cada estado de origem, aplica cada alternativa.
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
-- Teorema da determinização, ou construção de subconjuntos (livro, Algoritmo 3.20,
-- p. 153): cada estado do AFD é o conjunto de estados em que o AFND pode estar
-- depois de ler o mesmo prefixo.
--
-- No exemplo: lendo s a partir de S, o AFND pode ir para A ou H. Logo, o AFD ganha
-- o estado {A,H}, com uma única transição {S} -s-> {A,H}.
--
-- O livro também trata transições ε (ε-closure). Aqui elas não existem, porque uma
-- produção A ::= ε só marca A como final. Por isso, basta o move.
-- ============================================================================

-- | Explora os conjuntos em largura, a partir do estado inicial. Como só segue
-- transições que existem, nunca gera um estado inalcançável. Quando uma célula não
-- tem destino, ela fica vazia na tabela.
--
-- Um estado do AFD é final se contém pelo menos um estado final do AFND. Por exemplo,
-- {C,M} é final porque M é final.
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
    -- Laço principal, escrito como recursão. Parâmetros:
    --   queue       estados descobertos que ainda falta processar
    --   visited     estados já descobertos, para não repetir nenhum
    --   ordered     estados já processados, na ordem das linhas da tabela
    --   transitions transições encontradas até agora
    -- Quando a fila esvazia, a construção terminou.
    discover [] _ ordered transitions = (ordered, transitions)
    discover (current : queue) visited ordered transitions =
        let (queue', visited', transitions') =
                foldl
                    (step current)
                    (queue, visited, transitions)
                    (nfaAlphabet nfa)
         in discover queue' visited' (ordered ++ [current]) transitions'

    -- Preenche uma célula: a do estado atual com um símbolo. Destino vazio deixa a
    -- célula vazia. Um destino nunca visto entra na fila para ser processado.
    step current (queue, visited, transitions) symbol =
        let next = move current symbol
         in if Set.null next
                then (queue, visited, transitions)
                else
                    let transitions' = Map.insert (current, symbol) next transitions
                     in if Set.member next visited
                            then (queue, visited, transitions')
                            else (queue ++ [next], Set.insert next visited, transitions')

    -- Une os destinos de todos os estados do conjunto com o mesmo símbolo. É esta união
    -- que cria os novos estados: em {S}, o símbolo s leva a {A,H}.
    move dfaState symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (stateName, symbol) (nfaTransitions nfa)
            | stateName <- Set.toList dfaState
            ]


-- ============================================================================
-- 5. MINIMIZAÇÃO
--
-- A apostila (p. 25) minimiza em três passos: eliminar inacessíveis, eliminar
-- mortos e construir as classes de equivalência. O enunciado proíbe o terceiro,
-- então fazemos os dois primeiros.
--
-- Por isso, aqui os estados só são removidos, nunca fundidos. A apostila (p. 29)
-- explica o motivo: fundir faria "todos os estados finais se juntassem num só",
-- e o autômato não saberia mais qual token reconheceu. No exemplo, {G} (entao) e
-- {L} (senao) precisam continuar separados. Isso também descarta algoritmos que
-- chegam ao mesmo resultado sem calcular as classes, como o de Brzozowski.
--
-- As duas remoções podem rodar em qualquer ordem e nenhuma precisa ser repetida.
-- Uma aresta que sai de um estado alcançável chega a um estado alcançável, e um
-- estado que alcança um estado vivo também é vivo. Assim, remover um tipo de
-- estado nunca cria um estado do outro tipo.
-- ============================================================================

-- | Mantém só os estados alcançáveis a partir do estado inicial, seguindo as transições
-- para a frente.
--
-- A determinização já parte do estado inicial, então esta etapa não encontra nada na
-- saída dela. O passo continua explícito para que o requisito não dependa da forma
-- como o AFD foi gerado.
removeUnreachableStates :: Dfa -> Dfa
removeUnreachableStates dfa =
    restrictTo (reachableFrom (neighbours forward) [dfaStartState dfa]) dfa
  where
    -- Cada transição vira uma aresta (origem, destino). O símbolo não importa.
    forward = [(from, to) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]

-- | Mantém só os estados que conseguem chegar a um estado final. Para descobri-los, parte
-- dos estados finais e anda pelas transições ao contrário: quem for visitado está vivo,
-- e o resto está morto.
--
-- O estado inicial sempre fica: quando a linguagem é vazia ele está morto, mas todo AFD
-- precisa de um estado inicial.
removeDeadStates :: Dfa -> Dfa
removeDeadStates dfa =
    restrictTo (Set.insert (dfaStartState dfa) live) dfa
  where
    -- Mesmas arestas de forward, com origem e destino trocados.
    backward = [(to, from) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]
    live = reachableFrom (neighbours backward) (Set.toList (dfaFinalStates dfa))

-- | Monta uma função que diz para onde se pode ir a partir de um estado. A tabela só é
-- montada uma vez, e a função devolvida apenas consulta essa tabela.
neighbours :: [(DfaState, DfaState)] -> DfaState -> [DfaState]
neighbours edges =
    let table = Map.fromListWith (++) [(from, [to]) | (from, to) <- edges]
     in \state -> Map.findWithDefault [] state table

-- | Busca em grafo: devolve tudo o que dá para visitar a partir dos estados iniciais da
-- busca, incluindo eles próprios. As duas remoções acima usam esta mesma busca, mudando
-- só o ponto de partida e o sentido das arestas.
--
-- seen guarda os já visitados e pending os que faltam olhar. Um estado já visto é
-- ignorado, o que garante que a busca termina mesmo com ciclos.
reachableFrom :: (DfaState -> [DfaState]) -> [DfaState] -> Set.Set DfaState
reachableFrom next = go Set.empty
  where
    go seen [] = seen
    go seen (state : pending)
        | Set.member state seen = go seen pending
        | otherwise = go (Set.insert state seen) (next state ++ pending)

-- | Apaga todos os estados fora do conjunto keep, junto com as transições que saem deles
-- ou chegam neles. As células que apontavam para um estado removido ficam vazias e são
-- preenchidas depois pelo estado de erro.
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
-- Enunciado: "acrescentar um último estado final". Todas as células vazias apontam
-- para ele, e todas as transições que saem dele voltam para ele. Depois desta etapa
-- a tabela fica completa: toda célula tem um destino.
-- ============================================================================

-- | Acrescenta o estado de erro como último estado.
--
-- Ele é final porque o enunciado pede, e a apostila (p. 29) diz o mesmo: "será um estado
-- final, que reportará erro". Sendo final, ele não conta como estado morto, e o
-- autômato continua livre de estados mortos. Isso não significa que a sentença foi
-- aceita: em um reconhecedor, esse é o estado em que a leitura para e informa o erro,
-- assim como os outros estados finais informam o token reconhecido.
--
-- Um AFD sem células vazias não muda: nada levaria ao estado de erro, e ele seria um
-- estado inalcançável.
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
    -- Percorre a tabela inteira e guarda as células sem transição.
    unmappedCells =
        [ (state, symbol)
        | state <- dfaStates dfa
        , symbol <- alphabet
        , Map.notMember (state, symbol) (dfaTransitions dfa)
        ]
    -- A linha do próprio estado de erro: todas as células voltam para ele.
    errorLoops = [(errorState, symbol) | symbol <- alphabet]
    errorTransitions = Map.fromList [(cell, errorState) | cell <- unmappedCells ++ errorLoops]


-- ============================================================================
-- 7. IMPRESSÃO DAS TABELAS
--
-- As linhas são os estados e as colunas são os símbolos do alfabeto. Estados finais
-- levam um * na frente do nome.
-- ============================================================================

-- | Tabela do AFND: uma célula com vários destinos sai separada por vírgula, como A,H.
renderNfaTable :: Nfa -> String
renderNfaTable nfa = renderTable (nfaAlphabet nfa) (map row (nfaStates nfa))
  where
    row state = renderStateCell (Set.member state (nfaFinalStates nfa)) state : map (cell state) (nfaAlphabet nfa)
    cell state symbol =
        intercalate "," (Set.toList (Map.findWithDefault Set.empty (state, symbol) (nfaTransitions nfa)))

-- | Tabela do AFD: uma célula sem transição sai vazia.
renderDfaTable :: Dfa -> String
renderDfaTable dfa = renderTable (dfaAlphabet dfa) (map row (dfaStates dfa))
  where
    row state =
        renderStateCell (Set.member state (dfaFinalStates dfa)) (renderDfaState state)
            : map (cell state) (dfaAlphabet dfa)
    cell state symbol = maybe "" renderDfaState (Map.lookup (state, symbol) (dfaTransitions dfa))

-- | Dá nome a um estado do AFD a partir dos estados do AFND que ele representa, como
-- {A,H}. Assim, o mesmo estado tem o mesmo nome em todas as tabelas.
renderDfaState :: DfaState -> String
renderDfaState state
    | state == errorState = "ERR"
    | otherwise = "{" ++ intercalate "," (Set.toList state) ++ "}"

renderStateCell :: Bool -> String -> String
renderStateCell isFinal name = if isFinal then '*' : name else name

-- | Monta a tabela alinhada. A largura de cada coluna é a da maior célula dela. O
-- transpose troca linhas por colunas para facilitar esse cálculo.
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

-- | Completa o texto com espaços até a largura da coluna.
padRight :: Int -> String -> String
padRight width text = text ++ replicate (width - length text) ' '


-- ============================================================================
-- 8. PROGRAMA PRINCIPAL
--
-- Junta as etapas: cada uma recebe o resultado da anterior.
-- ============================================================================

-- | Confere os argumentos da linha de comando e inicia o processamento.
main :: IO ()
main = do
    -- O ε da entrada e o δ da saída não podem depender da configuração da máquina: sem
    -- isto, o programa quebra em ambientes com LANG=C.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs

    case args of
        [filePath] -> readAndProcessFile filePath
        _ -> die "Usage: runghc MinimalDfaGenerator.hs <input-file.txt>"

-- | Lê o arquivo e monta o AFND.
--
-- As duas etapas devolvem um Either. O operador >>= só chama buildNfa se parseInput deu
-- certo. Se qualquer uma falhar, o programa termina mostrando a mensagem de erro.
readAndProcessFile :: FilePath -> IO ()
readAndProcessFile filePath = do
    contents <- readInputFile filePath

    case parseInput contents >>= buildNfa of
        Left err -> die err
        Right nfa -> printPipeline nfa

-- | Lê o arquivo como UTF-8. A opção utf8_bom descarta a marca invisível (BOM) que alguns
-- editores do Windows colocam no início do arquivo.
readInputFile :: FilePath -> IO String
readInputFile filePath = do
    handle <- openFile filePath ReadMode
    hSetEncoding handle utf8_bom
    hGetContents handle

-- | Executa as etapas em ordem e imprime a tabela de cada uma.
printPipeline :: Nfa -> IO ()
printPipeline nfa = do
    let dfa = buildDfa nfa
        reachableDfa = removeUnreachableStates dfa
        minimizedDfa = removeDeadStates reachableDfa
        minimizedWithError = addErrorState minimizedDfa
        -- Cada estado do AFD é um conjunto de estados do AFND. Quem não aparece em nenhum
        -- desses conjuntos nunca foi alcançado a partir de S. O operador \\ é a diferença
        -- entre listas.
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

-- | Lista os estados que existiam em before e não existem mais em after.
renderRemovedStates :: Dfa -> Dfa -> String
renderRemovedStates before after =
    renderStateList (map renderDfaState (dfaStates before \\ dfaStates after))

-- | Junta os nomes com vírgula, ou escreve "none" se a lista estiver vazia.
renderStateList :: [String] -> String
renderStateList [] = "none"
renderStateList names = intercalate ", " names
