-- | Etapa de carga: transforma cada token e cada gramática em transições de um
-- único AFND. Como pede o enunciado, só o estado inicial S é compartilhado, e
-- todos os outros estados pertencem a um único token ou gramática.
--
-- O indeterminismo nasce aqui: quando duas sentenças começam com o mesmo
-- símbolo, a célula de S ganha dois destinos. No exemplo do enunciado, "se" e "senao"
-- fazem a célula S/s ter os destinos A e H.
module Automata.Nfa (buildNfa) where

import Automata.Input (Alt (..), Input (..), Production (..))
import Automata.Types (Nfa (..), StateName, Symbol, TransitionMap)
import Control.Monad (foldM)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | O AFND em construção. Haskell não altera valores: cada função abaixo recebe
-- um BuildState e devolve um novo, com a mudança aplicada. bsCounter é o número
-- usado para dar nome ao próximo estado.
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
-- O foldl aplica uma função a cada item, passando o resultado adiante. O foldM
-- faz o mesmo, mas cada passo pode falhar: o primeiro Left interrompe tudo. As
-- gramáticas precisam dele porque podem ter erros, e os tokens não.
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

-- | Transforma um número em nome de estado, como as colunas de uma planilha:
-- 0 = A, 25 = Z, 26 = AA, 27 = AB. O go gera as letras da última para a primeira,
-- por isso o resultado é invertido no final.
stateLabelFromIndex :: Int -> String
stateLabelFromIndex n = reverse (go n)
  where
    go x =
        let (q, r) = x `divMod` 26
            ch = toEnum (fromEnum 'A' + r) :: Char
         in ch : if q == 0 then [] else go (q - 1)

-- | Cria um estado novo e devolve seu nome junto com o BuildState atualizado.
-- O nome S é pulado porque pertence ao estado inicial. É por isso que, em
-- autômatos grandes, a sequência de nomes vai de R direto para T.
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

-- | Carrega um token: cria um caminho a partir de S, com um estado novo para
-- cada símbolo, e marca o último como final. O token "se" vira S -s-> A -e-> *B.
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

-- | Carrega uma gramática. Cada não-terminal ganha um estado novo, o que permite
-- que duas gramáticas usem o mesmo nome, como @<A>@, sem compartilhar estados.
-- No exemplo do enunciado, @<A>@ vira o estado M.
--
-- @<S>@ é o estado inicial compartilhado. Quando uma produção volta para @<S>@,
-- ela aponta para uma cópia privada dele. Sem isso, a leitura poderia seguir para
-- os tokens das outras sentenças: com o token ok e @<S> ::= a<S> | b@, a palavra
-- aok seria aceita, e ela não pertence a nenhuma das duas sentenças.
--
-- Cada alternativa vira transição assim:
--
--   A ::= a<B>  ->  transição de A para B com o símbolo a
--   A ::= a     ->  transição de A para um estado novo e final
--   A ::= ε     ->  A vira estado final
buildGrammar :: BuildState -> [Production] -> Either String BuildState
buildGrammar st0 productions = foldM applyProduction st1 productions
  where
    nonTerminals = nub (map pLhs productions)
    -- Alguma produção volta para <S>? Só nesse caso a cópia é necessária.
    startIsRecursive = any goesToStart (concatMap pAlts productions)
    privateStates = filter (\nt -> nt /= "S" || startIsRecursive) nonTerminals
    -- ntMap diz qual estado representa cada não-terminal desta gramática. Por
    -- ser criado aqui, ele não é visto pelas outras gramáticas.
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

    -- De quais estados saem as transições de uma produção. As de <S> saem do
    -- inicial e, se ela existir, também da cópia privada.
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
