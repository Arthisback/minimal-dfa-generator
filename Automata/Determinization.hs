-- | Teorema da determinização (construção de subconjuntos): cada estado do AFD é
-- o conjunto de estados em que o AFND pode estar depois de ler o mesmo prefixo.
--
-- Exemplo do enunciado: lendo s a partir de S, o AFND pode ir para A ou H. Logo,
-- o AFD ganha o estado {A,H}, com uma única transição S -s-> {A,H}.
module Automata.Determinization (buildDfa) where

import Automata.Types (Dfa (..), Nfa (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Explora os conjuntos em largura, a partir do estado inicial. Como só segue
-- transições que existem, nunca gera um estado inalcançável. Quando uma célula
-- não tem destino, ela fica vazia na tabela.
--
-- Um estado do AFD é final se contém pelo menos um estado final do AFND. Por
-- exemplo, {C,M} é final porque M é final.
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

    -- Preenche uma célula: a do estado atual com um símbolo. Destino vazio deixa
    -- a célula vazia. Um destino nunca visto entra na fila para ser processado.
    step current (queue, visited, transitions) symbol =
        let next = move current symbol
         in if Set.null next
                then (queue, visited, transitions)
                else
                    let transitions' = Map.insert (current, symbol) next transitions
                     in if Set.member next visited
                            then (queue, visited, transitions')
                            else (queue ++ [next], Set.insert next visited, transitions')

    -- Une os destinos de todos os estados do conjunto com o mesmo símbolo. É esta
    -- união que cria os novos estados: em {S}, o símbolo s leva a {A,H}.
    move dfaState symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (stateName, symbol) (nfaTransitions nfa)
            | stateName <- Set.toList dfaState
            ]
