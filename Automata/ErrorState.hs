-- | Estado de erro: entra depois da minimização e completa a tabela do AFD.
module Automata.ErrorState (addErrorState) where

import Automata.Types (Dfa (..), errorState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Acrescenta o estado de erro como último estado, aponta todas as células
-- vazias para ele e faz com que ele só transite para si mesmo.
--
-- O enunciado pede "um último estado final". Sendo final, ele também não conta
-- como estado morto, e o autômato continua livre de estados mortos. Isso não
-- significa que a sentença foi aceita: em um reconhecedor, esse é o estado em que
-- a leitura para e informa o erro, assim como os outros estados finais informam
-- o token reconhecido.
--
-- Um AFD sem células vazias não muda: nada levaria ao estado de erro, e ele
-- seria um estado inalcançável.
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
