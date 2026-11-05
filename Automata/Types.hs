-- | Tipos usados por todas as etapas do pipeline.
module Automata.Types
    ( StateName
    , Symbol
    , TransitionMap
    , DfaState
    , DfaTransitionMap
    , Nfa (..)
    , Dfa (..)
    , errorState
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type StateName = String
type Symbol = Char

-- | Tabela de transições do AFND: um dicionário em que a chave é a célula
-- (estado, símbolo) e o valor é o conjunto de destinos. É um conjunto porque no
-- AFND uma célula pode ter vários destinos. No exemplo do enunciado, a célula
-- ("S", 's') guarda {"A", "H"}.
type TransitionMap = Map.Map (StateName, Symbol) (Set.Set StateName)

-- | Um estado do AFD é o conjunto de estados do AFND que ele representa, como
-- manda o teorema da determinização. Exemplo: o estado {A,H}.
type DfaState = Set.Set StateName

-- | Tabela de transições do AFD: cada célula tem no máximo um destino.
type DfaTransitionMap = Map.Map (DfaState, Symbol) DfaState

-- | Autômato finito não determinístico (AFND). Os campos correspondem à
-- definição formal: estados, estado inicial, alfabeto, função de transição e
-- estados finais.
data Nfa = Nfa
    { nfaStates :: [StateName]
    , nfaStartState :: StateName
    , nfaAlphabet :: [Symbol]
    , nfaTransitions :: TransitionMap
    , nfaFinalStates :: Set.Set StateName
    }

-- | Autômato finito determinístico (AFD). Um par que não existe em
-- 'dfaTransitions' é uma célula vazia da tabela.
data Dfa = Dfa
    { dfaStates :: [DfaState]
    , dfaStartState :: DfaState
    , dfaAlphabet :: [Symbol]
    , dfaTransitions :: DfaTransitionMap
    , dfaFinalStates :: Set.Set DfaState
    }

-- | O estado de erro é o conjunto vazio: nenhum estado do AFND está ativo nele.
-- A determinização nunca gera esse conjunto, então ele não se confunde com
-- nenhum outro estado. Na impressão, ele aparece como ERR.
errorState :: DfaState
errorState = Set.empty
