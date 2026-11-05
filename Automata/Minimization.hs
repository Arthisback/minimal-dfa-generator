-- | Minimização como o enunciado define: o AFD deve terminar livre de estados
-- inalcançáveis e mortos, sem aplicar classes de equivalência entre estados.
--
-- Por isso, aqui os estados só são removidos, nunca fundidos. Fundir estados
-- equivalentes juntaria os estados finais de tokens diferentes, e o autômato não
-- saberia mais qual token reconheceu. Isso também descarta algoritmos que chegam
-- ao mesmo resultado sem calcular as classes, como o de Brzozowski.
--
-- As duas remoções podem rodar em qualquer ordem e nenhuma precisa ser repetida.
-- Uma aresta que sai de um estado alcançável chega a um estado alcançável, e um
-- estado que alcança um estado vivo também é vivo. Assim, remover um tipo de
-- estado nunca cria um estado do outro tipo.
module Automata.Minimization
    ( removeUnreachableStates
    , removeDeadStates
    ) where

import Automata.Types (Dfa (..), DfaState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Mantém só os estados alcançáveis a partir do estado inicial, seguindo as
-- transições para a frente.
--
-- A determinização já parte do estado inicial, então esta etapa não encontra
-- nada na saída dela. O passo continua explícito para que o requisito não
-- dependa da forma como o AFD foi gerado.
removeUnreachableStates :: Dfa -> Dfa
removeUnreachableStates dfa =
    restrictTo (reachableFrom (neighbours forward) [dfaStartState dfa]) dfa
  where
    -- Cada transição vira uma aresta (origem, destino). O símbolo não importa.
    forward = [(from, to) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]

-- | Mantém só os estados que conseguem chegar a um estado final. Para descobri-los,
-- parte dos estados finais e anda pelas transições ao contrário: quem for visitado
-- está vivo, e o resto está morto.
--
-- O estado inicial sempre fica: quando a linguagem é vazia ele está morto, mas
-- todo AFD precisa de um estado inicial.
removeDeadStates :: Dfa -> Dfa
removeDeadStates dfa =
    restrictTo (Set.insert (dfaStartState dfa) live) dfa
  where
    -- Mesmas arestas de forward, com origem e destino trocados.
    backward = [(to, from) | ((from, _), to) <- Map.toList (dfaTransitions dfa)]
    live = reachableFrom (neighbours backward) (Set.toList (dfaFinalStates dfa))

-- | Monta uma função que diz para onde se pode ir a partir de um estado. A tabela
-- só é montada uma vez, e a função devolvida apenas consulta essa tabela.
neighbours :: [(DfaState, DfaState)] -> DfaState -> [DfaState]
neighbours edges =
    let table = Map.fromListWith (++) [(from, [to]) | (from, to) <- edges]
     in \state -> Map.findWithDefault [] state table

-- | Busca em grafo: devolve tudo o que dá para visitar a partir dos estados
-- iniciais da busca, incluindo eles próprios. As duas remoções acima usam esta
-- mesma busca, mudando só o ponto de partida e o sentido das arestas.
--
-- seen guarda os já visitados e pending os que faltam olhar. Um estado já visto
-- é ignorado, o que garante que a busca termina mesmo com ciclos.
reachableFrom :: (DfaState -> [DfaState]) -> [DfaState] -> Set.Set DfaState
reachableFrom next = go Set.empty
  where
    go seen [] = seen
    go seen (state : pending)
        | Set.member state seen = go seen pending
        | otherwise = go (Set.insert state seen) (next state ++ pending)

-- | Apaga todos os estados fora do conjunto keep, junto com as transições que
-- saem deles ou chegam neles. As células que apontavam para um estado removido
-- ficam vazias e são preenchidas depois pelo estado de erro.
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
