-- | Imprime os autômatos como tabelas de transição. Estados finais levam um *.
module Automata.Render
    ( renderNfaTable
    , renderDfaTable
    , renderDfaState
    ) where

import Automata.Types (Dfa (..), DfaState, Nfa (..), Symbol, errorState)
import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Tabela do AFND: uma linha por estado. Uma célula com vários destinos sai
-- separada por vírgula, como A,H.
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

-- | Dá nome a um estado do AFD a partir dos estados do AFND que ele representa,
-- como {A,H}. Assim, o mesmo estado tem o mesmo nome em todas as tabelas.
renderDfaState :: DfaState -> String
renderDfaState state
    | state == errorState = "ERR"
    | otherwise = "{" ++ intercalate "," (Set.toList state) ++ "}"

renderStateCell :: Bool -> String -> String
renderStateCell isFinal name = if isFinal then '*' : name else name

-- | Monta a tabela alinhada. A largura de cada coluna é a da maior célula dela.
-- O transpose troca linhas por colunas para facilitar esse cálculo.
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
