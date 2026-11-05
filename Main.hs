-- | Ponto de entrada do programa. Lê um arquivo com tokens e gramáticas
-- regulares e imprime a tabela de cada etapa do pipeline:
--
--   arquivo -> AFND -> AFD -> AFD minimizado -> AFD minimizado + estado de erro
--
-- Glossário: no código, NFA significa AFND e DFA significa AFD (siglas em inglês).
module Main (main) where

import Automata.Determinization (buildDfa)
import Automata.ErrorState (addErrorState)
import Automata.Input (parseInput)
import Automata.Minimization (removeDeadStates, removeUnreachableStates)
import Automata.Nfa (buildNfa)
import Automata.Render (renderDfaState, renderDfaTable, renderNfaTable)
import Automata.Types (Dfa (..), Nfa (..))
import Data.List (intercalate, (\\))
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, stderr, stdout, utf8, utf8_bom)

-- | Confere os argumentos da linha de comando e inicia o processamento.
main :: IO ()
main = do
    -- O ε da entrada e o δ da saída não podem depender da configuração da
    -- máquina: sem isto, o programa quebra em ambientes com LANG=C.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs

    case args of
        [filePath] -> readAndProcessFile filePath
        _ -> die "Usage: runghc Main.hs <input-file.txt>"

-- | Lê o arquivo e monta o AFND.
--
-- As duas etapas devolvem um Either: Left traz uma mensagem de erro e Right traz
-- o resultado. O operador >>= só chama buildNfa se parseInput deu certo. Se
-- qualquer uma falhar, o programa termina mostrando a mensagem de erro.
readAndProcessFile :: FilePath -> IO ()
readAndProcessFile filePath = do
    contents <- readInputFile filePath

    case parseInput contents >>= buildNfa of
        Left err -> die err
        Right nfa -> printPipeline nfa

-- | Lê o arquivo como UTF-8. A opção utf8_bom descarta a marca invisível (BOM)
-- que alguns editores do Windows colocam no início do arquivo.
readInputFile :: FilePath -> IO String
readInputFile filePath = do
    handle <- openFile filePath ReadMode
    hSetEncoding handle utf8_bom
    hGetContents handle

-- | Executa as etapas em ordem, cada uma recebendo o resultado da anterior, e
-- imprime a tabela de cada uma. A última tabela é o resultado final do trabalho.
printPipeline :: Nfa -> IO ()
printPipeline nfa = do
    let dfa = buildDfa nfa
        reachableDfa = removeUnreachableStates dfa
        minimizedDfa = removeDeadStates reachableDfa
        minimizedWithError = addErrorState minimizedDfa
        -- Cada estado do AFD é um conjunto de estados do AFND. Quem não aparece
        -- em nenhum desses conjuntos nunca foi alcançado a partir de S.
        -- O operador \\ é a diferença entre listas.
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
