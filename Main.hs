module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import Data.List (partition, isPrefixOf)

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filePath] -> readAndProcessFile filePath
        _ -> die "Usage: runghc Main.hs <input-file.txt>"

readAndProcessFile :: FilePath -> IO ()
readAndProcessFile filePath = do
    contents <- readFile filePath

    let inputs = filter (not . null) (lines contents)
        (grs, tokens) = partition
            (isPrefixOf "<" )
            inputs

    putStrLn "Tokens:"
    mapM_ putStrLn tokens

    putStrLn "\n GRs:"
    mapM_ putStrLn grs
