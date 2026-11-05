-- | Suíte de testes. Rode na raiz do repositório com: runghc test/Spec.hs
--
-- Além dos casos fixos, ela gera entradas pseudoaleatórias e compara cada autômato
-- do pipeline com as próprias gramáticas de origem, testando todas as palavras
-- curtas. As funções de aceitação abaixo foram escritas de forma independente das
-- etapas que elas verificam.
module Main (main) where

import Automata.Determinization (buildDfa)
import Automata.ErrorState (addErrorState)
import Automata.Input (parseInput)
import Automata.Minimization (removeDeadStates, removeUnreachableStates)
import Automata.Nfa (buildNfa)
import Automata.Types
import Control.Monad (foldM, forM, replicateM, unless)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)

type Test = (String, Bool)

main :: IO ()
main = do
    let tests = concat [statementTests, passTests, fixtureTests, inputFormatTests, invalidInputTests, randomTests]
        failures = [name | (name, passed) <- tests, not passed]
    mapM_ (putStrLn . ("FAILED: " ++)) failures
    unless (null failures) exitFailure
    putStrLn ("All " ++ show (length tests) ++ " tests passed.")

-- Execução do pipeline --------------------------------------------------------

data Pipeline = Pipeline
    { pNfa :: Nfa
    , pDfa :: Dfa
    , pMinimized :: Dfa
    , pComplete :: Dfa
    }

runPipeline :: String -> Pipeline
runPipeline text =
    case parseInput text >>= buildNfa of
        Left err -> error ("Unexpected invalid input: " ++ err)
        Right nfa ->
            let dfa = buildDfa nfa
                minimized = removeDeadStates (removeUnreachableStates dfa)
             in Pipeline nfa dfa minimized (addErrorState minimized)

nfaAccepts :: Nfa -> String -> Bool
nfaAccepts nfa word = not (Set.null (Set.intersection (nfaFinalStates nfa) reached))
  where
    reached = foldl step (Set.singleton (nfaStartState nfa)) word
    step states symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (state, symbol) (nfaTransitions nfa)
            | state <- Set.toList states
            ]

dfaRun :: Dfa -> String -> Maybe DfaState
dfaRun dfa = foldM (\state symbol -> Map.lookup (state, symbol) (dfaTransitions dfa)) (dfaStartState dfa)

dfaAccepts :: Dfa -> String -> Bool
dfaAccepts dfa word = maybe False (`Set.member` dfaFinalStates dfa) (dfaRun dfa word)

-- | O estado de erro é final, mas chegar nele significa que a palavra foi rejeitada.
recognizes :: Dfa -> String -> Bool
recognizes dfa word = dfaAccepts dfa word && dfaRun dfa word /= Just errorState

wordsUpTo :: Int -> [Symbol] -> [String]
wordsUpTo n alphabet = concat (take (n + 1) (iterate extend [""]))
  where
    extend ws = [c : w | c <- alphabet, w <- ws]

-- | Todos os autômatos do pipeline decidem estas palavras como o oráculo decide.
matchesOracle :: (String -> Bool) -> Pipeline -> [String] -> Bool
matchesOracle oracle p = all check
  where
    check word =
        all
            (== oracle word)
            [ nfaAccepts (pNfa p) word
            , dfaAccepts (pDfa p) word
            , dfaAccepts (pMinimized p) word
            , recognizes (pComplete p) word
            ]

-- Propriedades da estrutura ----------------------------------------------------

grow :: (Set.Set DfaState -> [DfaState]) -> Set.Set DfaState -> Set.Set DfaState
grow next seen =
    let seen' = Set.union seen (Set.fromList (next seen))
     in if seen' == seen then seen else grow next seen'

noUnreachableStates :: Dfa -> Bool
noUnreachableStates dfa = Set.fromList (dfaStates dfa) == grow forward (Set.singleton (dfaStartState dfa))
  where
    forward seen = [to | ((from, _), to) <- Map.toList (dfaTransitions dfa), Set.member from seen]

-- | O estado inicial é a única exceção: mesmo a linguagem vazia precisa dele.
noDeadStates :: Dfa -> Bool
noDeadStates dfa = Set.fromList (dfaStates dfa) `Set.isSubsetOf` Set.insert (dfaStartState dfa) live
  where
    live = grow backward (dfaFinalStates dfa)
    backward seen = [from | ((from, _), to) <- Map.toList (dfaTransitions dfa), Set.member to seen]

-- | A minimização só pode apagar estados e transições, nunca fundir.
nothingMerged :: Pipeline -> Bool
nothingMerged p =
    Set.fromList (dfaStates (pMinimized p)) `Set.isSubsetOf` Set.fromList (dfaStates (pDfa p))
        && dfaTransitions (pMinimized p) `Map.isSubmapOf` dfaTransitions (pDfa p)

isTotal :: Dfa -> Bool
isTotal dfa =
    and [Map.member (state, symbol) (dfaTransitions dfa) | state <- dfaStates dfa, symbol <- dfaAlphabet dfa]

errorStateIsCorrect :: Pipeline -> Bool
errorStateIsCorrect p
    | isTotal (pMinimized p) = dfaStates complete == dfaStates (pMinimized p)
    | otherwise =
        last (dfaStates complete) == errorState
            && Set.member errorState (dfaFinalStates complete)
            && all loops (dfaAlphabet complete)
            && dfaTransitions (pMinimized p) `Map.isSubmapOf` dfaTransitions complete
  where
    complete = pComplete p
    loops symbol = Map.lookup (errorState, symbol) (dfaTransitions complete) == Just errorState

structureHolds :: Pipeline -> Bool
structureHolds p =
    and
        [ noUnreachableStates (pMinimized p)
        , noDeadStates (pMinimized p)
        , nothingMerged p
        , isTotal (pComplete p)
        , errorStateIsCorrect p
        , noUnreachableStates (pComplete p)
        , noDeadStates (pComplete p)
        ]

-- Exemplo publicado no enunciado do projeto --------------------------------------

statementExample :: Pipeline
statementExample =
    runPipeline
        ( unlines
            [ "se"
            , "entao"
            , "senao"
            , ""
            , "<S> ::= a<A> | e<A> | i<A> | o<A> | u<A> "
            , "<A> ::= a<A> | e<A> | i<A> | o<A> | u<A> | ε"
            ]
        )

-- | As transições da tabela do AFND impressa no enunciado. Cada destino é uma
-- única letra, então uma string lista os estados de destino de uma célula.
publishedTransitions :: TransitionMap
publishedTransitions =
    Map.fromList
        [ ((state, symbol), Set.fromList (map pure destinations))
        | (state, cells) <-
            [ ("S", [('s', "AH"), ('e', "CM"), ('a', "M"), ('o', "M"), ('i', "M"), ('u', "M")])
            , ("A", [('e', "B")])
            , ("C", [('n', "D")])
            , ("D", [('t', "E")])
            , ("E", [('a', "F")])
            , ("F", [('o', "G")])
            , ("H", [('e', "I")])
            , ("I", [('n', "J")])
            , ("J", [('a', "K")])
            , ("K", [('o', "L")])
            , ("M", [(symbol, "M") | symbol <- "eaoiu"])
            ]
        , (symbol, destinations) <- cells
        ]

statementTests :: [Test]
statementTests =
    [ ("statement: NFA states", nfaStates nfa == "S" : map pure "ABCDEFGHIJKLM")
    , ("statement: NFA columns", nfaAlphabet nfa == "sentaoiu")
    , ("statement: NFA final states", nfaFinalStates nfa == Set.fromList ["B", "G", "L", "M"])
    , ("statement: NFA transitions", nfaTransitions nfa == publishedTransitions)
    , ("statement: DFA state count", length (dfaStates (pDfa statementExample)) == 12)
    , ("statement: each token keeps its own final state", all isDistinct [("se", "entao"), ("se", "senao"), ("entao", "senao")])
    , ("statement: tokens stop at the expected states", map finalOf ["entao", "senao"] == map (Just . Set.singleton) ["G", "L"])
    , ("statement: variables are recognized", finalOf "aeiou" == Just (Set.singleton "M"))
    , ("statement: language", matchesOracle oracle statementExample (wordsUpTo 5 "sentaoiu"))
    , ("statement: structure", structureHolds statementExample)
    ]
  where
    nfa = pNfa statementExample
    finalOf = dfaRun (pComplete statementExample)
    isDistinct (first, second) = finalOf first /= finalOf second
    oracle word = word `elem` ["se", "entao", "senao"] || (not (null word) && all (`elem` "aeiou") word)

-- Cada remoção da minimização, isoladamente --------------------------------------

named :: String -> DfaState
named = Set.singleton

-- | q0 -a-> q1 (final) é a parte útil. q2 é inalcançável e q3 está morto.
handBuiltDfa :: Dfa
handBuiltDfa =
    Dfa
        { dfaStates = map named ["q0", "q1", "q2", "q3"]
        , dfaStartState = named "q0"
        , dfaAlphabet = "ab"
        , dfaTransitions =
            Map.fromList
                [ ((named "q0", 'a'), named "q1")
                , ((named "q0", 'b'), named "q3")
                , ((named "q2", 'a'), named "q1")
                , ((named "q3", 'b'), named "q3")
                ]
        , dfaFinalStates = Set.singleton (named "q1")
        }

passTests :: [Test]
passTests =
    [ ("passes: unreachable state removed", dfaStates withoutUnreachable == map named ["q0", "q1", "q3"])
    , ("passes: dead state removed", dfaStates withoutDead == map named ["q0", "q1", "q2"])
    , ("passes: no transition is left into a removed state", Map.size (dfaTransitions withoutDead) == 2)
    , ("passes: both passes together", dfaStates both == map named ["q0", "q1"])
    ]
  where
    withoutUnreachable = removeUnreachableStates handBuiltDfa
    withoutDead = removeDeadStates handBuiltDfa
    both = removeDeadStates withoutUnreachable

-- Entradas pequenas com linguagem conhecida --------------------------------------

fixture :: String -> String -> (String -> Bool) -> Int -> [Test]
fixture name text oracle maxLength =
    [ (name ++ ": language", matchesOracle oracle p (wordsUpTo maxLength (dfaAlphabet (pDfa p))))
    , (name ++ ": structure", structureHolds p)
    ]
  where
    p = runPipeline text

isAll :: Symbol -> String -> Bool
isAll symbol = all (== symbol)

fixtureTests :: [Test]
fixtureTests =
    concat
        [ fixture "tokens only" "se\nsenao\n" (`elem` ["se", "senao"]) 6
        , fixture "grammar only" "<S> ::= a<S> | ε\n" (isAll 'a') 5
        , fixture "grammars do not share <A>" "<S> ::= a<A>\n<A> ::= b\n\n<S> ::= c<A>\n<A> ::= d\n" (`elem` ["ab", "cd"]) 4
        , fixture "recursion on <S> stays out of the tokens" "ok\n<S> ::= a<S> | b\n" oneOrMoreAs 5
        , fixture "dead state" "<S> ::= a<A> | b<B>\n<A> ::= a<A>\n<B> ::= b\n" (== "bb") 4
        , fixture "unreachable grammar symbol" "<S> ::= a\n<Z> ::= z<Z> | z\n" (== "a") 4
        , fixture "dead cycle" "<S> ::= a<A> | b\n<A> ::= a<B>\n<B> ::= a<A>\n" (== "b") 4
        , fixture "empty language" "<S> ::= a<A>\n<A> ::= a<A>\n" (const False) 3
        , fixture "empty word" "<S> ::= ε\n" null 0
        , fixture "total DFA gets no error state" "<S> ::= a<A>| b<A> | ε\n<A> ::= a<A> | b<A> | ε\n" (const True) 3
        , fixture "same token twice" "se\nse\n" (== "se") 3
        , fixture "productions split by tokens" "x\n<S> ::= a<A>\ny\n<A> ::= b\n" (`elem` ["x", "y", "ab"]) 3
        , fixture "non-terminal defined before <S>" "<A> ::= b\n<S> ::= a<A>\n" (== "ab") 3
        ]
  where
    oneOrMoreAs word = word == "ok" || (not (null word) && last word == 'b' && isAll 'a' (init word))

-- Formato da entrada -------------------------------------------------------------

inputFormatTests :: [Test]
inputFormatTests =
    concat
        [ fixture "operators that start with <" "<\n<=\n<>\n<=>\n" (`elem` ["<", "<=", "<>", "<=>"]) 3
        , fixture "other operators" ":=\n::=\n|\n" (`elem` [":=", "::=", "|"]) 3
        , fixture "windows line endings" "se\r\n<S> ::= a<A>\r\n<A> ::= b\r\n" (`elem` ["se", "ab"]) 3
        , fixture "surrounding spaces" "  se  \n\t<S> ::= a  \n   \n" (`elem` ["se", "a"]) 3
        , fixture "BNF without spaces" "<S>::=a<A>|b\n<A>::=c|ε\n" (`elem` ["a", "b", "ac"]) 3
        , fixture "long non-terminal names" "<S> ::= a<Digit_1>\n<Digit_1> ::= b\n" (== "ab") 3
        ]
        ++ [ ("state names stay unique past Z", distinctNames (nfaStates (pNfa (runPipeline (unlines longTokens)))))
           , ("long tokens are recognized", matchesOracle (`elem` longTokens) (runPipeline (unlines longTokens)) (map init longTokens ++ longTokens))
           , ("windows line endings stay out of the alphabet", nfaAlphabet (pNfa (runPipeline "se\r\n")) == "se")
           , ("a symbol used only by unreachable states stays in the alphabet", dfaAlphabet (pComplete (runPipeline "<S> ::= a\n<Z> ::= z\n")) == "az")
           ]

longTokens :: [String]
longTokens = [replicate 30 symbol | symbol <- "ab"]

distinctNames :: [StateName] -> Bool
distinctNames names = Set.size (Set.fromList names) == length names

invalidInputTests :: [Test]
invalidInputTests =
    [ ("rejects " ++ name, either (const True) (const False) (parseInput text >>= buildNfa))
    | (name, text) <-
        [ ("a production without ::=", "<S> = a<A>\n")
        , ("an undefined non-terminal", "<S> ::= a<B>\n")
        , ("an empty alternative", "<S> ::= a | \n")
        , ("two terminals in one alternative", "<S> ::= ab<A>\n<A> ::= a\n")
        , ("a production without terminal", "<S> ::= <A>\n<A> ::= a\n")
        , ("an unterminated non-terminal", "<S> ::= a<A\n<A> ::= a\n")
        ]
    ]

-- Entradas pseudoaleatórias -------------------------------------------------------

-- | Os geradores consomem uma sequência fixa de números, então as falhas podem
-- ser reproduzidas.
newtype Gen a = Gen {runGen :: [Int] -> (a, [Int])}

instance Functor Gen where
    fmap f (Gen g) = Gen (\ns -> let (a, rest) = g ns in (f a, rest))

instance Applicative Gen where
    pure a = Gen (\ns -> (a, ns))
    Gen f <*> Gen g = Gen (\ns -> let (h, r1) = f ns; (a, r2) = g r1 in (h a, r2))

instance Monad Gen where
    Gen g >>= f = Gen (\ns -> let (a, rest) = g ns in runGen (f a) rest)

numbers :: Int -> [Int]
numbers = map (`div` 65536) . drop 1 . iterate (\x -> (x * 1103515245 + 12345) `mod` 2147483648)

-- | Um número do intervalo, com as duas pontas incluídas.
choose :: Int -> Int -> Gen Int
choose low high =
    Gen
        ( \ns -> case ns of
            n : rest -> (low + n `mod` (high - low + 1), rest)
            [] -> error "The number stream is infinite"
        )

element :: [a] -> Gen a
element xs = (xs !!) <$> choose 0 (length xs - 1)

data GenAlt = GenEpsilon | GenTerminal Symbol | GenStep Symbol String

type GenGrammar = [(String, [GenAlt])]

data GenInput = GenInput [String] [GenGrammar]

genAlphabet :: [Symbol]
genAlphabet = "abc"

genAlt :: [String] -> Gen GenAlt
genAlt names = do
    kind <- choose 0 5
    symbol <- element genAlphabet
    next <- element names
    pure $ case kind of
        0 -> GenEpsilon
        1 -> GenTerminal symbol
        _ -> GenStep symbol next

genGrammar :: Gen GenGrammar
genGrammar = do
    extraNames <- choose 0 3
    let names = "S" : take extraNames ["A", "B", "C"]
    forM names $ \name -> do
        altCount <- choose 1 3
        alts <- replicateM altCount (genAlt names)
        pure (name, alts)

genInput :: Gen GenInput
genInput = do
    tokenCount <- choose 0 3
    tokens <- replicateM tokenCount (choose 1 4 >>= \size -> replicateM size (element genAlphabet))
    grammarCount <- choose 0 2
    GenInput tokens <$> replicateM grammarCount genGrammar

renderInput :: GenInput -> String
renderInput (GenInput tokens grammars) = unlines (tokens ++ concatMap (map renderProduction) grammars)
  where
    renderProduction (name, alts) = "<" ++ name ++ "> ::= " ++ intercalate " | " (map renderAlt alts)
    renderAlt GenEpsilon = "ε"
    renderAlt (GenTerminal symbol) = [symbol]
    renderAlt (GenStep symbol next) = symbol : "<" ++ next ++ ">"

-- | Aplica as produções diretamente, sem usar nenhum autômato. É o oráculo.
derives :: GenGrammar -> String -> String -> Bool
derives grammar name word = any (matches word) (Map.findWithDefault [] name productions)
  where
    productions = Map.fromList grammar
    matches [] GenEpsilon = True
    matches [symbol] (GenTerminal terminal) = symbol == terminal
    matches (symbol : rest) (GenStep terminal next) = symbol == terminal && derives grammar next rest
    matches _ _ = False

inLanguage :: GenInput -> String -> Bool
inLanguage (GenInput tokens grammars) word =
    word `elem` tokens || any (\grammar -> derives grammar "S" word) grammars

randomInputs :: [GenInput]
randomInputs = fst (runGen (replicateM 400 genInput) (numbers 20261104))

randomTests :: [Test]
randomTests =
    [ ("random input " ++ show index ++ ": " ++ show (renderInput input), check input)
    | (index, input) <- zip [1 :: Int ..] randomInputs
    ]
  where
    check input =
        let p = runPipeline (renderInput input)
         in matchesOracle (inLanguage input) p (wordsUpTo 5 (nfaAlphabet (pNfa p))) && structureHolds p
