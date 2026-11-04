module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import Data.List (intercalate, isPrefixOf, partition, sort, transpose)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

    case buildNfa tokens grs of
        Left err -> die err
        Right nfa -> do
            putStrLn "NFA:"
            putStrLn (renderNfaTable nfa)
            let dfa = buildDfa nfa
                minimizedDfa = minimizeDfa dfa
                minimizedWithError = addErrorState minimizedDfa
            putStrLn "DFA:"
            putStrLn (renderDfaTable dfa)
            putStrLn "Minimized DFA:"
            putStrLn (renderDfaTable minimizedDfa)
            putStrLn "Minimized DFA + Error state:"
            putStrLn (renderDfaTable minimizedWithError)

type StateName = String
type Symbol = Char
type TransitionMap = Map.Map (StateName, Symbol) (Set.Set StateName)
type DfaState = Set.Set StateName
type DfaTransitionMap = Map.Map (DfaState, Symbol) DfaState

data Nfa = Nfa
    { nfaStates :: [StateName]
    , nfaAlphabet :: [Symbol]
    , nfaTransitions :: TransitionMap
    , nfaFinalStates :: Set.Set StateName
    }

data Dfa = Dfa
    { dfaStates :: [DfaState]
    , dfaStartState :: DfaState
    , dfaAlphabet :: [Symbol]
    , dfaTransitions :: DfaTransitionMap
    , dfaFinalStates :: Set.Set DfaState
    }

data Alt
    = Epsilon
    | TermOnly Symbol
    | TermToNonTerminal Symbol String

data Production = Production
    { pLhs :: String
    , pAlts :: [Alt]
    }

data BuildState = BuildState
    { bsStates :: [StateName]
    , bsUsed :: Set.Set StateName
    , bsTransitions :: TransitionMap
    , bsFinalStates :: Set.Set StateName
    , bsCounter :: Int
    }

initialBuildState :: BuildState
initialBuildState = BuildState
    { bsStates = ["S"]
    , bsUsed = Set.singleton "S"
    , bsTransitions = Map.empty
    , bsFinalStates = Set.empty
    , bsCounter = 0
    }

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy sep xs =
    case break (== sep) xs of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitBy sep rest

stateLabelFromIndex :: Int -> String
stateLabelFromIndex n = reverse (go n)
  where
    go x =
        let (q, r) = x `divMod` 26
            ch = toEnum (fromEnum 'A' + r) :: Char
         in ch : if q == 0 then [] else go (q - 1)

allocateState :: BuildState -> (StateName, BuildState)
allocateState st =
    let candidate = stateLabelFromIndex (bsCounter st)
     in if Set.member candidate (bsUsed st)
            then allocateState st {bsCounter = bsCounter st + 1}
            else
                ( candidate
                , st
                    { bsStates = bsStates st ++ [candidate]
                    , bsUsed = Set.insert candidate (bsUsed st)
                    , bsCounter = bsCounter st + 1
                    }
                )

addTransition :: StateName -> Symbol -> StateName -> BuildState -> BuildState
addTransition from symbol to st =
    let key = (from, symbol)
        nextDests = Set.insert to (Map.findWithDefault Set.empty key (bsTransitions st))
     in st {bsTransitions = Map.insert key nextDests (bsTransitions st)}

markFinal :: StateName -> BuildState -> BuildState
markFinal stateName st = st {bsFinalStates = Set.insert stateName (bsFinalStates st)}

buildTokenPath :: String -> BuildState -> BuildState
buildTokenPath token st0 =
    let (endState, stN) =
            foldl
                (\(current, st) ch ->
                    let (next, st1) = allocateState st
                        st2 = addTransition current ch next st1
                     in (next, st2)
                )
                ("S", st0)
                token
     in markFinal endState stN

parseProduction :: String -> Either String Production
parseProduction line =
    let cleaned = trim line
        (lhsPart, rhsPartRaw) = breakSubstring "::=" cleaned
     in if null rhsPartRaw
            then Left ("Invalid grammar line (missing ::=): " ++ line)
            else do
                lhs <- parseNonTerminal (trim lhsPart)
                let rhsPart = trim (drop 3 rhsPartRaw)
                alts <- mapM parseAlt (map trim (splitBy '|' rhsPart))
                Right Production {pLhs = lhs, pAlts = alts}
  where
    breakSubstring needle haystack = search [] haystack
      where
        search _ [] = (haystack, [])
        search acc remaining@(x : xs)
            | needle `isPrefixOf` remaining = (reverse acc, remaining)
            | otherwise = search (x : acc) xs

parseNonTerminal :: String -> Either String String
parseNonTerminal text =
    case text of
        '<' : xs ->
            case reverse xs of
                '>' : revName ->
                    let name = reverse revName
                     in if null name
                            then Left ("Invalid non-terminal: " ++ text)
                            else Right name
                _ -> Left ("Invalid non-terminal: " ++ text)
        _ -> Left ("Invalid non-terminal: " ++ text)

parseAlt :: String -> Either String Alt
parseAlt altText
    | altText == "ε" = Right Epsilon
    | null altText = Left "Empty alternative in grammar"
    | otherwise =
        case altText of
            [] -> Left "Empty alternative in grammar"
            (symbol : restChars) ->
                let rest = trim restChars
                 in if null rest
                        then Right (TermOnly symbol)
                        else do
                            nt <- parseNonTerminal rest
                            Right (TermToNonTerminal symbol nt)

buildGrammar :: [Production] -> BuildState -> Either String BuildState
buildGrammar productions st0 = do
    let nonTerminals = Set.fromList (map pLhs productions)
        (ntMap, st1) = foldl mapNonTerminal (Map.empty, st0) (Set.toList nonTerminals)
        mapNonTerminal (m, st) nt
            | nt == "S" = (Map.insert nt "S" m, st)
            | otherwise =
                let (newState, st1') = allocateState st
                 in (Map.insert nt newState m, st1')

        lookupNtState nt =
            case Map.lookup nt ntMap of
                Just s -> Right s
                Nothing -> Left ("Unknown non-terminal: <" ++ nt ++ ">")

        applyProduction acc production = do
            st <- acc
            lhsState <- lookupNtState (pLhs production)
            foldl (applyAlt lhsState) (Right st) (pAlts production)

        applyAlt fromState acc alt = do
            st <- acc
            case alt of
                Epsilon -> Right (markFinal fromState st)
                TermOnly sym ->
                    let (finalState, st1') = allocateState st
                        st2 = addTransition fromState sym finalState st1'
                     in Right (markFinal finalState st2)
                TermToNonTerminal sym nt -> do
                    toState <- lookupNtState nt
                    Right (addTransition fromState sym toState st)

    foldl applyProduction (Right st1) productions

buildNfa :: [String] -> [String] -> Either String Nfa
buildNfa tokens grammarLines = do
    productions <- mapM parseProduction grammarLines
    let stAfterTokens = foldl (flip buildTokenPath) initialBuildState tokens
    stFinal <- buildGrammar productions stAfterTokens
    let alphabet = sort . Set.toList $ Set.fromList [symbol | (_, symbol) <- Map.keys (bsTransitions stFinal)]
    Right
        Nfa
            { nfaStates = bsStates stFinal
            , nfaAlphabet = alphabet
            , nfaTransitions = bsTransitions stFinal
            , nfaFinalStates = bsFinalStates stFinal
            }

renderNfaTable :: Nfa -> String
renderNfaTable nfa =
    let states = nfaStates nfa
        alphabet = nfaAlphabet nfa
        transitions = nfaTransitions nfa
        finals = nfaFinalStates nfa
        headerCells = "δ" : map pure alphabet
        rowCells = map (renderRowCells alphabet transitions finals) states
        widths = map (maximum . map length) (transpose (headerCells : rowCells))
        header = renderPaddedRow widths headerCells
        rows = map (renderPaddedRow widths) rowCells
        divider = replicate (length header) '-'
     in unlines (header : divider : rows)

renderRowCells :: [Symbol] -> TransitionMap -> Set.Set StateName -> StateName -> [String]
renderRowCells alphabet transitions finals stateName =
    let stateCell = if Set.member stateName finals then "*" ++ stateName else stateName
        cells = map (renderCell stateName transitions) alphabet
     in stateCell : cells

renderPaddedRow :: [Int] -> [String] -> String
renderPaddedRow widths cells = intercalate " | " (zipWith padRight widths cells)

padRight :: Int -> String -> String
padRight width text = text ++ replicate (width - length text) ' '

renderCell :: StateName -> TransitionMap -> Symbol -> String
renderCell fromState transitions symbol =
    let dests = Set.toList (Map.findWithDefault Set.empty (fromState, symbol) transitions)
     in intercalate "," dests

buildDfa :: Nfa -> Dfa
buildDfa nfa =
    let alphabet = nfaAlphabet nfa
        startState = Set.singleton "S"
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
    discover [] _ ordered transitions = (ordered, transitions)
    discover (current : queue) visited ordered transitions =
        let (queue', visited', transitions') =
                foldl
                    (step current)
                    (queue, visited, transitions)
                    (nfaAlphabet nfa)
         in discover queue' visited' (ordered ++ [current]) transitions'

    step current (queue, visited, transitions) symbol =
        let next = move current symbol
         in if Set.null next
                then (queue, visited, transitions)
                else
                    let transitions' = Map.insert (current, symbol) next transitions
                     in if Set.member next visited
                            then (queue, visited, transitions')
                            else (queue ++ [next], Set.insert next visited, transitions')

    move dfaState symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (stateName, symbol) (nfaTransitions nfa)
            | stateName <- Set.toList dfaState
            ]

renderDfaTable :: Dfa -> String
renderDfaTable dfa =
    let states = dfaStates dfa
        errState = Set.singleton "ERR"
        nonErrStates = filter (/= errState) states
        aliasMap = Map.fromList (zip nonErrStates (map (\i -> "M" ++ show i) [0 ..]))
        labelOf stateName
            | stateName == errState = "ERR"
            | otherwise = Map.findWithDefault (renderDfaStateLabel stateName) stateName aliasMap
        alphabet = dfaAlphabet dfa
        transitions = dfaTransitions dfa
        finals = dfaFinalStates dfa
        headerCells = "δ" : map pure alphabet
        rowCells = map (renderDfaRowCells labelOf alphabet transitions finals) states
        widths = map (maximum . map length) (transpose (headerCells : rowCells))
        header = renderPaddedRow widths headerCells
        rows = map (renderPaddedRow widths) rowCells
        divider = replicate (length header) '-'
        legendLines = map (\stateName -> labelOf stateName ++ " = " ++ renderDfaStateLabel stateName) states
     in unlines (header : divider : rows ++ ["", "Legend:"] ++ legendLines)

renderDfaRowCells :: (DfaState -> String) -> [Symbol] -> DfaTransitionMap -> Set.Set DfaState -> DfaState -> [String]
renderDfaRowCells labelOf alphabet transitions finals stateName =
    let stateCell =
            if Set.member stateName finals
                then "*" ++ labelOf stateName
                else labelOf stateName
        cells = map (renderDfaCell labelOf stateName transitions) alphabet
     in stateCell : cells

renderDfaCell :: (DfaState -> String) -> DfaState -> DfaTransitionMap -> Symbol -> String
renderDfaCell labelOf fromState transitions symbol =
    case Map.lookup (fromState, symbol) transitions of
        Just toState -> labelOf toState
        Nothing -> ""

renderDfaStateLabel :: DfaState -> String
renderDfaStateLabel dfaState = "{" ++ intercalate "," (Set.toList dfaState) ++ "}"

type EncState = Int
type EncDfaTransitionMap = Map.Map (EncState, Symbol) EncState
type EncNfaTransitionMap = Map.Map (EncState, Symbol) (Set.Set EncState)

data EncDfa = EncDfa
    { encStates :: [EncState]
    , encStart :: EncState
    , encAlphabet :: [Symbol]
    , encTransitions :: EncDfaTransitionMap
    , encLabels :: Map.Map EncState DfaState
    , encFinals :: Set.Set EncState
    }

data EncNfa = EncNfa
    { encNfaStates :: [EncState]
    , encNfaStarts :: Set.Set EncState
    , encNfaAlphabet :: [Symbol]
    , encNfaTransitions :: EncNfaTransitionMap
    , encNfaLabels :: Map.Map EncState DfaState
    , encNfaFinals :: Set.Set EncState
    }

minimizeDfa :: Dfa -> Dfa
minimizeDfa dfa =
    let encoded = encodeDfa dfa
        rev1 = reverseDfa encoded
        det1 = determinizeEncNfa rev1
        rev2 = reverseDfa det1
        det2 = determinizeEncNfa rev2
     in decodeDfa det2

encodeDfa :: Dfa -> EncDfa
encodeDfa dfa =
    let states = dfaStates dfa
        stateToId = Map.fromList (zip states [0 ..])
        lookupId state =
            case Map.lookup state stateToId of
                Just sid -> sid
                Nothing -> error ("Internal error: unknown DFA state " ++ renderDfaStateLabel state)
        encTrans =
            Map.fromList
                [ ((lookupId fromState, sym), lookupId toState)
                | ((fromState, sym), toState) <- Map.toList (dfaTransitions dfa)
                ]
        encFinalSet = Set.fromList (map lookupId (Set.toList (dfaFinalStates dfa)))
        encStartState = lookupId (dfaStartState dfa)
     in EncDfa
            { encStates = [0 .. length states - 1]
            , encStart = encStartState
            , encAlphabet = dfaAlphabet dfa
            , encTransitions = encTrans
            , encLabels = Map.fromList [(lookupId st, st) | st <- states]
            , encFinals = encFinalSet
            }

decodeDfa :: EncDfa -> Dfa
decodeDfa encDfa =
    let orderedIds = encStates encDfa
        idLabel sid = Map.findWithDefault (Set.singleton ("Q" ++ show sid)) sid (encLabels encDfa)
        states = map idLabel orderedIds
        stateById = Map.fromList (zip orderedIds states)
        toState sid =
            case Map.lookup sid stateById of
                Just st -> st
                Nothing -> error ("Internal error: unknown encoded state " ++ show sid)
        transitions =
            Map.fromList
                [ ((toState fromId, sym), toState toId)
                | ((fromId, sym), toId) <- Map.toList (encTransitions encDfa)
                ]
        finals = Set.fromList [toState sid | sid <- Set.toList (encFinals encDfa)]
     in Dfa
            { dfaStates = states
            , dfaStartState = toState (encStart encDfa)
            , dfaAlphabet = encAlphabet encDfa
            , dfaTransitions = transitions
            , dfaFinalStates = finals
            }

reverseDfa :: EncDfa -> EncNfa
reverseDfa dfa =
    let reversedTransitions =
            foldl
                (\acc ((fromState, sym), toState) ->
                    let key = (toState, sym)
                        current = Map.findWithDefault Set.empty key acc
                     in Map.insert key (Set.insert fromState current) acc
                )
                Map.empty
                (Map.toList (encTransitions dfa))
     in EncNfa
            { encNfaStates = encStates dfa
            , encNfaStarts = encFinals dfa
            , encNfaAlphabet = encAlphabet dfa
            , encNfaTransitions = reversedTransitions
            , encNfaLabels = encLabels dfa
            , encNfaFinals = Set.singleton (encStart dfa)
            }

determinizeEncNfa :: EncNfa -> EncDfa
determinizeEncNfa nfa =
    let startSubset = encNfaStarts nfa
        (orderedSubsets, subsetTransitions) =
            discoverSubsets [startSubset] (Set.singleton startSubset) [] Map.empty
        subsetToId = Map.fromList (zip orderedSubsets [0 ..])
        lookupId subset =
            case Map.lookup subset subsetToId of
                Just sid -> sid
                Nothing -> error "Internal error: subset not indexed"
        encTrans =
            Map.fromList
                [ ((lookupId fromSubset, sym), lookupId toSubset)
                | ((fromSubset, sym), toSubset) <- Map.toList subsetTransitions
                ]
        subsetLabel subset =
            Set.fromList
                [ renderDfaStateLabel (Map.findWithDefault Set.empty sid (encNfaLabels nfa))
                | sid <- Set.toList subset
                ]
        encLabelMap =
            Map.fromList
                [ (lookupId subset, subsetLabel subset)
                | subset <- orderedSubsets
                ]
        encFinalSet =
            Set.fromList
                [ lookupId subset
                | subset <- orderedSubsets
                , not (Set.null (Set.intersection subset (encNfaFinals nfa)))
                ]
     in EncDfa
            { encStates = [0 .. length orderedSubsets - 1]
            , encStart = 0
            , encAlphabet = encNfaAlphabet nfa
            , encTransitions = encTrans
            , encLabels = encLabelMap
            , encFinals = encFinalSet
            }
  where
    discoverSubsets [] _ ordered transitions = (ordered, transitions)
    discoverSubsets (current : queue) visited ordered transitions =
        let (queue', visited', transitions') =
                foldl
                    (step current)
                    (queue, visited, transitions)
                    (encNfaAlphabet nfa)
         in discoverSubsets queue' visited' (ordered ++ [current]) transitions'

    step current (queue, visited, transitions) symbol =
        let nextSubset = moveSubset current symbol
         in if Set.null nextSubset
                then (queue, visited, transitions)
                else
                    let transitions' = Map.insert (current, symbol) nextSubset transitions
                     in if Set.member nextSubset visited
                            then (queue, visited, transitions')
                            else (queue ++ [nextSubset], Set.insert nextSubset visited, transitions')

    moveSubset subset symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (nfaState, symbol) (encNfaTransitions nfa)
            | nfaState <- Set.toList subset
            ]

addErrorState :: Dfa -> Dfa
addErrorState dfa =
    let errorState = Set.singleton "ERR"
        baseStates = dfaStates dfa
        statesWithError =
            if errorState `elem` baseStates
                then baseStates
                else baseStates ++ [errorState]
        fillStateTransitions acc stateName =
            foldl (fillSymbolTransition stateName) acc (dfaAlphabet dfa)
        fillSymbolTransition fromState acc symbol =
            let key = (fromState, symbol)
             in case Map.lookup key acc of
                    Just _ -> acc
                    Nothing -> Map.insert key errorState acc
        seededTransitions = foldl fillStateTransitions (dfaTransitions dfa) statesWithError
     in dfa
            { dfaStates = statesWithError
            , dfaTransitions = seededTransitions
            }
