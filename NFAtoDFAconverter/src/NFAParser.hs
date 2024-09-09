module NFAParser (readNFAFromFile, NFA, State, Symbol, Transition) where

import Data.Char (isLower, isDigit)
import Data.List (nub)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)

type State = String
type Symbol = String
type Transition = (State, Symbol, State)
type NFA = ([State], [Symbol], State, [State], [Transition])

removeExtraCommasAndSpaces :: String -> [String]
removeExtraCommasAndSpaces = filter (not . null) . splitOn ',' . filter (/= ' ')

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (before, remainder) = span (/= delim) str
    in before : case remainder of
                   [] -> []
                   _ : rest -> splitOn delim rest

-- state should start with q followed by numbers
checkState :: String -> Either String State
checkState [] = Left "Error: Empty state"
checkState s@(x:xs)
    | x == 'q' && not (null xs) && all isDigit xs = Right s
    | otherwise = Left $ "Error: (States) '" ++ s ++ "' is invalid. State should start with 'q' followed by digits."



-- symbol should be '0' or string of letter(s) in lowercase
checkSymbol :: String -> Either String Symbol
checkSymbol s
    | s == "0" = Right s  -- 0 means epsilon 
    | all isLower s = Right s
    | otherwise = Left $ "Error: (Alphabet) Symbol '" ++ s ++ "' is invalid. It should only contain lowercase letters or just '0'."

parseNFA :: String -> Either String NFA
parseNFA content = do
    let linesOfFile = filter (not . null) (lines content) -- remove all empty lines from file
    let cleanedLines = map removeExtraCommasAndSpaces linesOfFile
    states   <- nub <$> mapM checkState (head cleanedLines)
    alphabet <- nub <$> mapM checkSymbol (cleanedLines !! 1)
    let startStateCandidates = nub (cleanedLines !! 2)
    startState <- case startStateCandidates of
        [singleState] -> do
            state <- checkState singleState
            if state `elem` states
                then Right state
                else Left $ "Error: (States) Initial state not found in state list: '" ++ state ++ "'"
        _ -> Left "Error: (States) Invalid initial state."
    let finalStates = nub (cleanedLines !! 3)
    if all (`elem` states) finalStates
        then return ()
        else Left $ "Error: (States) (Some) final states are not in the state list: " ++ show finalStates
    
    transitions <- mapM (parseTransition states alphabet) (drop 4 cleanedLines)
    return (states, alphabet, startState, finalStates, nub transitions)

parseTransition :: [State] -> [Symbol] -> [String] -> Either String Transition
parseTransition states alphabet transition@[startState, transtinionSymbol, endState] = do
-- I don't need to check if a state or symbol in a transition is correct, 
-- I just need to make sure it is in the states or in the alphabet
    if startState `elem` states && endState `elem` states
        then if transtinionSymbol `elem` alphabet
                then Right (startState, transtinionSymbol, endState)
                else Left $ "Error: Symbol '" ++ transtinionSymbol ++ "' not found in alphabet."
        else Left $ "Error: Invalid state(s) in transition: " ++ show transition ++ " not found in States."
parseTransition _ _ transition = Left $ "Error: An incorrect number of arguments has been passed: " ++ show transition

readNFAFromFile :: FilePath -> IO (Either String NFA)
readNFAFromFile fileString = catch (do
    filePath <- readFile fileString
    return (parseNFA filePath)
    ) handler

    
handler :: IOException -> IO (Either String NFA)
handler e
    | isDoesNotExistError e = return $ Left "Error: The file does not exist."
    | otherwise = return $ Left $ "Error: An unknown error occurred: " ++ show e