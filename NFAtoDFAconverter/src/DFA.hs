module DFA (printDfa, writeDfaToFile, dfaToString, DFA) where

import qualified Data.Set as Set
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.FilePath (takeDirectory, takeBaseName, (</>), (<.>))
import Data.List (intercalate)
import NFAtoDFA (DFA)



writeDfaToFile :: FilePath -> DFA -> IO ()
writeDfaToFile inputFileName dfa = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
    let outputDir = takeDirectory inputFileName
    let baseName = takeBaseName inputFileName
    let outputFileName = outputDir </> (baseName ++ "_" ++ formattedTime <.> "txt")
    
    writeFile outputFileName (dfaToString dfa)
    putStrLn $ "Output written to: " ++ outputFileName

dfaToFormattedString :: (String -> String) -> DFA -> String
dfaToFormattedString output (states, alphabet, startState, finalStates, transitions) =
    output "DFA States:\n" ++
    output (intercalate ", " (map (replaceSpaces . unwords . Set.toList) states)) ++
    output "\n\nDFA Alphabet:\n" ++
    output (show alphabet) ++
    output "\n\nDFA Start State:\n" ++
    output (replaceSpaces . unwords . Set.toList $ startState) ++
    output "\n\nDFA Final States:\n" ++
    output (intercalate ", " (map (replaceSpaces . unwords . Set.toList) finalStates)) ++
    output "\n\nDFA Transitions:\n" ++
    unlines (map printTransition transitions)
  where
    printTransition (from, sym, to) =
        (replaceSpaces . unwords . Set.toList $ from) ++ " -- " ++ sym ++ " --> " ++ (replaceSpaces . unwords . Set.toList $ to)

printDfa :: DFA -> IO ()
printDfa dfa = putStrLn (dfaToFormattedString id dfa)

dfaToString :: DFA -> String
dfaToString = dfaToFormattedString id

replaceSpaces :: String -> String
replaceSpaces = map (\c -> if c == ' ' then '_' else c)