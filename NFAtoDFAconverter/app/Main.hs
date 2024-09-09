module Main where

import System.Environment (getArgs)
import NFAParser (readNFAFromFile)
import NFAtoDFA (nfaToDfa)
import DFA (printDfa, writeDfaToFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            result <- readNFAFromFile fileName
            case result of
                Right nfa -> do
                    let dfa = nfaToDfa nfa
                    printDfa dfa
                    writeDfaToFile fileName dfa
                Left err -> putStrLn err
        _ -> putStrLn "Error: You did not provide the filename."
