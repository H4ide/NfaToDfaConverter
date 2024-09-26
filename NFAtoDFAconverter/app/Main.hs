module Main where

import System.Environment (getArgs) -- get command-line arguments
import NFAParser (readNFAFromFile)
import NFAtoDFA (nfaToDfa)
import DFA (printDfa, writeDfaToFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            result <- readNFAFromFile fileName                   -- if exactly one argument (the filename) was provided
            case result of
                Right nfa -> do -- if we get success in pasting NFA from file
                    let dfa = nfaToDfa nfa
                    printDfa dfa
                    writeDfaToFile fileName dfa
                Left err -> putStrLn err
        _ -> putStrLn "Error: You did not provide the filename." -- Error otherwise

