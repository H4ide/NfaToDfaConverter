module Main where

import Test.HUnit
import NFAParser (readNFAFromFile)
import NFAtoDFA (nfaToDfa)
import DFA (dfaToString)
import System.IO (readFile)
import System.FilePath (takeBaseName)

testNfaToDfa :: String -> Test
testNfaToDfa fileName = TestCase $ do
    result <- readNFAFromFile fileName
    case result of
        Right nfa -> do
            let dfa = dfaToString . nfaToDfa $ nfa
            let keyFileName = "test/keys/" ++ takeBaseName fileName ++ "_key.txt"
            expectedDFAOutput <- readFile keyFileName
            assertEqual ("Test " ++ fileName) expectedDFAOutput dfa
        Left err -> assertFailure $ "Failed to load NFA: " ++ err

testNfaInput :: String -> String -> Test
testNfaInput fileName expectedError = TestCase $ do
    result <- readNFAFromFile fileName
    case result of
        Left err -> assertEqual ("Test " ++ fileName ++ " (error case)") expectedError err
        Right _  -> assertFailure $ "Expected fail, but got success " ++ fileName

main :: IO ()
main = do
    _ <- runTestTT $ TestList 
        [
        testNfaToDfa "test/nfa_empty.txt",
        testNfaToDfa "test/nfa_epsilon.txt", 
        testNfaToDfa "test/nfa_multiple_finals.txt",
        testNfaToDfa "test/nfa_simple.txt",
        testNfaToDfa "test/nfa.txt",
        testNfaToDfa "test/nfa2.txt",
        testNfaToDfa "test/nfa3.txt",
        testNfaToDfa "test/nfa4.txt",
        testNfaToDfa "test/nfa5.txt",
        testNfaInput "test/bad_states.txt" "Error: (States) 'q' is invalid. State should start with 'q' followed by digits.",
        testNfaInput "test/bad_alphabet.txt" "Error: (Alphabet) Symbol 'c0' is invalid. It should only contain lowercase letters or just '0'.",
        testNfaInput "test/bad_start.txt" "Error: (States) Initial state not found in state list: 'q0'",
        testNfaInput "test/bad_finish.txt" "Error: (States) (Some) final states are not in the state list: [\"q4\",\"q3\"]"
        ]
    return ()