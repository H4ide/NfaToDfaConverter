# Program Documentation

This Haskell program converting an epsilon-NFA (epsilon-Nondeterministic Finite Automaton) into a  DFA* (Deterministic Finite Automaton). 

*The generated DFA may not always be fully minimized.

The program consists of four modules:

1. **Main.hs: The starting point of the application. It handles user input and manages the conversion process.**
   
2. **NFAParser.hs: Responsible for reading and parsing the NFA from an input file.**
   
3. **NFAtoDFA.hs: Contains the logic to convert the NFA into a DFA.**
   
4. **DFA.hs: Manages the output of the DFA, displaying it on the screen and writing it to a file.**

## Input File Rules for the NFA Text File

**File Format**: The input file consists of multiple lines of text, each of which represents different components of the NFA: states, alphabet, the start state, final states, and transitions.

Elements in the string are separated by a comma.

## 1. States

- The first non-empty line defines the **states** in the NFA.
- States are comma-separated and must follow the format `q` followed by a number. For example:
  
```q0,q1,q2,q10```

## 2. Alphabet

- The second non-empty line defines the **alphabet** of the NFA, which consists of lower-case Latin letters and the special symbol `0` (epsilon transition).

  ```0,a,b```

## 3. Start State

- The third non-empty line defines the **start state** of the NFA.
- The start state must be one of the states listed in the first line (states definition).

  ```q0```

## 4. Final States

- The fourth non-empty line defines the **final states** of the NFA.

  ```q2```
  
  - There can be multiple final states

## 5. Transitions

- Subsequent lines define the **transitions** of the NFA, with **one transition per line**. Each transition should follow the format:
  ```state_from, alphabet_symbol, state_to```

  ```q0, 0, q1```

- **`state_from`** and **`state_to`** must be valid states listed in the first non-empty line.
- **`alphabet_symbol`** must be a valid symbol from the alphabet defined in the second non-empty line.

  ## Error Handling:

1. **Invalid State Names**: If a state does not begin with `q` followed by digits (e.g., `q0`), an error will occur.
2. **Invalid Alphabet**: If the alphabet contains uppercase letters, numbers (except `0` for epsilon), or symbols, the file will be considered invalid.
3. **Invalid Transitions**: Transitions must have valid states and symbols from the alphabet. If a state or symbol is used in a transition but not defined in the earlier sections, it will trigger an error.
4. **Missing Start or Final States**: If no start state is defined or the start state is invalid, the program will fail. Similarly, if any final state is not listed in the states section, an error will occur.


## Example Input and Output

### Input NFA nfa_epilion.txt

```
q0,q1,q2  -- States
0,a,b  -- Alphabet
q0  -- Start State
q2  -- Final State
q0,0,q1 -- 1st transition
q1,a,q1 -- 2nd transition
q1,b,q2 -- ...
q2,a,q2 -- ...
q2,b,q2  -- 5th transition
```
 ### **Input**
```
./NFAtoDFA.exe "nfa_epsilion.txt"
```

### **Output**
```
DFA States:
q2, q1, q0_q1

DFA Alphabet:
["a","b"]

DFA Start State:
q0_q1

DFA Final States:
q2

DFA Transitions:
q0_q1 -- a --> q1
q0_q1 -- b --> q2
q1 -- a --> q1
q1 -- b --> q2
q2 -- a --> q2
q2 -- b --> q2

```
which is also saved to the current folder with the name of the "**currentFile**_**timestampGMT+0**"


# 1. Main.hs

The Main.hs is the entry point for the program. It takes the following steps:

 - Reads command-line arguments to get the input file name containing the NFA.
 - Uses NFAParser to parse the (epsilon)-NFA from the file.
 - Converts the parsed NFA to a DFA using the nfaToDfa function from the NFAtoDFA module.
 -  Prints the DFA to the console and writes it to an output file with a timestamp.

# 2. NFAParser.hs

The **```NFAParser.hs```** is responsible for reading and parsing a text file that defines an NFA. It validates the structure of the states, alphabet, transitions, start state, and final states. The main function, **``readNFAFromFile``**, takes a file path, reads the file, and converts it into an NFA or returns an error if any issues are found.

Errors in the file structure or missing elements are handled with a "handler" and reported to the user.

The **``parseNFA``** function processes each line of the input file, cleaning them with the helper functions **``removeExtraCommasAndSpaces``** and **``splitOn``**. It validates each line to ensure:

- The states are correct using the ``checkState`` function.
- The symbols in the alphabet are valid using the ``checkSymbol`` function.
- The transitions are properly structured and valid using the ``parseTransition`` function.

# 3. NFAtoDFA.hs

### DFA Data Structure:

**```type TransitionDFA = (Set.Set State, Symbol, Set.Set State)```**

**```type DFA = ([Set.Set State], [Symbol], Set.Set State, [Set.Set State], [TransitionDFA])```**

The DFA is represented by:
- A list of DFA states ([Set.Set State]), where each DFA state is a set of NFA states.
- A list of symbols [Symbol] (the alphabet, excluding epsilon 0).
- The DFA start state, which is the epsilon-closure of the NFA's start state.
- A list of final states for the DFA.
- A list of transitions ([TransitionDFA]), where each transition is a tuple (Data.Set State, Symbol, Data.Set State).

The `NFAtoDFA` module is responsible for converting epsilon-NFA into a DFA. This process involves handling epsilon transitions and generating the appropriate states and transitions for the DFA.

1. **`nfaToDfa`**:
   - The main function that makes the conversion from an NFA to a DFA.
   - The function takes an NFA as input and returns a DFA, ensuring that epsilon transitions are handled correctly.
     - **Epsilon Closure**: The epsilon-closure is calculated for the start state of the NFA. The epsilon-closure is the set of all states reachable from the start state via epsilon transition.
     - **Processing DFA States**: Recursively, the function explores all possible DFA states and their transitions, making sure epsilon transitions are accounted for at every step.

2. **`epsilonClosure`**:
   - This function computes the epsilon-closure for a set of NFA states. It returns all states that are reachable via one or more epsilon transitions from the given states.
   
3. **`bfsEpsilonClosure`**:
   - This is a helper function that uses a BFS  to find all states reachable via epsilon transitions from a given set of states.
   - It takes care of avoiding infinite loops by keeping track of visited states.

4. **`processDfaStates`**:
   - This function processes all possible DFA states and their transitions. It ensures that the epsilon-closure is calculated for every newly discovered state.
   - The function iterates through the alphabet (excluding epsilon) and calculates the reachable states for each symbol, applying epsilon-closure to them to ensure all possible transitions are captured.
   - If a state has already been visited, it is skipped to avoid unnecessary recursion or processing.

5. **`findReachableStates`**:
   - This function finds all states reachable from a given set of NFA states via a specific symbol.

# 4. DFA.hs

This module formats the DFA states, alphabet, transitions in a readable format.

1. **`writeDfaToFile`**:
 - Writes the DFA to a file in the directory that input File had. Adds the current time to the file name.

2. **`dfaToFormattedString`**
 - one function, which is used by two functions - the function to output to the console `printDfa` and to String `dfaToString` for saving to a text file. Uses `replaceSpaces` to make States more readable.

_______________________________________________________________________

## TestNfaToDfa.hs

- This file tests the functionality of modules and input parsing. The tests are written using the HUnit and ensure that the conversion process works as required and that error cases are handled as required.

 - **`testNfaToDfa`**:
   - This function tests the conversion of an NFA to a DFA by comparing the actual DFA output with a predefined expected DFA stored in a *name*_key file.

   - If the outputs match, the test passes; otherwise, it fails with a message showing the difference.

 - **`testNfaInput`**:
   -  tests invalid NFA inputs.
   - It takes an NFA file and an expected error message, and verifies that the parsing function (readNFAFromFile) produces the expected error.
   - If the error message is different from what was expected or the file succeeds in parsing, the test fails.
