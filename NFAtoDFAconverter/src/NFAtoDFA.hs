module NFAtoDFA (nfaToDfa, DFA) where

import qualified Data.Set as Set
import Data.List (nub)
import NFAParser (NFA, State, Symbol, Transition)

type TransitionDFA = (Set.Set State, Symbol, Set.Set State)
type DFA = ([Set.Set State], [Symbol], Set.Set State, [Set.Set State], [TransitionDFA])

nfaToDfa :: NFA -> DFA
nfaToDfa nfa@(_, alphabet, startState, finalStates, _) =
    let startClosure = epsilonClosure nfa startState
        (dfaTransitions, allStates) = processDfaStates nfa [startClosure] [] []
        allStatesNub = nub allStates
        finalStatesDfa = filter (\state -> any (`Set.member` state) finalStates) allStatesNub
    in (allStatesNub, filter (/= "0") alphabet, startClosure, finalStatesDfa, dfaTransitions)

-- Function for calculating epsilon-closure for one state
epsilonClosure :: NFA -> State -> Set.Set State
epsilonClosure (_, _, _, _, transitions) startState = bfsEpsilonClosure Set.empty (Set.singleton startState) transitions

bfsEpsilonClosure :: Set.Set State -> Set.Set State -> [Transition] -> Set.Set State
bfsEpsilonClosure visited queue transitions
    | Set.null queue = visited
    | otherwise =
        let current = Set.elemAt 0 queue
            newStates = Set.fromList [t | (s, "0", t) <- transitions, s == current, t /= s]
            newVisited = Set.insert current visited
            newQueue = Set.union (Set.delete current queue) (Set.difference newStates visited)
        in bfsEpsilonClosure newVisited newQueue transitions

-- all possible transitions
processDfaStates :: NFA -> [Set.Set State] -> [Set.Set State] -> [TransitionDFA] -> ([TransitionDFA], [Set.Set State])
processDfaStates _ [] visited dfaTransitions = (dfaTransitions, visited)
processDfaStates nfa@(_, alphabet, _, _, _) (current:queue) visited dfaTransitions
    | current `elem` visited = processDfaStates nfa queue visited dfaTransitions
    | otherwise =
        let newTransitions = [(current, sym, findReachableStates nfa current sym)
                              | sym <- alphabet, sym /= "0",
                                let reachable = findReachableStates nfa current sym,
                                not (Set.null reachable)]
            newStates = [to | (_, _, to) <- newTransitions, to `notElem` visited]
            newVisited = current : visited
        in processDfaStates nfa (queue ++ newStates) newVisited (dfaTransitions ++ newTransitions)

-- return all endStates from epsilon-closure by symbol 
findReachableStates :: NFA -> Set.Set State -> Symbol -> Set.Set State
findReachableStates (_, _, _, _, transitions) closure symbol =
    Set.fromList [t | s <- Set.toList closure, (s', sym, t) <- transitions, s == s', sym == symbol]