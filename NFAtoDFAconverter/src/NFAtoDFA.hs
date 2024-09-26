module NFAtoDFA (nfaToDfa, DFA) where

import qualified Data.Set as Set
import Data.List (nub)
import NFAParser (NFA, State, Symbol, Transition)

type TransitionDFA = (Set.Set State, Symbol, Set.Set State)
type DFA = ([Set.Set State], [Symbol], Set.Set State, [Set.Set State], [TransitionDFA])

nfaToDfa :: NFA -> DFA
nfaToDfa nfa@(_, alphabet, startState, finalStates, _) =
    let startClosure = epsilonClosure nfa (Set.singleton startState) -- epsilon-closure for Start State.
        -- find all DFA states and transitions.
        (dfaTransitions, allStates) = processDfaStates nfa [startClosure] [] []
        allStatesNub = nub allStates -- Remove duplicate states
        -- DFA state is final if it contains any of the NFA's final states.
        finalStatesDfa = filter (\state -> any (`Set.member` state) finalStates) allStatesNub
    in (allStatesNub, filter (/= "0") alphabet, startClosure, finalStatesDfa, dfaTransitions)

-- set of states reachable from a given states via epsilon-transitions.
epsilonClosure :: NFA -> Set.Set State -> Set.Set State
epsilonClosure (_, _, _, _, transitions) states = bfsEpsilonClosure Set.empty states transitions

-- BFS to find all epsilon-transitions from a states.
-- q0,0,q1; q0,0,q2; q2,0,q4 -> (q0,q1,q2,q4)
bfsEpsilonClosure :: Set.Set State -> Set.Set State -> [Transition] -> Set.Set State
bfsEpsilonClosure visited queue transitions
    | Set.null queue = visited
    | otherwise =
        let current = Set.elemAt 0 queue -- take element from queue
            newStates = Set.fromList [t | (s, "0", t) <- transitions, s == current, t /= s] -- add "state_to"s
            newVisited = Set.insert current visited
            newQueue = Set.union (Set.delete current queue) (Set.difference newStates visited) -- add "state_to"s to queue
        in bfsEpsilonClosure newVisited newQueue transitions

-- all possible transitions
processDfaStates :: NFA -> [Set.Set State] -> [Set.Set State] -> [TransitionDFA] -> ([TransitionDFA], [Set.Set State])
processDfaStates _ [] visited dfaTransitions = (dfaTransitions, visited)
processDfaStates nfa@(_, alphabet, _, _, _) (current:queue) visited dfaTransitions
    | current `elem` visited = processDfaStates nfa queue visited dfaTransitions -- exmample: q0,_,q0 - so we don't stuck in a recursion
    | otherwise =
        let newTransitions = [(current, sym, epsilonClosure nfa reachable) -- epsilon-closure for every transition e.g q1,b,q_n + [t|q_n,0,t]
                                | sym <- alphabet, sym /= "0", 
                                  let reachable = findReachableStates nfa current sym,
                                  not (Set.null reachable)]
            newStates = [to | (_, _, to) <- newTransitions, to `notElem` visited]
            newVisited = current : visited
        in processDfaStates nfa (queue ++ newStates) newVisited (dfaTransitions ++ newTransitions)


-- return all endStates from epsilon-closure by symbol 
-- for q0_q1. where q0,a,q2 -> q0_q1,a,q2
findReachableStates :: NFA -> Set.Set State -> Symbol -> Set.Set State
findReachableStates (_, _, _, _, transitions) closure symbol =
    Set.fromList [t | s <- Set.toList closure, (s', sym, t) <- transitions, s == s', sym == symbol]
