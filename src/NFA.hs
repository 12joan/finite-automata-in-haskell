module NFA (Transition (Epsilon, Read), NFA (NFA), NFA.initialState, NFA.stateIsFinal, NFA.transitionFunc, toDFA, runNFA, evalNFA, acceptsNFA) where

import DFA
import Data.List

data Transition a = Epsilon | Read a
  deriving Eq

data NFA a b = NFA {
  initialState :: a,
  stateIsFinal :: a -> Bool,
  transitionFunc :: a -> Transition b -> [a]
}

eClose :: Eq a => [a] -> (a -> Transition b -> [a]) -> [a]
eClose initialStates transitionFunc = reachable initialStates []
  where
    reachable fromStates visitedStates =
      let unvisitedStates = fromStates \\ visitedStates
       in case unvisitedStates of
            [] -> visitedStates
            _ -> reachable (adjacentToAny unvisitedStates) (visitedStates ++ unvisitedStates)
    adjacentToAny ss = nub [s | s1 <- ss, s <- adjacentTo s1]
    adjacentTo x = transitionFunc x Epsilon

toDFA :: Eq a => NFA a b -> DFA [a] b
toDFA nfa = DFA initialState' stateIsFinal' transitionFunc'
  where
    initialState' = eClose [NFA.initialState nfa] (NFA.transitionFunc nfa)
    stateIsFinal' = any (NFA.stateIsFinal nfa)
    transitionFunc' ss c = nub $ do
      s <- ss
      eClose (f s (Read c)) f
        where f = NFA.transitionFunc nfa

runNFA :: Eq a => NFA a b -> [b] -> [[a]]
runNFA nfa = runDFA (toDFA nfa)

evalNFA :: Eq a => NFA a b -> [b] -> [a]
evalNFA nfa = evalDFA (toDFA nfa)

acceptsNFA :: Eq a => NFA a b -> [b] -> Bool
acceptsNFA nfa = acceptsDFA (toDFA nfa)
