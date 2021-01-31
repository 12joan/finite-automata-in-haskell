module DFA (DFA (DFA), DFA.initialState, DFA.stateIsFinal, DFA.transitionFunc, runDFA, evalDFA, acceptsDFA) where

data DFA a b = DFA {
  initialState :: a,
  stateIsFinal :: a -> Bool,
  transitionFunc :: a -> b -> a
}

runDFA :: DFA a b -> [b] -> [a]
runDFA dfa = scanl (transitionFunc dfa) (initialState dfa)

evalDFA :: DFA a b -> [b] -> a
evalDFA dfa = foldl (transitionFunc dfa) (initialState dfa)

acceptsDFA :: Eq a => DFA a b -> [b] -> Bool
acceptsDFA dfa string = stateIsFinal dfa (evalDFA dfa string)
