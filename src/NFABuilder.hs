module NFABuilder (StateId, NFABuilder, buildNFA, acceptSymbolNFA, emptyWordNFA, emptySetNFA, concatNFAs, disjunctNFAs, kleeneNFA) where

import NFA
import Control.Monad.State

type StateId = Int
type NFABuilder a = State StateId (NFA StateId a)

buildNFA :: NFABuilder a -> NFA StateId a
buildNFA = (flip evalState) 0

nextStateId :: State StateId StateId
nextStateId = do
  x <- get
  put (x+1)
  return x

acceptSymbolNFA :: Eq a => a -> NFABuilder a
acceptSymbolNFA x = do
  q0 <- nextStateId
  q1 <- nextStateId
  return $ NFA q0 (== q1) (f q0 q1)
    where
      f q0 q1 s (Read c) = if s == q0 && c == x then [q1] else []
      f _  _  _  _       = []

emptyWordNFA :: NFABuilder a
emptyWordNFA = do
  q0 <- nextStateId
  return $ NFA q0 (== q0) (\_ _ -> [])

emptySetNFA :: NFABuilder a
emptySetNFA = do
  q0 <- nextStateId
  return $ NFA q0 (\_ -> False) (\_ _ -> [])

concatNFAs :: Eq a => (NFA StateId a) -> (NFA StateId a) -> NFABuilder a
concatNFAs nfa1 nfa2 = return $ NFA initialState' stateIsFinal' transitionFunc'
  where
    initialState' = NFA.initialState nfa1
    stateIsFinal' = NFA.stateIsFinal nfa2
    transitionFunc' s t =
      (NFA.transitionFunc nfa1 s t)
      ++ (NFA.transitionFunc nfa2 s t)
      ++ (if t == Epsilon && NFA.stateIsFinal nfa1 s then [NFA.initialState nfa2] else [])

disjunctNFAs :: Eq a => (NFA StateId a) -> (NFA StateId a) -> NFABuilder a
disjunctNFAs nfa1 nfa2 = do
  q0 <- nextStateId
  return $ NFA (initialState' q0) stateIsFinal' (transitionFunc' q0)
    where
      initialState' q0 = q0
      stateIsFinal' s = (NFA.stateIsFinal nfa1 s) || (NFA.stateIsFinal nfa2 s)
      transitionFunc' q0 s t =
        (NFA.transitionFunc nfa1 s t)
        ++ (NFA.transitionFunc nfa2 s t)
        ++ (if s == q0 && t == Epsilon then [NFA.initialState nfa1, NFA.initialState nfa2] else [])

kleeneNFA :: Eq a => (NFA StateId a) -> NFABuilder a
kleeneNFA nfa = do
  q0 <- nextStateId
  return $ NFA (initialState' q0) (stateIsFinal' q0) (transitionFunc' q0)
    where
      initialState' q0 = q0
      stateIsFinal' q0 s = s == q0 || (NFA.stateIsFinal nfa s)
      transitionFunc' q0 s t =
        (NFA.transitionFunc nfa s t)
        ++ (if s == q0 && t == Epsilon then [NFA.initialState nfa] else [])
        ++ (if NFA.stateIsFinal nfa s && t == Epsilon then [NFA.initialState nfa] else [])
