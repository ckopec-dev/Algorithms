# Viterbi Algorithm in Haskell

Here's an implementation of the Viterbi algorithm for Hidden Markov Models in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module Viterbi where

import Data.List (foldl')
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Define the types for our HMM
type State = String
type Observation = String
type Probability = Double

-- HMM model definition
data HMM = HMM
  { states :: [State]
  , observations :: [Observation]
  , startProb :: [(State, Probability)]
  , transProb :: [(State, [(State, Probability)])]
  , emitProb :: [(State, [(Observation, Probability)])]
  } deriving (Show, Eq)

-- Viterbi algorithm implementation
viterbi :: HMM -> [Observation] -> [(State, Probability)]
viterbi hmm observations = 
  let statesList = states hmm
      startProbs = map snd (filter (\(s, _) -> s `elem` statesList) (startProb hmm))
      transProbs = map (\s -> (s, map snd (filter (\(s', _) -> s' == s) (concatMap snd (transProb hmm))))) statesList
      emitProbs = map (\s -> (s, map snd (filter (\(s', _) -> s' == s) (concatMap snd (emitProb hmm))))) statesList
  in viterbi' statesList startProbs transProbs emitProbs observations

-- Core Viterbi algorithm
viterbi' :: [State] -> [Probability] -> [(State, [Probability])] -> [(State, [Probability])] -> [Observation] -> [(State, Probability)]
viterbi' states startProbs transProbs emitProbs obs
  | null obs = []
  | otherwise = 
    let (probabilities, backpointers) = viterbiStep states startProbs transProbs emitProbs (head obs) (repeat (0, ""))
        finalStates = zip states probabilities
        bestPath = traceback backpointers (maximumByProb finalStates)
    in finalStates

-- Simplified version for clarity
viterbiSimple :: HMM -> [Observation] -> [State]
viterbiSimple hmm obs = 
  let states = states hmm
      n = length states
      -- Initialize
      v = replicate n 0.0
      b = replicate n ""
      -- For each observation
      (vFinal, bFinal) = foldl' (viterbiUpdate hmm) (v, b) obs
      -- Find best state
      bestStateIndex = fromMaybe 0 (findIndex (== maximum vFinal) vFinal)
  in [states !! bestStateIndex]

-- Helper functions
viterbiUpdate :: HMM -> ([Probability], [String]) -> Observation -> ([Probability], [String])
viterbiUpdate hmm (v, b) obs = 
  let states = states hmm
      n = length states
      newV = zipWith (\i _ -> 
        let probs = zipWith (\j _ -> 
          let trans = getTransProb hmm (states !! j) (states !! i)
              emit = getEmitProb hmm (states !! i) obs
              prev = v !! j
          in trans * emit * prev
          ) [0..] (repeat ())
        in maximum probs
        ) [0..] (repeat ())
      newB = zipWith (\i _ -> 
        let probs = zipWith (\j _ -> 
          let trans = getTransProb hmm (states !! j) (states !! i)
              emit = getEmitProb hmm (states !! i) obs
              prev = v !! j
          in trans * emit * prev
          ) [0..] (repeat ())
        in states !! (fromMaybe 0 (findIndex (== maximum probs) probs))
        ) [0..] (repeat ())
  in (newV, newB)

-- Helper functions for getting probabilities
getStartProb :: HMM -> State -> Probability
getStartProb hmm state = fromMaybe 0.0 (lookup state (startProb hmm))

getTransProb :: HMM -> State -> State -> Probability
getTransProb hmm from to = 
  let trans = lookup from (transProb hmm)
  in fromMaybe 0.0 (lookup to (fromMaybe [] trans))

getEmitProb :: HMM -> State -> Observation -> Probability
getEmitProb hmm state obs = 
  let emit = lookup state (emitProb hmm)
  in fromMaybe 0.0 (lookup obs (fromMaybe [] emit))

-- More practical implementation
viterbiPath :: HMM -> [Observation] -> [State]
viterbiPath hmm observations = 
  let states = states hmm
      n = length states
      -- Create transition and emission matrices
      transMatrix = [[getTransProb hmm from to | to <- states] | from <- states]
      emitMatrix = [[getEmitProb hmm state obs | obs <- observations] | state <- states]
      -- Initialize
      v = [getStartProb hmm state * getEmitProb hmm state (head observations) | state <- states]
      b = replicate n (-1)
      -- Dynamic programming
      (finalV, finalB) = foldl' (viterbiStep hmm) (v, b) (tail observations)
      -- Backtrack
      bestLastState = fromMaybe 0 (findIndex (== maximum finalV) finalV)
  in backtrack finalB bestLastState states

-- Backtracking function
backtrack :: [Int] -> Int -> [State] -> [State]
backtrack b lastState states = 
  let path = iterate (\i -> b !! i) lastState
  in reverse (map (states !!) (takeWhile (/= -1) path))

-- Helper functions
viterbiStep :: HMM -> ([Probability], [Int]) -> Observation -> ([Probability], [Int])
viterbiStep hmm (v, b) obs = 
  let states = states hmm
      n = length states
      newV = [maximum [v !! j * getTransProb hmm (states !! j) (states !! i) * getEmitProb hmm (states !! i) obs | j <- [0..n-1]] | i <- [0..n-1]]
      newB = [fromMaybe 0 (findIndex (== maximum [v !! j * getTransProb hmm (states !! j) (states !! i) * getEmitProb hmm (states !! i) obs | j <- [0..n-1]]) [v !! j * getTransProb hmm (states !! j) (states !! i) * getEmitProb hmm (states !! i) obs | j <- [0..n-1]]) | i <- [0..n-1]]
  in (newV, newB)

-- Utility functions
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = go 0
  where go n (x:xs) = if p x then Just n else go (n+1) xs
        go _ [] = Nothing

maximumByProb :: [(State, Probability)] -> (State, Probability)
maximumByProb = foldl' (\acc (s, p) -> if p > snd acc then (s, p) else acc) (head [])

-- Example usage
exampleHMM :: HMM
exampleHMM = HMM
  { states = ["Sunny", "Rainy"]
  , observations = ["Walk", "Shop", "Clean"]
  , startProb = [("Sunny", 0.6), ("Rainy", 0.4)]
  , transProb = 
    [ ("Sunny", [("Sunny", 0.7), ("Rainy", 0.3)])
    , ("Rainy", [("Sunny", 0.4), ("Rainy", 0.6)])
    ]
  , emitProb = 
    [ ("Sunny", [("Walk", 0.6), ("Shop", 0.3), ("Clean", 0.1)])
    , ("Rainy", [("Walk", 0.2), ("Shop", 0.3), ("Clean", 0.5)])
    ]
  }

-- Run example
exampleRun :: IO ()
exampleRun = do
  let observations = ["Walk", "Shop", "Clean"]
  let path = viterbiPath exampleHMM observations
  putStrLn $ "Observations: " ++ show observations
  putStrLn $ "Most likely path: " ++ show path
```

## Usage Example

```haskell
-- Run the example
main :: IO ()
main = do
  let observations = ["Walk", "Shop", "Clean"]
  let path = viterbiPath exampleHMM observations
  putStrLn $ "Most likely weather sequence: " ++ show path
```

## Key Features

1. **Type Safety**: Uses Haskell's strong type system to define HMM components
2. **Functional Approach**: Pure functions with no side effects
3. **Pattern Matching**: Uses pattern matching for clean code structure
4. **Lazy Evaluation**: Leverages Haskell's lazy evaluation for efficiency
5. **Error Handling**: Uses `Maybe` type for safe operations

## Algorithm Complexity

- **Time Complexity**: O(N²T) where N is the number of states and T is the number of observations
- **Space Complexity**: O(NT) for storing the dynamic programming table

This implementation provides a clean, idiomatic Haskell approach to the Viterbi algorithm for Hidden Markov Models.

