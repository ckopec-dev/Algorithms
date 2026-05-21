# Dynamic Programming Example in Haskell: Fibonacci Sequence

Here's a classic dynamic programming example implemented in Haskell using memoization:

```haskell
-- Basic recursive fibonacci (inefficient)
fibNaive :: Int -> Int
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)

-- Dynamic programming with memoization using array
import Data.Array

fibDP :: Int -> Int
fibDP n = fibs ! n
  where
    fibs = array (0,n) [(i, fib i) | i <- [0..n]]
    fib 0 = 0
    fib 1 = 1
    fib i = fibs ! (i-1) + fibs ! (i-2)

-- Alternative implementation using lazy evaluation
fibLazy :: Int -> Int
fibLazy n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Using memoization with Map for arbitrary inputs
import qualified Data.Map as Map

fibMemo :: Int -> Int
fibMemo n = fibMemo' n Map.empty
  where
    fibMemo' 0 _ = 0
    fibMemo' 1 _ = 1
    fibMemo' x memo =
      case Map.lookup x memo of
        Just result -> result
        Nothing -> 
          let result = fibMemo' (x-1) memo' + fibMemo' (x-2) memo'
              memo' = Map.insert x result memo
          in result

-- Example usage
main :: IO ()
main = do
  putStrLn "Fibonacci numbers:"
  print $ map fibDP [0..10]
  print $ map fibLazy [0..10]
  print $ map fibMemo [0..10]
```

## Key Features of this Dynamic Programming Implementation:

1. **Memoization**: Caches previously computed results to avoid redundant calculations
2. **Bottom-up approach**: Builds solutions from smaller subproblems
3. **Lazy evaluation**: The `fibLazy` version uses Haskell's lazy evaluation for efficient computation
4. **Multiple approaches**: Shows different ways to implement dynamic programming in Haskell

## Time Complexity Comparison:
- **Naive recursive**: O(2^n) - exponential
- **Dynamic programming**: O(n) - linear
- **Space complexity**: O(n) for storing intermediate results

The dynamic programming approach dramatically improves performance by eliminating overlapping subproblems that occur in the naive recursive implementation.

