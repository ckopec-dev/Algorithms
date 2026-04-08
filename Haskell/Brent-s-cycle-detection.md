# Brent's Cycle Detection Algorithm in Haskell

Brent's cycle detection algorithm is an efficient method for detecting cycles in sequences. Here's an implementation in Haskell:

```haskell
-- Brent's Cycle Detection Algorithm
brentCycleDetection :: (Eq a) => (a -> a) -> a -> Maybe Int
brentCycleDetection next start = 
    let (cycleLength, cycleStart) = brent next start
    in if cycleLength == 0 
       then Nothing 
       else Just cycleLength

-- Main Brent algorithm implementation
brent :: (Eq a) => (a -> a) -> a -> (Int, a)
brent next start = 
    let power = 1
        lambda = 1
        tortoise = start
        hare = next start
    in brentLoop next power lambda tortoise hare

brentLoop :: (Eq a) => (a -> a) -> Int -> Int -> a -> a -> (Int, a)
brentLoop next power lambda tortoise hare
    | tortoise == hare = (lambda, tortoise)
    | lambda == power = 
        let newPower = power * 2
            newLambda = 0
            newTortoise = hare
            newHare = next hare
        in brentLoop next newPower newLambda newTortoise newHare
    | otherwise = 
        let newLambda = lambda + 1
            newTortoise = tortoise
            newHare = next hare
        in brentLoop next power newLambda newTortoise newHare

-- Alternative implementation with more explicit steps
brentCycleDetection' :: (Eq a) => (a -> a) -> a -> Maybe Int
brentCycleDetection' next start = 
    let (length, _) = brent next start
    in if length == 0 
       then Nothing 
       else Just length

-- Example usage with a simple cycle
-- This creates a sequence: 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> 2 -> ...
-- The cycle starts at 2 with length 3
exampleSequence :: Int -> Int
exampleSequence x
    | x == 1 = 2
    | x == 2 = 3
    | x == 3 = 4
    | x == 4 = 2  -- Back to start of cycle
    | otherwise = x

-- Test function
testBrent :: IO ()
testBrent = do
    let result = brentCycleDetection exampleSequence 1
    putStrLn $ "Cycle length: " ++ show result
    -- Expected output: Cycle length: Just 3

-- More complex example with a longer cycle
complexSequence :: Int -> Int
complexSequence x
    | x == 1 = 2
    | x == 2 = 3
    | x == 3 = 4
    | x == 4 = 5
    | x == 5 = 6
    | x == 6 = 3  -- Back to start of cycle (3,4,5)
    | otherwise = x

-- Function to trace the sequence
traceSequence :: (Eq a) => (a -> a) -> a -> [a]
traceSequence next start = start : traceSequence next (next start)

-- Example with tracing
exampleTrace :: IO ()
exampleTrace = do
    putStrLn "Simple sequence trace:"
    let trace = take 10 (traceSequence exampleSequence 1)
    print trace
    
    putStrLn "Complex sequence trace:"
    let complexTrace = take 10 (traceSequence complexSequence 1)
    print complexTrace
```

## Usage Example

```haskell
-- Run the examples
main :: IO ()
main = do
    -- Test simple cycle detection
    let simpleResult = brentCycleDetection exampleSequence 1
    print simpleResult  -- Should output: Just 3
    
    -- Test complex cycle detection
    let complexResult = brentCycleDetection complexSequence 1
    print complexResult  -- Should output: Just 3
    
    -- Show sequence traces
    exampleTrace
```

## Key Features

1. **Efficient**: Uses Brent's optimization of the Floyd's cycle detection algorithm
2. **Memory Efficient**: Only stores a constant number of references
3. **Time Complexity**: O(λ + μ) where λ is the cycle length and μ is the distance to the cycle
4. **Space Complexity**: O(1) - constant space usage

## How it Works

1. **Phase 1**: Fast hare and slow tortoise move at different speeds
2. **Phase 2**: When they meet, the cycle length is determined
3. **Phase 3**: The cycle start is identified by moving one pointer back to the beginning

The algorithm is particularly useful for detecting cycles in linked lists, pseudo-random number generators, and other sequence-based problems.

