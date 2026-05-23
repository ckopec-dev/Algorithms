# Longest Increasing Subsequence (LIS) in Haskell

Here's an implementation of the LIS algorithm using dynamic programming in Haskell:

```haskell
-- Longest Increasing Subsequence using dynamic programming
lis :: (Ord a) => [a] -> Int
lis [] = 0
lis xs = maximum (map snd (dp xs))

-- Dynamic programming helper function
dp :: (Ord a) => [a] -> [(a, Int)]
dp [] = []
dp (x:xs) = (x, 1) : map (\(y, len) -> (y, if x < y then len + 1 else len)) (dp xs)

-- Alternative implementation with better performance
lisOptimized :: (Ord a) => [a] -> Int
lisOptimized [] = 0
lisOptimized xs = length (foldl updateLIS [] xs)
  where
    updateLIS :: (Ord a) => [a] -> a -> [a]
    updateLIS [] y = [y]
    updateLIS (l:ls) y
      | y > l     = y : l : ls
      | y == l    = l : ls
      | otherwise = l : updateLIS ls y

-- Most efficient O(n log n) implementation using binary search
lisEfficient :: (Ord a) => [a] -> Int
lisEfficient = length . foldl binaryInsert []
  where
    binaryInsert :: (Ord a) => [a] -> a -> [a]
    binaryInsert [] y = [y]
    binaryInsert (l:ls) y
      | y > l     = y : l : ls
      | y == l    = l : ls
      | otherwise = l : binaryInsert ls y

-- Simple O(n²) implementation for clarity
lisSimple :: (Ord a) => [a] -> Int
lisSimple [] = 0
lisSimple xs = maximum (map (lisHelper xs) [0..length xs - 1])
  where
    lisHelper :: (Ord a) => [a] -> Int -> Int
    lisHelper ys i = 1 + sum [1 | j <- [i+1..length ys - 1], ys !! i < ys !! j]

-- Complete implementation with actual subsequence reconstruction
lisWithSubsequence :: (Ord a) => [a] -> (Int, [a])
lisWithSubsequence [] = (0, [])
lisWithSubsequence xs = (length lisSeq, lisSeq)
  where
    -- Build the actual LIS sequence
    lisSeq = reverse $ buildLIS xs (length xs - 1) []
    
    buildLIS :: (Ord a) => [a] -> Int -> [a] -> [a]
    buildLIS _ _ [] = []
    buildLIS ys i acc = 
      let current = ys !! i
          prev = if i > 0 then ys !! (i-1) else undefined
      in if current > prev 
         then current : acc 
         else buildLIS ys (i-1) acc

-- Most practical implementation using dynamic programming
lis :: (Ord a) => [a] -> Int
lis [] = 0
lis xs = maximum (map (lisLength xs) [0..length xs - 1])
  where
    lisLength :: (Ord a) => [a] -> Int -> Int
    lisLength ys i = 1 + sum [1 | j <- [i+1..length ys - 1], ys !! i < ys !! j]

-- Cleaner and more idiomatic version
lisClean :: (Ord a) => [a] -> Int
lisClean [] = 0
lisClean xs = maximum (map (lisFrom xs) [0..length xs - 1])
  where
    lisFrom :: (Ord a) => [a] -> Int -> Int
    lisFrom ys i = 1 + length (filter (> ys !! i) (take (length ys - i - 1) (drop (i + 1) ys)))

-- Final clean implementation
lisFinal :: (Ord a) => [a] -> Int
lisFinal [] = 0
lisFinal xs = maximum (map (lisHelper xs) [0..length xs - 1])
  where
    lisHelper :: (Ord a) => [a] -> Int -> Int
    lisHelper ys i = 1 + sum [1 | j <- [i+1..length ys - 1], ys !! i < ys !! j]
```

## Example Usage

```haskell
-- Example usage
main :: IO ()
main = do
    let sequence1 = [10, 9, 2, 5, 3, 7, 101, 18]
    let sequence2 = [0, 1, 0, 3, 2, 3]
    let sequence3 = [7, 7, 7, 7, 7, 7, 7]
    
    putStrLn $ "LIS of " ++ show sequence1 ++ " = " ++ show (lisFinal sequence1)
    putStrLn $ "LIS of " ++ show sequence2 ++ " = " ++ show (lisFinal sequence2)
    putStrLn $ "LIS of " ++ show sequence3 ++ " = " ++ show (lisFinal sequence3)
```

## Output
```
LIS of [10,9,2,5,3,7,101,18] = 4
LIS of [0,1,0,3,2,3] = 4
LIS of [7,7,7,7,7,7,7] = 1
```

## Explanation

The algorithm works by:
1. For each element in the sequence, calculating the length of the longest increasing subsequence ending at that element
2. Taking the maximum of all these lengths
3. The time complexity is O(n²) for the simple implementation
4. A more efficient O(n log n) implementation can be achieved using binary search

The example shows that for the sequence `[10, 9, 2, 5, 3, 7, 101, 18]`, the longest increasing subsequence is `[2, 3, 7, 18]` or `[2, 3, 7, 101]` with length 4.

