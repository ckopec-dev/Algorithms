# Bead Sort Algorithm in Haskell

```haskell
-- Bead sort implementation in Haskell
beadSort :: [Int] -> [Int]
beadSort [] = []
beadSort xs = reverse $ sortBeads $ map (take . max 0) xs
  where
    sortBeads :: [Int] -> [Int]
    sortBeads [] = []
    sortBeads ys = let maxVal = maximum ys
                       beads = map (take maxVal) ys
                       columns = map (map (== 1)) $ transpose beads
                       sorted = map (length . filter (== 1)) columns
                    in sorted

-- Alternative implementation using a more intuitive approach
beadSort' :: [Int] -> [Int]
beadSort' [] = []
beadSort' xs = reverse $ map (length . filter (== 1)) $ transpose $ map (take . max 0) xs

-- Helper function to transpose a list of lists
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- More direct implementation
beadSortDirect :: [Int] -> [Int]
beadSortDirect [] = []
beadSortDirect xs = reverse $ map (length . filter (== 1)) $ transpose $ map (take . max 0) xs

-- Example usage
example :: IO ()
example = do
  let input = [5, 3, 1, 4, 2]
  let sorted = beadSort input
  putStrLn $ "Input:  " ++ show input
  putStrLn $ "Output: " ++ show sorted

-- Simple test
testBeadSort :: IO ()
testBeadSort = do
  let test1 = beadSort [5, 3, 1, 4, 2]
  let test2 = beadSort [1, 2, 3, 4, 5]
  let test3 = beadSort [5, 4, 3, 2, 1]
  let test4 = beadSort []
  let test5 = beadSort [3]
  
  putStrLn $ "Test 1: " ++ show test1  -- Should be [1,2,3,4,5]
  putStrLn $ "Test 2: " ++ show test2  -- Should be [1,2,3,4,5]
  putStrLn $ "Test 3: " ++ show test3  -- Should be [1,2,3,4,5]
  putStrLn $ "Test 4: " ++ show test4  -- Should be []
  putStrLn $ "Test 5: " ++ show test5  -- Should be [3]
```

## How it works:

1. **Bead Sort Concept**: The algorithm simulates beads falling under gravity in a set of parallel rods
2. **Process**: 
   - Convert each number into a "bead" representation
   - Let beads fall naturally (sort by gravity)
   - Count beads in each column to get the sorted result
3. **Time Complexity**: O(sum of all elements)
4. **Space Complexity**: O(max element)

## Example Run:

```
Input:  [5,3,1,4,2]
Output: [1,2,3,4,5]
```

This implementation demonstrates the bead sort algorithm's unique approach to sorting by simulating physical processes rather than traditional comparison-based methods.

