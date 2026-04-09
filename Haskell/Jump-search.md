# Jump Search Algorithm in Haskell

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified block.

```haskell
import Data.List (findIndex)

-- Jump search implementation
jumpSearch :: Ord a => [a] -> a -> Maybe Int
jumpSearch arr target = jumpSearchHelper arr target stepSize 0
  where
    n = length arr
    stepSize = floor $ sqrt $ fromIntegral n

jumpSearchHelper :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
jumpSearchHelper arr target stepSize prev
  | prev >= length arr = Nothing
  | target < arr !! prev = linearSearch arr target (prev - stepSize) (prev - 1)
  | target == arr !! prev = Just prev
  | target > arr !! prev = jumpSearchHelper arr target stepSize (prev + stepSize)
  | otherwise = Nothing

-- Linear search helper function
linearSearch :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
linearSearch arr target start end
  | start > end = Nothing
  | target == arr !! start = Just start
  | target < arr !! start = Nothing
  | otherwise = linearSearch arr target (start + 1) end

-- Alternative cleaner implementation using built-in functions
jumpSearchClean :: Ord a => [a] -> a -> Maybe Int
jumpSearchClean arr target = 
  let n = length arr
      step = floor $ sqrt $ fromIntegral n
  in jumpSearchLoop arr target step 0

jumpSearchLoop :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
jumpSearchLoop arr target step pos
  | pos >= n = Nothing
  | target < arr !! pos = findIndex (== target) (take (pos - prev + 1) (drop prev arr))
  | target == arr !! pos = Just pos
  | target > arr !! pos = jumpSearchLoop arr target step (pos + step)
  | otherwise = Nothing
  where
    n = length arr
    prev = max 0 (pos - step)

-- Simple and clean version
jumpSearchSimple :: Ord a => [a] -> a -> Maybe Int
jumpSearchSimple arr target = 
  let n = length arr
      step = max 1 $ floor $ sqrt $ fromIntegral n
      result = jumpSearchStep arr target step 0
  in result

jumpSearchStep :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
jumpSearchStep arr target step pos
  | pos >= n = Nothing
  | target == arr !! pos = Just pos
  | target < arr !! pos = linearSearch arr target (pos - step) (pos - 1)
  | otherwise = jumpSearchStep arr target step (pos + step)
  where
    n = length arr

-- Example usage
main :: IO ()
main = do
  let sortedArray = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
  let target1 = 55
  let target2 = 100
  let target3 = 0
  
  putStrLn "Sorted Array: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]"
  putStrLn $ "Searching for " ++ show target1 ++ ": " ++ show (jumpSearchSimple sortedArray target1)
  putStrLn $ "Searching for " ++ show target2 ++ ": " ++ show (jumpSearchSimple sortedArray target2)
  putStrLn $ "Searching for " ++ show target3 ++ ": " ++ show (jumpSearchSimple sortedArray target3)
```

## Output:
```
Sorted Array: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
Searching for 55: Just 10
Searching for 100: Nothing
Searching for 0: Just 0
```

## How it works:

1. **Calculate step size**: `step = floor (sqrt n)` where n is the array length
2. **Jump ahead**: Move through the array in steps of `step` size
3. **Bound detection**: When we find an element greater than target, we know the target is in the previous block
4. **Linear search**: Perform linear search in the identified block

## Time Complexity:
- **Best case**: O(1) - when element is found at the first position
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity: O(1)

The jump search algorithm is particularly useful when the cost of jumping ahead is much less than the cost of linear search within a block, making it more efficient than linear search for large sorted arrays.

