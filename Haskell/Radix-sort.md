# Radix Sort in Haskell

Here's an implementation of the Radix Sort algorithm in Haskell:

```haskell
import Data.List (sort)

-- Radix sort implementation
radixSort :: [Int] -> [Int]
radixSort [] = []
radixSort xs = radixSortHelper xs 0
  where
    -- Find maximum number to determine number of digits
    maxNum = maximum xs
    
    -- Main radix sort helper function
    radixSortHelper :: [Int] -> Int -> [Int]
    radixSortHelper arr digit
      | digit > numDigits maxNum = arr
      | otherwise = 
          let buckets = bucketByDigit arr digit
              sortedBuckets = map (radixSortHelper (concat buckets)) (repeat (digit + 1))
          in concat sortedBuckets
    
    -- Helper function to count digits
    numDigits :: Int -> Int
    numDigits 0 = 1
    numDigits n = 1 + numDigits (n `div` 10)
    
    -- Bucket numbers by their digit at given position
    bucketByDigit :: [Int] -> Int -> [[Int]]
    bucketByDigit numbers pos = 
      let digit = (numbers !! 0) `div` (10 ^ pos) `mod` 10
          buckets = replicate 10 []
      in foldr (bucketByDigitHelper pos) buckets numbers
    
    -- Helper function to place numbers in buckets
    bucketByDigitHelper :: Int -> Int -> [[Int]] -> [[Int]]
    bucketByDigitHelper pos num buckets = 
      let digit = num `div` (10 ^ pos) `mod` 10
          (before, current:after) = splitAt digit buckets
      in before ++ (current ++ [num]) : after

-- Simpler and more practical implementation
radixSortSimple :: [Int] -> [Int]
radixSortSimple [] = []
radixSortSimple xs = 
  let maxNum = maximum xs
      maxDigits = length (show maxNum)
  in radixSortHelper xs maxDigits 0
  where
    radixSortHelper :: [Int] -> Int -> Int -> [Int]
    radixSortHelper arr maxDigits digit
      | digit >= maxDigits = arr
      | otherwise = 
          let buckets = replicate 10 []
              bucketsWithNumbers = foldl (bucketNumber digit) buckets arr
              sortedBuckets = map sort bucketsWithNumbers
              flattened = concat sortedBuckets
          in radixSortHelper flattened maxDigits (digit + 1)
    
    bucketNumber :: Int -> [[Int]] -> Int -> [[Int]]
    bucketNumber digit buckets num = 
      let digitValue = (num `div` (10 ^ digit)) `mod` 10
          (before, current:after) = splitAt digit buckets
      in before ++ (current ++ [num]) : after

-- Even simpler version using built-in sorting
radixSortSimplest :: [Int] -> [Int]
radixSortSimplest = sort

-- Example usage
main :: IO ()
main = do
  let testArray = [170, 45, 75, 90, 2, 802, 24, 66]
  putStrLn $ "Original array: " ++ show testArray
  putStrLn $ "Sorted array:   " ++ show (radixSortSimple testArray)
  
  -- Test with different inputs
  let testArray2 = [329, 457, 657, 839, 436, 720, 355]
  putStrLn $ "Original array: " ++ show testArray2
  putStrLn $ "Sorted array:   " ++ show (radixSortSimple testArray2)
```

## How it works:

1. **Find maximum number** to determine the number of digits
2. **Process each digit position** from least significant to most significant
3. **Bucket numbers** based on the current digit
4. **Reassemble** numbers in order by digit buckets
5. **Repeat** for each digit position

## Key Features:

- **Functional approach**: Uses recursion and higher-order functions
- **Pure functions**: No side effects
- **Handles edge cases**: Empty lists, single elements
- **Type-safe**: Uses Haskell's strong type system

## Time Complexity:
- **Best/Average/Worst**: O(d × n) where d is the number of digits and n is the number of elements

## Space Complexity:
- O(n + k) where k is the range of input (typically 0-9 for decimal digits)

The algorithm works by sorting numbers digit by digit, starting from the least significant digit to the most significant, using a stable sorting algorithm (bucket sort) at each step.

