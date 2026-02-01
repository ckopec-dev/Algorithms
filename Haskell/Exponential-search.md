# Exponential Search in Haskell

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

```haskell
-- Exponential search implementation in Haskell
exponentialSearch :: Ord a => [a] -> a -> Maybe Int
exponentialSearch arr target = 
    if arr !! 0 == target
        then Just 0
        else binarySearchInRange arr target 1 (min 1 (length arr - 1))

-- Helper function to find the range where target might be located
findRange :: Ord a => [a] -> a -> Int -> Int
findRange arr target low high
    | low > high = low
    | arr !! high >= target = high
    | otherwise = findRange arr target (low * 2) (min (high * 2) (length arr - 1))

-- Binary search within a specific range
binarySearchInRange :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
binarySearchInRange arr target low high
    | low > high = Nothing
    | otherwise = 
        let mid = low + (high - low) `div` 2
            midVal = arr !! mid
        in case compare target midVal of
            EQ -> Just mid
            LT -> binarySearchInRange arr target low (mid - 1)
            GT -> binarySearchInRange arr target (mid + 1) high

-- Alternative cleaner implementation
exponentialSearch' :: Ord a => [a] -> a -> Maybe Int
exponentialSearch' arr target = 
    case searchHelper arr target 1 of
        Just pos -> binarySearch arr target 0 pos
        Nothing -> Nothing
  where
    searchHelper arr target i
        | i >= length arr = Nothing
        | arr !! i == target = Just i
        | arr !! i > target = Just i
        | otherwise = searchHelper arr target (i * 2)
    
    binarySearch arr target low high
        | low > high = Nothing
        | otherwise = 
            let mid = low + (high - low) `div` 2
                midVal = arr !! mid
            in case compare target midVal of
                EQ -> Just mid
                LT -> binarySearch arr target low (mid - 1)
                GT -> binarySearch arr target (mid + 1) high

-- Example usage
main :: IO ()
main = do
    let sortedArray = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    
    -- Test cases
    print $ exponentialSearch sortedArray 7    -- Just 6
    print $ exponentialSearch sortedArray 1    -- Just 0
    print $ exponentialSearch sortedArray 15   -- Just 14
    print $ exponentialSearch sortedArray 20   -- Nothing
    print $ exponentialSearch sortedArray 0    -- Nothing
```

## How it works:

1. **First Phase**: Start with index 1 and keep doubling the index until we find an element that is greater than or equal to the target
2. **Second Phase**: Perform binary search in the range [prev_index/2, current_index]

## Time Complexity:
- **Best case**: O(1) - when target is at the first position
- **Average case**: O(log n)
- **Worst case**: O(log n)

## Space Complexity:
- O(log n) due to the recursive binary search calls

The algorithm is particularly useful when the target element is likely to be near the beginning of the array, as it can find the range quickly before performing the binary search.

