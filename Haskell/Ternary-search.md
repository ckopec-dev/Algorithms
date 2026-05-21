# Ternary Search in Haskell

Here's an implementation of the ternary search algorithm in Haskell:

```haskell
-- Ternary search implementation
ternarySearch :: (Ord a) => [a] -> a -> Maybe Int
ternarySearch arr target = ternarySearchHelper arr target 0 (length arr - 1)
  where
    ternarySearchHelper :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
    ternarySearchHelper arr target left right
      | left > right = Nothing
      | otherwise =
          let mid1 = left + (right - left) `div` 3
              mid2 = right - (right - left) `div` 3
              val1 = arr !! mid1
              val2 = arr !! mid2
          in case compare target val1 of
               EQ -> Just mid1
               LT -> ternarySearchHelper arr target left (mid1 - 1)
               GT -> 
                 case compare target val2 of
                   EQ -> Just mid2
                   LT -> ternarySearchHelper arr target (mid1 + 1) (mid2 - 1)
                   GT -> ternarySearchHelper arr target (mid2 + 1) right

-- Alternative implementation using explicit bounds checking
ternarySearch' :: (Ord a) => [a] -> a -> Maybe Int
ternarySearch' arr target = ternarySearchBounds arr target 0 (length arr - 1)
  where
    ternarySearchBounds :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
    ternarySearchBounds arr target left right
      | left > right = Nothing
      | left == right = if arr !! left == target then Just left else Nothing
      | otherwise = 
          let third = (right - left) `div` 3
              mid1 = left + third
              mid2 = right - third
              val1 = arr !! mid1
              val2 = arr !! mid2
          in case (compare target val1, compare target val2) of
               (EQ, _) -> Just mid1
               (_, EQ) -> Just mid2
               (LT, _) -> ternarySearchBounds arr target left (mid1 - 1)
               (_, LT) -> ternarySearchBounds arr target (mid1 + 1) (mid2 - 1)
               (_, _) -> ternarySearchBounds arr target (mid2 + 1) right

-- Example usage
main :: IO ()
main = do
  let sortedArray = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  -- Search for existing elements
  print $ ternarySearch sortedArray 7    -- Just 3
  print $ ternarySearch sortedArray 1    -- Just 0
  print $ ternarySearch sortedArray 19   -- Just 9
  
  -- Search for non-existing elements
  print $ ternarySearch sortedArray 6    -- Nothing
  print $ ternarySearch sortedArray 20   -- Nothing
  
  -- Empty array
  print $ ternarySearch [] 5             -- Nothing
```

## How it works:

1. **Divide**: The array is divided into three parts using two midpoints
2. **Compare**: The target is compared with the values at both midpoints
3. **Conquer**: Based on the comparison, the search space is reduced to one of three parts
4. **Recursive**: The process continues recursively until the element is found or the search space is exhausted

## Key Features:

- **Type Safety**: Uses Haskell's strong type system with `Ord` constraint
- **Pattern Matching**: Leverages Haskell's pattern matching for clean comparison logic
- **Maybe Type**: Returns `Nothing` when element not found, `Just index` when found
- **Recursive**: Implements the search recursively for clarity and functional style

## Time Complexity:
- **Time**: O(log₃ n) - logarithmic base 3
- **Space**: O(log₃ n) - due to recursion stack

The ternary search is particularly useful when the array is sorted and you want to minimize the number of comparisons, though in practice the difference from binary search is often negligible due to the constant factors involved.

