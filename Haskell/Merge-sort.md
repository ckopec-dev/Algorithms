# Merge Sort in Haskell

Here's an implementation of the merge sort algorithm in Haskell:

```haskell
-- Merge sort implementation in Haskell
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    -- Split the list into two halves
    (left, right) = splitAt (length xs `div` 2) xs
    
    -- Merge two sorted lists
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Example usage
main :: IO ()
main = do
  let unsorted = [64, 34, 25, 12, 22, 11, 90]
  let sorted = mergeSort unsorted
  putStrLn $ "Unsorted: " ++ show unsorted
  putStrLn $ "Sorted:   " ++ show sorted
```

## How it works:

1. **Base cases**: Empty list returns empty list, single element list returns itself
2. **Split**: Divide the list into two halves using `splitAt`
3. **Recursion**: Sort both halves recursively
4. **Merge**: Combine the two sorted halves using the `merge` helper function

## Key Haskell features in this implementation:

- **Pattern matching**: `merge [] ys = ys` and `merge (x:xs) (y:ys)`
- **Guards**: `| x <= y` for conditional logic
- **Lazy evaluation**: Lists are processed only as needed
- **Type signatures**: `Ord a => [a] -> [a]` ensures elements can be ordered

## Sample Output:
```
Unsorted: [64,34,25,12,22,11,90]
Sorted:   [11,12,22,25,34,64,90]
```

