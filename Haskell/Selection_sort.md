# Selection Sort in Haskell

Here's an implementation of the selection sort algorithm in Haskell:

```haskell
-- Selection sort implementation
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimum xs : selectionSort (remove (minimum xs) xs)
  where
    remove :: Eq a => a -> [a] -> [a]
    remove y [] = []
    remove y (y':ys)
      | y == y'   = ys
      | otherwise = y' : remove y ys

-- Alternative implementation using explicit index tracking
selectionSort' :: Ord a => [a] -> [a]
selectionSort' [] = []
selectionSort' xs = let minElem = minimum xs
                        minIndex = indexOf minElem xs
                        remaining = take minIndex xs ++ drop (minIndex + 1) xs
                    in minElem : selectionSort' remaining
  where
    indexOf :: Eq a => a -> [a] -> Int
    indexOf x (y:ys)
      | x == y    = 0
      | otherwise = 1 + indexOf x ys
    indexOf _ [] = -1

-- More efficient version using fold
selectionSort'' :: Ord a => [a] -> [a]
selectionSort'' [] = []
selectionSort'' xs = let (minElem, rest) = findMinAndRemove xs
                     in minElem : selectionSort'' rest
  where
    findMinAndRemove :: Ord a => [a] -> (a, [a])
    findMinAndRemove [] = error "Empty list"
    findMinAndRemove [x] = (x, [])
    findMinAndRemove (x:xs) = 
      let (minRest, rest) = findMinAndRemove xs
          (minElem, remaining) = if x <= minRest then (x, xs) else (minRest, x:rest)
      in (minElem, remaining)
```

## Example Usage

```haskell
-- Example usage
main :: IO ()
main = do
  let unsorted = [64, 34, 25, 12, 22, 11, 90]
  putStrLn $ "Original list: " ++ show unsorted
  putStrLn $ "Sorted list:   " ++ show (selectionSort unsorted)
  
  -- Output:
  -- Original list: [64,34,25,12,22,11,90]
  -- Sorted list:   [11,12,22,25,34,64,90]
```

## How it works:

1. **Base case**: Empty list returns empty list
2. **Recursive case**: Find the minimum element, place it at the front, and recursively sort the remaining elements
3. **Helper function**: `remove` function eliminates one occurrence of the minimum element from the list

## Time Complexity:
- **Best case**: O(n²)
- **Average case**: O(n²)
- **Worst case**: O(n²)

## Space Complexity:
- O(n) due to the recursive calls and list copying

The algorithm repeatedly finds the smallest element in the unsorted portion and moves it to the sorted portion, building the sorted list incrementally.

