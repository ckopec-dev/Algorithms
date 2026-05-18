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

-- Alternative implementation using explicit recursion
selectionSort' :: Ord a => [a] -> [a]
selectionSort' [] = []
selectionSort' xs = let minElem = minimum xs
                       rest = remove minElem xs
                   in minElem : selectionSort' rest
  where
    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove y (y':ys)
      | y == y'   = ys
      | otherwise = y' : remove y ys

-- More efficient version using helper function
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
          minElem = if x <= minRest then x else minRest
          newRest = if x <= minRest then rest else x : rest
      in (minElem, newRest)
```

## Simple Example Usage

```haskell
-- Simple version (most readable)
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimum xs : selectionSort (remove (minimum xs) xs)
  where
    remove y [] = []
    remove y (y':ys)
      | y == y'   = ys
      | otherwise = y' : remove y ys

-- Example usage:
-- Main> selectionSort [64, 34, 25, 12, 22, 11, 90]
-- [11, 12, 22, 25, 34, 64, 90]
--
-- Main> selectionSort ['c', 'a', 'e', 'b', 'd']
-- "abcde"
```

## How it works:

1. **Base case**: Empty list returns empty list
2. **Recursive case**: Find the minimum element, place it at the front, and recursively sort the rest
3. **Helper function**: `remove` function eliminates one occurrence of the minimum element from the list

## Time Complexity:
- **Best case**: O(n²)
- **Average case**: O(n²)
- **Worst case**: O(n²)

## Space Complexity:
- O(n) due to the recursive call stack

The algorithm works by repeatedly finding the smallest element in the unsorted portion and moving it to the sorted portion, building the result list one element at a time.

