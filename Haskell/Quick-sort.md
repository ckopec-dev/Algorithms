# Quick Sort in Haskell

Here's an implementation of the Quick Sort algorithm in Haskell:

```haskell
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger = [y | y <- xs, y > x]
```

## How it works:

1. **Base case**: An empty list is already sorted
2. **Recursive case**: 
   - Take the first element `x` as the pivot
   - Partition the rest of the list into two sublists:
     - `smaller`: elements ≤ pivot
     - `larger`: elements > pivot
   - Recursively sort both sublists
   - Concatenate: sorted smaller + pivot + sorted larger

## Example usage:

```haskell
-- Sort a list of numbers
ghci> quickSort [3, 6, 8, 1, 4, 2, 5]
[1,2,3,4,5,6,8]

-- Sort a list of strings
ghci> quickSort ["banana", "apple", "cherry", "date"]
["apple","banana","cherry","date"]

-- Sort an already sorted list
ghci> quickSort [1, 2, 3, 4, 5]
[1,2,3,4,5]

-- Sort a reverse sorted list
ghci> quickSort [5, 4, 3, 2, 1]
[1,2,3,4,5]
```

## Alternative implementation with explicit partitioning:

```haskell
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = 
    let (smaller, larger) = partition (< x) xs
    in quickSort' smaller ++ [x] ++ quickSort' larger
  where
    partition p [] = ([], [])
    partition p (y:ys)
        | p y       = (y:smaller, larger)
        | otherwise = (smaller, y:larger)
        where (smaller, larger) = partition p ys
```

This implementation is elegant and functional, leveraging Haskell's pattern matching and list comprehensions to create a clean, readable sorting algorithm.

