# Insertion Sort in Haskell

Here's an implementation of the insertion sort algorithm in Haskell:

```haskell
-- Insertion sort implementation
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- Alternative implementation using fold
insertionSort' :: Ord a => [a] -> [a]
insertionSort' = foldl insert [] 
  where
    insert :: Ord a => [a] -> a -> [a]
    insert [] x = [x]
    insert (y:ys) x
      | x <= y    = x : y : ys
      | otherwise = y : insert ys x
```

## Example Usage

```haskell
-- Examples
main :: IO ()
main = do
    let list1 = [64, 34, 25, 12, 22, 11, 90]
    let list2 = [5, 2, 4, 6, 1, 3]
    let list3 = []
    let list4 = [42]
    
    putStrLn $ "Original: " ++ show list1
    putStrLn $ "Sorted:   " ++ show (insertionSort list1)
    
    putStrLn $ "Original: " ++ show list2
    putStrLn $ "Sorted:   " ++ show (insertionSort list2)
    
    putStrLn $ "Original: " ++ show list3
    putStrLn $ "Sorted:   " ++ show (insertionSort list3)
    
    putStrLn $ "Original: " ++ show list4
    putStrLn $ "Sorted:   " ++ show (insertionSort list4)
```

## Output
```
Original: [64,34,25,12,22,11,90]
Sorted:   [11,12,22,25,34,64,90]
Original: [5,2,4,6,1,3]
Sorted:   [1,2,3,4,5,6]
Original: []
Sorted:   []
Original: [42]
Sorted:   [42]
```

## How it works

1. **Base case**: Empty list returns empty list
2. **Recursive case**: Take the first element and insert it into the sorted rest of the list
3. **Insert function**: Places an element in the correct position in a sorted list
4. **Ordering**: Uses `Ord` typeclass for comparison operations

The algorithm has O(n²) time complexity in the worst case and O(n) in the best case when the input is already sorted.

