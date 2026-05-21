# Randomized Quicksort in Haskell

Here's an implementation of the Randomized Quicksort algorithm in Haskell:

```haskell
import System.Random

-- Randomized Quicksort implementation
randomizedQuicksort :: Ord a => [a] -> [a]
randomizedQuicksort [] = []
randomizedQuicksort (x:xs) = 
    let (smaller, equal, larger) = partition3 x xs
        smallerSorted = randomizedQuicksort smaller
        largerSorted = randomizedQuicksort larger
    in smallerSorted ++ equal ++ largerSorted

-- Helper function to partition list into three parts
partition3 :: Ord a => a -> [a] -> ([a], [a], [a])
partition3 pivot xs = (smaller, equal, larger)
  where
    smaller = [x | x <- xs, x < pivot]
    equal = [x | x <- xs, x == pivot]
    larger = [x | x <- xs, x > pivot]

-- Alternative implementation with random pivot selection
randomizedQuicksort' :: Ord a => [a] -> IO [a]
randomizedQuicksort' [] = return []
randomizedQuicksort' xs = do
    -- Generate a random index
    let n = length xs
    randomIndex <- randomRIO (0, n - 1)
    let pivot = xs !! randomIndex
    let (smaller, equal, larger) = partition3 pivot xs
    smallerSorted <- randomizedQuicksort' smaller
    largerSorted <- randomizedQuicksort' larger
    return (smallerSorted ++ equal ++ largerSorted)

-- Example usage
main :: IO ()
main = do
    let unsorted = [64, 34, 25, 12, 22, 11, 90, 5]
    putStrLn $ "Original list: " ++ show unsorted
    
    let sorted = randomizedQuicksort unsorted
    putStrLn $ "Sorted list: " ++ show sorted
    
    -- Example with random pivot selection
    putStrLn "Using random pivot selection:"
    sortedRandom <- randomizedQuicksort' unsorted
    putStrLn $ "Sorted list: " ++ show sortedRandom
```

## Key Features:

1. **Three-way partitioning**: Splits the list into elements smaller than pivot, equal to pivot, and larger than pivot
2. **Random pivot selection**: The second implementation randomly selects a pivot element
3. **Type safety**: Uses Haskell's type system with `Ord a` constraint
4. **Functional approach**: Pure functions without side effects
5. **IO handling**: The second version handles random number generation using `IO`

## Time Complexity:
- **Average case**: O(n log n)
- **Worst case**: O(n²) - very unlikely with randomization
- **Space complexity**: O(log n) due to recursion stack

## Example Output:
```
Original list: [64,34,25,12,22,11,90,5]
Sorted list: [5,11,12,22,25,34,64,90]
Using random pivot selection:
Sorted list: [5,11,12,22,25,34,64,90]
```

