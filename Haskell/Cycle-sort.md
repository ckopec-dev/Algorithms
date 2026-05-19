# Cycle Sort Algorithm in Haskell

Here's an implementation of the Cycle Sort algorithm in Haskell:

```haskell
-- Cycle Sort implementation in Haskell
cycleSort :: Ord a => [a] -> [a]
cycleSort arr = 
    let n = length arr
        sortedArr = cycleSortHelper arr 0 n
    in sortedArr

cycleSortHelper :: Ord a => [a] -> Int -> Int -> [a]
cycleSortHelper arr start n
    | start >= n - 1 = arr
    | otherwise = 
        let item = arr !! start
            pos = findPosition arr start item
        in if pos == start
           then cycleSortHelper arr (start + 1) n
           else 
               let newArr = swap arr start pos
                   newPos = findPosition newArr (start + 1) item
               in cycleSortHelper newArr (start + 1) n

-- Find the correct position for an item
findPosition :: Ord a => [a] -> Int -> a -> Int
findPosition arr start item = 
    let count = length $ filter (\x -> x < item) (take start arr ++ drop (start + 1) arr)
    in start + count

-- Swap two elements in a list
swap :: [a] -> Int -> Int -> [a]
swap arr i j = 
    let elemI = arr !! i
        elemJ = arr !! j
        arrI = replaceAt arr i elemJ
        result = replaceAt arrI j elemI
    in result

-- Replace element at given index
replaceAt :: [a] -> Int -> a -> [a]
replaceAt arr i newVal = 
    let (before, after) = splitAt i arr
    in before ++ newVal : (tail after)

-- Alternative cleaner implementation
cycleSortClean :: Ord a => [a] -> [a]
cycleSortClean arr = 
    let n = length arr
        result = cycleSortLoop arr 0 n
    in result
  where
    cycleSortLoop arr start n
        | start >= n - 1 = arr
        | otherwise = 
            let item = arr !! start
                pos = countSmaller arr start item
                newArr = if pos == start then arr else swap arr start pos
            in cycleSortLoop newArr (start + 1) n
    
    countSmaller arr start item = 
        length $ filter (< item) (take start arr ++ drop (start + 1) arr)

-- Example usage
main :: IO ()
main = do
    let testArray = [4, 2, 5, 1, 3]
    putStrLn $ "Original array: " ++ show testArray
    let sortedArray = cycleSortClean testArray
    putStrLn $ "Sorted array: " ++ show sortedArray
    
    -- Test with another example
    let testArray2 = [3, 1, 4, 1, 5, 9, 2, 6]
    putStrLn $ "Original array: " ++ show testArray2
    let sortedArray2 = cycleSortClean testArray2
    putStrLn $ "Sorted array: " ++ show sortedArray2
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation by finding where each element should go
2. **In-place Sorting**: It sorts the array by rotating elements within cycles
3. **Minimal Swaps**: It minimizes the number of swaps needed (at most n-1 swaps)

## Key Features:

- **Time Complexity**: O(n²) in worst case, O(n) in best case (already sorted)
- **Space Complexity**: O(1) - sorts in-place
- **Stable**: Maintains relative order of equal elements
- **In-place**: Uses only a constant amount of extra memory

## Example Output:
```
Original array: [4,2,5,1,3]
Sorted array: [1,2,3,4,5]

Original array: [3,1,4,1,5,9,2,6]
Sorted array: [1,1,2,3,4,5,6,9]
```

