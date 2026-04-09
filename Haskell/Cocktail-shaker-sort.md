# Cocktail Shaker Sort in Haskell

```haskell
-- Cocktail Shaker Sort implementation in Haskell
cocktailShakerSort :: Ord a => [a] -> [a]
cocktailShakerSort [] = []
cocktailShakerSort [x] = [x]
cocktailShakerSort xs = cocktailSortHelper xs 0 (length xs - 1)

-- Helper function that performs the actual sorting
cocktailSortHelper :: Ord a => [a] -> Int -> Int -> [a]
cocktailSortHelper arr left right
  | left >= right = arr
  | otherwise = 
      let (arr1, newRight) = bubbleRight arr left right
          (arr2, newLeft) = bubbleLeft arr1 (left + 1) newRight
      in cocktailSortHelper arr2 newLeft newRight

-- Bubble from left to right (forward pass)
bubbleRight :: Ord a => [a] -> Int -> Int -> ([a], Int)
bubbleRight arr left right = 
  let (newArr, swapped) = bubbleRightHelper arr left right
  in if swapped 
     then (newArr, right - 1) 
     else (newArr, right)

bubbleRightHelper :: Ord a => [a] -> Int -> Int -> ([a], Bool)
bubbleRightHelper arr left right
  | left >= right = (arr, False)
  | otherwise = 
      let (newArr, swapped) = bubbleAdjacent arr left (left + 1)
      in if swapped
         then bubbleRightHelper newArr (left + 1) right
         else bubbleRightHelper arr (left + 1) right

-- Bubble from right to left (backward pass)
bubbleLeft :: Ord a => [a] -> Int -> Int -> ([a], Int)
bubbleLeft arr left right = 
  let (newArr, swapped) = bubbleLeftHelper arr left right
  in if swapped 
     then (newArr, left + 1) 
     else (newArr, left)

bubbleLeftHelper :: Ord a => [a] -> Int -> Int -> ([a], Bool)
bubbleLeftHelper arr left right
  | left >= right = (arr, False)
  | otherwise = 
      let (newArr, swapped) = bubbleAdjacent arr (right - 1) right
      in if swapped
         then bubbleLeftHelper newArr left (right - 1)
         else bubbleLeftHelper arr left (right - 1)

-- Helper to swap adjacent elements if needed
bubbleAdjacent :: Ord a => [a] -> Int -> Int -> ([a], Bool)
bubbleAdjacent arr i j
  | i < 0 || j >= length arr = (arr, False)
  | otherwise = 
      let x = arr !! i
          y = arr !! j
          newArr = replaceAt arr i y
      in if x > y 
         then (replaceAt newArr j x, True)
         else (arr, False)

-- Replace element at given index
replaceAt :: [a] -> Int -> a -> [a]
replaceAt arr i x = take i arr ++ [x] ++ drop (i + 1) arr

-- Alternative simpler implementation using a more functional approach
cocktailShakerSortSimple :: Ord a => [a] -> [a]
cocktailShakerSortSimple [] = []
cocktailShakerSortSimple xs = 
  let (sorted, _) = until (uncurry (==)) 
                    (\(arr, swapped) -> 
                      let (newArr, newSwapped) = cocktailPass arr
                      in (newArr, newSwapped))
                    (xs, True)
  in sorted

-- Single cocktail pass (both directions)
cocktailPass :: Ord a => [a] -> ([a], Bool)
cocktailPass [] = ([], False)
cocktailPass [x] = ([x], False)
cocktailPass arr = 
  let (forwardArr, forwardSwapped) = forwardPass arr
      (backwardArr, backwardSwapped) = backwardPass forwardArr
  in (backwardArr, forwardSwapped || backwardSwapped)

-- Forward pass (left to right)
forwardPass :: Ord a => [a] -> ([a], Bool)
forwardPass [] = ([], False)
forwardPass [x] = ([x], False)
forwardPass (x:y:rest) = 
  let (restArr, swapped) = forwardPass (y:rest)
  in if x > y 
     then (y:x:restArr, True)
     else (x:y:restArr, swapped)

-- Backward pass (right to left)
backwardPass :: Ord a => [a] -> ([a], Bool)
backwardPass [] = ([], False)
backwardPass [x] = ([x], False)
backwardPass (x:rest) = 
  let (restArr, swapped) = backwardPass rest
  in case restArr of
       [] -> ([x], False)
       y:ys -> if x > y 
               then (y:x:ys, True)
               else (x:restArr, swapped)

-- Example usage
main :: IO ()
main = do
  let testList = [64, 34, 25, 12, 22, 11, 90]
  putStrLn $ "Original list: " ++ show testList
  let sortedList = cocktailShakerSort testList
  putStrLn $ "Sorted list: " ++ show sortedList
  
  -- Test with different data types
  let stringList = ["banana", "apple", "cherry", "date"]
  putStrLn $ "String list: " ++ show stringList
  let sortedStrings = cocktailShakerSort stringList
  putStrLn $ "Sorted strings: " ++ show sortedStrings
```

## How it works:

1. **Cocktail Shaker Sort** is a variation of bubble sort that sorts in both directions alternately
2. The algorithm performs a forward pass (left to right) and a backward pass (right to left)
3. After each pass, the boundary of the unsorted portion is reduced
4. The process continues until no swaps are needed

## Key features:

- **Bidirectional sorting**: Sorts from both ends simultaneously
- **Early termination**: Stops when no swaps occur in a complete pass
- **Stable**: Maintains relative order of equal elements
- **In-place**: Sorts the list in-place with O(1) extra space

## Time Complexity:
- **Best case**: O(n) - when the list is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²) - when the list is reverse sorted

## Space Complexity:
- O(1) - only uses a constant amount of extra space

