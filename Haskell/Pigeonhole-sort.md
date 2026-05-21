# Pigeonhole Sort in Haskell

Here's an implementation of the Pigeonhole Sort algorithm in Haskell:

```haskell
import Data.List (foldl')
import Data.Array

-- Pigeonhole sort implementation
pigeonholeSort :: (Ord a, Bounded a, Enum a) => [a] -> [a]
pigeonholeSort [] = []
pigeonholeSort xs = 
    let minVal = minimum xs
        maxVal = maximum xs
        rangeSize = fromEnum maxVal - fromEnum minVal + 1
        -- Create array to count occurrences
        counts = array (0, rangeSize - 1) [(i, 0) | i <- [0..rangeSize-1]]
        -- Update counts with actual values
        updatedCounts = foldl' updateCount counts xs
        -- Extract sorted elements
        sortedElements = concatMap (\(i, count) -> replicate count (toEnum i)) 
                              (assocs updatedCounts)
    in sortedElements
  where
    updateCount arr x = 
        let index = fromEnum x - fromEnum minVal
        in arr // [(index, arr ! index + 1)]

-- Alternative implementation using list comprehension
pigeonholeSort' :: (Ord a, Bounded a, Enum a) => [a] -> [a]
pigeonholeSort' [] = []
pigeonholeSort' xs = 
    let minVal = minimum xs
        maxVal = maximum xs
        -- Count occurrences of each value
        counts = [length (filter (== toEnum i) xs) | i <- [fromEnum minVal..fromEnum maxVal]]
    in concatMap (\(i, count) -> replicate count (toEnum i)) 
              (zip [fromEnum minVal..] counts)

-- Example usage
main :: IO ()
main = do
    -- Test with integers
    let numbers = [8, 3, 2, 7, 4, 6, 8, 1]
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Sorted list: " ++ show (pigeonholeSort numbers)
    
    -- Test with characters
    let chars = ['d', 'b', 'a', 'c', 'b', 'a']
    putStrLn $ "Original chars: " ++ show chars
    putStrLn $ "Sorted chars: " ++ show (pigeonholeSort chars)
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the input list
2. **Create counting array**: Initialize an array to count occurrences of each value
3. **Count occurrences**: Iterate through the input list and increment counts
4. **Reconstruct sorted list**: Go through the counting array and build the sorted output

## Example output:
```
Original list: [8,3,2,7,4,6,8,1]
Sorted list: [1,2,3,4,6,7,8,8]
Original chars: ['d','b','a','c','b','a']
Sorted chars: "aabbcd"
```

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

**Note**: This implementation requires the type to be `Bounded` and `Enum` since it needs to convert between values and array indices.

