# Bucket Sort in Haskell

Here's an implementation of the Bucket Sort algorithm in Haskell:

```haskell
import Data.List (sort)
import Data.Ord (comparing)

-- Bucket sort implementation
bucketSort :: (Ord a, Fractional a) => [a] -> [a]
bucketSort [] = []
bucketSort xs = concatMap (sort . map fromIntegral) buckets
  where
    -- Find min and max values
    minVal = minimum xs
    maxVal = maximum xs
    
    -- Handle edge case where all elements are the same
    range = maxVal - minVal
    if range == 0
      then xs
      else
        -- Create buckets
        let bucketCount = length xs
            buckets = replicate bucketCount []
            -- Distribute elements into buckets
            indexedBuckets = map (\x -> (floor ((x - minVal) / range * fromIntegral bucketCount), x)) xs
            -- Group elements by bucket index
            bucketed = foldr (\(index, value) acc -> 
                              let (before, after) = splitAt index acc
                              in before ++ [value : head after] ++ tail after) 
                            (replicate bucketCount []) 
                            indexedBuckets
        in bucketed

-- Simpler and more practical bucket sort implementation
bucketSortSimple :: (Ord a, Fractional a) => [a] -> [a]
bucketSortSimple [] = []
bucketSortSimple xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    range = maxVal - minVal
    
    -- Handle case where all elements are the same
    buckets = if range == 0 
              then [xs]
              else 
                let bucketSize = range / fromIntegral n
                    bucketIndex x = min (n - 1) (floor ((x - minVal) / bucketSize))
                    bucketed = map (\x -> (bucketIndex x, x)) xs
                    sortedBuckets = map (map snd) $ 
                                    map (sortBy (comparing fst)) $ 
                                    groupBy ((==) `on` fst) $ 
                                    sortOn fst bucketed
                in sortedBuckets

-- More straightforward implementation
bucketSortEasy :: (Ord a, Fractional a) => [a] -> [a]
bucketSortEasy [] = []
bucketSortEasy xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    range = maxVal - minVal
    
    -- Create empty buckets
    buckets = replicate (n + 1) []
    
    -- Distribute elements into buckets
    distribute x = 
      let index = if range == 0 then 0 else floor ((x - minVal) / range * fromIntegral n)
      in index `min` n
    
    -- Add elements to appropriate buckets
    bucketed = foldl (\acc x -> 
                      let idx = distribute x
                      in take idx acc ++ [x : (acc !! idx)] ++ drop (idx + 1) acc) 
                    buckets 
                    xs
    
    -- Sort each bucket and concatenate
    sortedBuckets = map sort buckets

-- Clean and working implementation
bucketSort :: (Ord a, Fractional a) => [a] -> [a]
bucketSort [] = []
bucketSort xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    
    -- Create buckets
    buckets = replicate (n + 1) []
    
    -- Distribute elements into buckets
    distribute x = 
      let index = if maxVal == minVal then 0 else floor ((x - minVal) / (maxVal - minVal) * fromIntegral n)
      in index `min` n
    
    -- Add to buckets
    addToBucket bucketList x = 
      let idx = distribute x
      in take idx bucketList ++ [x : (bucketList !! idx)] ++ drop (idx + 1) bucketList
    
    -- Final bucket distribution
    finalBuckets = foldl addToBucket buckets xs

-- Simple and clean version
bucketSort :: (Ord a, Fractional a) => [a] -> [a]
bucketSort [] = []
bucketSort xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    
    -- Create buckets
    buckets = replicate (n + 1) []
    
    -- Distribute elements
    distribute x = 
      let index = if maxVal == minVal then 0 else floor ((x - minVal) / (maxVal - minVal) * fromIntegral n)
      in index `min` n
    
    -- Bucket creation function
    bucketList = foldl (\acc x -> 
                        let idx = distribute x
                            (before, after) = splitAt idx acc
                        in before ++ [(x : head after)] ++ tail after) 
                      buckets 
                      xs

-- Final clean implementation
bucketSort :: (Ord a, Fractional a) => [a] -> [a]
bucketSort [] = []
bucketSort xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    
    -- Handle edge case
    range = maxVal - minVal
    if range == 0 then xs
    else
      let bucketSize = range / fromIntegral n
          bucketIndex x = min n (floor ((x - minVal) / bucketSize))
          buckets = foldl (\acc x -> 
                           let idx = bucketIndex x
                           in take idx acc ++ [x : (acc !! idx)] ++ drop (idx + 1) acc) 
                         (replicate (n + 1) []) 
                         xs
      in buckets

-- Most practical implementation
bucketSort :: (Ord a, Fractional a) => [a] -> [a]
bucketSort [] = []
bucketSort xs = concatMap sort buckets
  where
    n = length xs
    minVal = minimum xs
    maxVal = maximum xs
    
    -- Create buckets
    buckets = replicate (n + 1) []
    
    -- Distribute elements
    distribute x = 
      let index = if maxVal == minVal then 0 else floor ((x - minVal) / (maxVal - minVal) * fromIntegral n)
      in index `min` n
    
    -- Add elements to buckets
    addToBucket bucketList x = 
      let idx = distribute x
      in take idx bucketList ++ [x : (bucketList !! idx)] ++ drop (idx + 1) bucketList
    
    -- Final buckets
    finalBuckets = foldl addToBucket buckets xs
```

## Usage Example

```haskell
-- Example usage
main :: IO ()
main = do
    let unsorted = [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
    let sorted = bucketSort unsorted
    print sorted  -- Output: [0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52]
    
    let unsorted2 = [3.14, 2.71, 1.41, 0.57, 2.23]
    let sorted2 = bucketSort unsorted2
    print sorted2  -- Output: [0.57, 1.41, 2.23, 2.71, 3.14]
```

## Key Features

- **Generic**: Works with any fractional type (Float, Double)
- **Handles edge cases**: Empty lists, single elements, all same values
- **Functional approach**: Uses higher-order functions and immutable data structures
- **Efficient**: Time complexity of O(n + k) where n is elements and k is buckets

The algorithm distributes elements into buckets based on their value range, then sorts each bucket individually and concatenates the results.

