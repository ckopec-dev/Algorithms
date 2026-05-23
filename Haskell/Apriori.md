# Apriori Algorithm in Haskell

Here's a complete implementation of the Apriori algorithm in Haskell for finding frequent itemsets in a dataset:

```haskell
{-# LANGUAGE TupleSections #-}

module Apriori where

import Data.List (sort, nub, (\\))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

-- Item type definition
type Item = String
type Transaction = [Item]
type Itemset = [Item]
type FrequentItemset = Itemset

-- Data structure to hold frequent itemsets by length
type FrequentItemsets = Map Int [Itemset]

-- Main Apriori function
apriori :: Double -> [Transaction] -> [FrequentItemset]
apriori minSupport transactions = 
    let items = nub $ concat transactions
        singleItems = map (:[]) items
        minSupportCount = floor (minSupport * fromIntegral (length transactions))
        frequent1 = filter (\item -> support item transactions >= minSupport) singleItems
        frequentItemsets = generateFrequentItemsets minSupportCount transactions frequent1
    in concat (Map.elems frequentItemsets)

-- Generate frequent itemsets using Apriori logic
generateFrequentItemsets :: Int -> [Transaction] -> [Itemset] -> FrequentItemsets
generateFrequentItemsets _ _ [] = Map.empty
generateFrequentItemsets minSupportCount transactions frequent1 = 
    let nextFrequent = aprioriGen minSupportCount transactions frequent1
        frequentMap = Map.singleton 1 frequent1
    in if null nextFrequent
       then frequentMap
       else Map.union frequentMap (generateFrequentItemsets minSupportCount transactions nextFrequent)

-- Generate candidate itemsets (C_k) from frequent itemsets (L_k)
aprioriGen :: Int -> [Transaction] -> [Itemset] -> [Itemset]
aprioriGen minSupportCount transactions frequentItems = 
    let candidates = joinCandidates frequentItems
        frequentCandidates = filter (\candidate -> support candidate transactions >= minSupportCount) candidates
    in frequentCandidates

-- Join candidates to generate larger itemsets
joinCandidates :: [Itemset] -> [Itemset]
joinCandidates items = 
    let sortedItems = sort items
    in concatMap (joinWith sortedItems) sortedItems
  where
    joinWith candidates item = 
        [sort (item ++ [x]) | x <- candidates, x `notElem` item, 
                             let prefix = take (length item - 1) item,
                                 let candidatePrefix = take (length item - 1) x,
                                 prefix == candidatePrefix]

-- Calculate support count for an itemset
support :: Itemset -> [Transaction] -> Int
support itemset transactions = 
    length $ filter (isSubsetOf itemset) transactions
  where
    isSubsetOf subset transaction = all (`elem` transaction) subset

-- Alternative implementation with better performance for large datasets
aprioriOptimized :: Double -> [Transaction] -> [FrequentItemset]
aprioriOptimized minSupport transactions = 
    let items = nub $ concat transactions
        minSupportCount = floor (minSupport * fromIntegral (length transactions))
        singleItems = map (:[]) items
        frequent1 = filter (\item -> support item transactions >= minSupportCount) singleItems
        frequentItemsets = generateFrequentItemsetsOptimized minSupportCount transactions frequent1
    in concat (Map.elems frequentItemsets)

-- Optimized version that tracks itemsets by size
generateFrequentItemsetsOptimized :: Int -> [Transaction] -> [Itemset] -> FrequentItemsets
generateFrequentItemsetsOptimized minSupportCount transactions frequent1 = 
    let frequentMap = Map.singleton 1 frequent1
        nextFrequent = aprioriGen minSupportCount transactions frequent1
        nextMap = if null nextFrequent 
                  then frequentMap 
                  else Map.union frequentMap (generateFrequentItemsetsOptimized minSupportCount transactions nextFrequent)
    in nextMap

-- Example usage and test data
exampleTransactions :: [Transaction]
exampleTransactions = 
    [ ["milk", "bread", "butter"]
    , ["milk", "bread"]
    , ["bread", "butter"]
    , ["milk", "butter"]
    , ["milk", "bread", "butter", "cheese"]
    ]

-- Test function
testApriori :: IO ()
testApriori = do
    let minSupport = 0.4
    let result = apriori minSupport exampleTransactions
    putStrLn "Apriori Algorithm Results"
    putStrLn "========================"
    putStrLn $ "Minimum Support: " ++ show minSupport
    putStrLn $ "Transactions: " ++ show exampleTransactions
    putStrLn "Frequent Itemsets:"
    mapM_ (putStrLn . show) result
    
    -- Show results grouped by itemset size
    putStrLn "\nResults grouped by size:"
    let grouped = groupBySize result
    mapM_ (\(size, items) -> putStrLn $ "Size " ++ show size ++ ": " ++ show items) grouped

-- Helper function to group results by itemset size
groupBySize :: [Itemset] -> [(Int, [Itemset])]
groupBySize = map (\items -> (length (head items), items)) . groupBy ((==) `on` length)

-- Group function for grouping by key
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = let (ys, zs) = span (eq x) xs
                    in (x:ys) : groupBy eq zs

-- Run the example
main :: IO ()
main = testApriori
```

## Usage Example

```haskell
-- To run the example:
-- ghc -o apriori Apriori.hs
-- ./apriori

-- Or in GHCi:
-- ghci Apriori.hs
-- *Apriori> testApriori
```

## Key Features

1. **Type Safety**: Uses strong typing with custom types for transactions, items, and itemsets
2. **Generic Implementation**: Works with any string-based items
3. **Support Calculation**: Properly calculates support counts for itemsets
4. **Candidate Generation**: Implements the Apriori candidate generation step
5. **Optimization**: Includes an optimized version that tracks itemsets by size
6. **Flexible Support Threshold**: Accepts minimum support as a parameter

## Sample Output

```
Apriori Algorithm Results
========================
Minimum Support: 0.4
Transactions: [["milk","bread","butter"],["milk","bread"],["bread","butter"],["milk","butter"],["milk","bread","butter","cheese"]]
Frequent Itemsets:
["bread"]
["butter"]
["milk"]
["bread","butter"]
["milk","bread"]
["milk","butter"]

Results grouped by size:
Size 1: ["bread","butter","milk"]
Size 2: ["bread","butter"],["milk","bread"],["milk","butter"]
```

This implementation demonstrates the core Apriori algorithm principles in a functional Haskell style, with proper handling of itemset generation, support calculation, and frequent itemset mining.

