# PageRank Algorithm in Haskell

Here's a complete implementation of the PageRank algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module PageRank where

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Ord (Down(..))

-- PageRank data types
type PageId = String
type PageRank = Double
type Link = (PageId, PageId)  -- (from, to)

-- PageRank state
data PageRankState = PageRankState
    { pages :: Map PageId PageRank
    , links :: [Link]
    , damping :: Double
    , tolerance :: Double
    } deriving (Show)

-- Create initial PageRank state
initialPageRankState :: [PageId] -> [Link] -> PageRankState
initialPageRankState pageIds linkList = PageRankState
    { pages = Map.fromList [(p, 1.0 / fromIntegral (length pageIds)) | p <- pageIds]
    , links = linkList
    , damping = 0.85
    , tolerance = 1e-6
    }

-- Build adjacency list from links
buildAdjacencyList :: [Link] -> Map PageId [PageId]
buildAdjacencyList links = 
    let grouped = Map.fromListWith (++) [(from, [to]) | (from, to) <- links]
    in Map.map (sortOn id) grouped

-- Calculate out-degree for each page
outDegrees :: Map PageId [PageId] -> Map PageId Int
outDegrees adjList = Map.map length adjList

-- Compute PageRank for one iteration
computePageRankStep :: PageRankState -> PageRankState
computePageRankStep state = 
    let adjList = buildAdjacencyList (links state)
        outD = outDegrees adjList
        newPages = Map.mapWithKey (computePageRankForPage adjList outD (damping state)) (pages state)
    in state { pages = newPages }

-- Compute PageRank for a single page
computePageRankForPage :: Map PageId [PageId] 
                       -> Map PageId Int
                       -> Double
                       -> PageId
                       -> PageRank
                       -> PageRank
computePageRankForPage adjList outD dampingFactor page currentRank = 
    let fromPages = Map.keys (Map.filter (page `elem`) (Map.map (filter (== page)) adjList))
        fromPagesCount = length fromPages
        outDegree = fromMaybe 0 (Map.lookup page outD)
        contribution = if outDegree > 0 
                      then currentRank / fromIntegral outDegree
                      else 0
        sumContributions = sum [contribution | _ <- fromPages]
        dangling = 1.0 - dampingFactor
    in dampingFactor * sumContributions + dangling / fromIntegral (Map.size (pages state))

-- Alternative simpler implementation
computePageRankSimple :: [PageId] -> [Link] -> Double -> Int -> Map PageId PageRank
computePageRankSimple pageIds linkList dampingFactor iterations = 
    let initial = Map.fromList [(p, 1.0 / fromIntegral (length pageIds)) | p <- pageIds]
        adjList = buildAdjacencyList linkList
        outD = outDegrees adjList
        finalState = iterate (computeStep adjList outD dampingFactor) initial !! iterations
    in finalState

-- Compute one step of PageRank
computeStep :: Map PageId [PageId] -> Map PageId Int -> Double -> Map PageId PageRank -> Map PageId PageRank
computeStep adjList outD dampingFactor oldRanks = 
    let n = Map.size oldRanks
        dangling = 1.0 - dampingFactor
        newRanks = Map.mapWithKey (computeRank adjList outD dampingFactor oldRanks) oldRanks
    in newRanks

-- Compute rank for a specific page
computeRank :: Map PageId [PageId] 
            -> Map PageId Int
            -> Double
            -> Map PageId PageRank
            -> PageId
            -> PageRank
            -> PageRank
computeRank adjList outD dampingFactor oldRanks page oldRank = 
    let fromPages = Map.keys (Map.filter (page `elem`) (Map.map (filter (== page)) adjList))
        contributions = map (\fromPage -> 
            let outDegree = fromMaybe 0 (Map.lookup fromPage outD)
            in if outDegree > 0 
               then oldRanks Map.! fromPage / fromIntegral outDegree
               else 0) fromPages
        sumContributions = sum contributions
    in dampingFactor * sumContributions + (1.0 - dampingFactor) / fromIntegral (Map.size oldRanks)

-- More practical implementation with convergence checking
pageRankConverge :: [PageId] -> [Link] -> Double -> Double -> Map PageId PageRank
pageRankConverge pageIds linkList dampingFactor tolerance = 
    let initial = Map.fromList [(p, 1.0 / fromIntegral (length pageIds)) | p <- pageIds]
        adjList = buildAdjacencyList linkList
        outD = outDegrees adjList
        finalRanks = pageRankIterate adjList outD dampingFactor initial tolerance
    in finalRanks

pageRankIterate :: Map PageId [PageId] -> Map PageId Int -> Double -> Map PageId PageRank -> Double -> Map PageId PageRank
pageRankIterate adjList outD dampingFactor oldRanks tolerance = 
    let newRanks = Map.mapWithKey (computeRank adjList outD dampingFactor oldRanks) oldRanks
        diff = Map.foldWithKey (\k v acc -> abs (v - oldRanks Map.! k) + acc) 0 newRanks
    in if diff < tolerance 
       then newRanks
       else pageRankIterate adjList outD dampingFactor newRanks tolerance

-- Example usage
example :: IO ()
example = do
    let pages = ["A", "B", "C", "D"]
    let links = [("A", "B"), ("A", "C"), ("B", "C"), ("C", "D"), ("D", "A"), ("D", "B")]
    
    putStrLn "PageRank calculation example:"
    putStrLn $ "Pages: " ++ show pages
    putStrLn $ "Links: " ++ show links
    
    let result = pageRankConverge pages links 0.85 1e-6
    
    putStrLn "Final PageRanks:"
    let sortedRanks = sortOn (Down . snd) (Map.toList result)
    mapM_ (\(page, rank) -> putStrLn $ page ++ ": " ++ show rank) sortedRanks

-- Helper function to get value from Map or default
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Run the example
main :: IO ()
main = example
```

## Usage Example

```haskell
-- Simple usage example
main :: IO ()
main = do
    let pages = ["A", "B", "C", "D"]
    let links = [("A", "B"), ("A", "C"), ("B", "C"), ("C", "D"), ("D", "A"), ("D", "B")]
    
    let result = pageRankConverge pages links 0.85 1e-6
    
    -- Print results
    mapM_ (\(page, rank) -> putStrLn $ page ++ ": " ++ show rank) 
          (sortOn (Down . snd) (Map.toList result))
```

## Key Features

1. **Data Types**: 
   - `PageId` for page identifiers
   - `PageRank` for rank values
   - `Link` for page connections

2. **Core Functions**:
   - `initialPageRankState`: Initialize the PageRank state
   - `computePageRankStep`: Perform one iteration
   - `pageRankConverge`: Run until convergence
   - `buildAdjacencyList`: Convert links to adjacency representation

3. **Algorithm Details**:
   - Implements the standard PageRank formula
   - Uses damping factor (typically 0.85)
   - Handles convergence checking
   - Supports multiple iterations

This implementation demonstrates the mathematical foundation of PageRank while leveraging Haskell's functional programming features for clean, maintainable code.

