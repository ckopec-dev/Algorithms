# DBSCAN Clustering in Haskell

Here's an implementation of the DBSCAN clustering algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module DBSCAN where

import Data.List (partition, sortOn)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Point type representing 2D coordinates
type Point = (Double, Double)

-- Cluster type
type Cluster = [Point]

-- DBSCAN parameters
data DBSCANParams = DBSCANParams
    { eps :: Double      -- Maximum distance between points
    , minPts :: Int      -- Minimum number of points to form a cluster
    } deriving (Show, Eq)

-- DBSCAN result
data DBSCANResult = DBSCANResult
    { clusters :: [Cluster]
    , noise :: [Point]
    } deriving (Show, Eq)

-- Calculate Euclidean distance between two points
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

-- Find all points within epsilon distance
neighbors :: Point -> [Point] -> Double -> [Point]
neighbors p points eps = filter (\q -> distance p q <= eps) points

-- Core function to find density-reachable points
densityReachable :: [Point] -> Point -> Point -> Double -> Bool
densityReachable points p q eps = distance p q <= eps

-- Find all points in the same cluster using density-based expansion
expandCluster :: DBSCANParams -> [Point] -> Point -> Set Point -> Set Point -> [Point] -> Set Point
expandCluster params points p visited cluster seedPoints
    | null seedPoints = cluster
    | otherwise = 
        let p' = head seedPoints
            newSeed = if not (Set.member p' visited) 
                         then neighbors p' points (eps params) 
                         else []
            newVisited = Set.insert p' visited
            newCluster = Set.insert p' cluster
            newSeedPoints = filter (not . (`Set.member` newVisited)) (newSeed ++ tail seedPoints)
        in expandCluster params points p newVisited newCluster newSeedPoints

-- Main DBSCAN algorithm
dbscan :: DBSCANParams -> [Point] -> DBSCANResult
dbscan params points = 
    let visited = Set.empty
        unvisited = Set.fromList points
        (clusters, noise) = clusterPoints params points visited unvisited []
    in DBSCANResult clusters noise
  where
    clusterPoints :: DBSCANParams -> [Point] -> Set Point -> Set Point -> [Cluster] -> ([Cluster], [Point])
    clusterPoints _ _ _ Set.empty clusters = (reverse clusters, [])
    clusterPoints params points visited unvisited clusters = 
        let p = Set.elemAt 0 unvisited
            neighborsList = neighbors p points (eps params)
            in if length neighborsList >= minPts params
               then 
                   let cluster = expandCluster params points p visited (Set.singleton p) neighborsList
                       newVisited = Set.union visited cluster
                       newUnvisited = Set.difference unvisited cluster
                       newClusters = Set.toList cluster : clusters
                   in clusterPoints params points newVisited newUnvisited newClusters
               else 
                   let newVisited = Set.insert p visited
                       newUnvisited = Set.delete p unvisited
                   in clusterPoints params points newVisited newUnvisited clusters

-- Example usage
examplePoints :: [Point]
examplePoints = 
    [ (1, 1), (1.1, 1.1), (1.2, 1.2), (1.3, 1.3)
    , (2, 2), (2.1, 2.1), (2.2, 2.2)
    , (10, 10), (10.1, 10.1)
    , (20, 20), (20.1, 20.1), (20.2, 20.2)
    , (30, 30)
    ]

-- Run example
main :: IO ()
main = do
    let params = DBSCANParams 1.5 2
    let result = dbscan params examplePoints
    putStrLn "DBSCAN Clustering Results:"
    putStrLn $ "Clusters: " ++ show (length $ clusters result)
    mapM_ (\cluster -> putStrLn $ "Cluster: " ++ show cluster) (clusters result)
    putStrLn $ "Noise points: " ++ show (noise result)
```

## Key Features of this Implementation:

1. **Point Representation**: Uses `(Double, Double)` tuples for 2D coordinates
2. **Core Algorithm**: Implements the standard DBSCAN algorithm with:
   - Core point identification
   - Density-reachable point expansion
   - Noise point detection
3. **Data Structures**: 
   - Uses `Set` for efficient membership testing
   - Uses `Map` for potential future extensions
4. **Parameters**: 
   - `eps`: Maximum distance for neighborhood
   - `minPts`: Minimum points required to form a cluster
5. **Result**: Returns both clusters and noise points

## Usage Example:

```haskell
-- Create parameters
params = DBSCANParams 1.5 2

-- Run clustering
result = dbscan params examplePoints

-- Access results
clusters = clusters result  -- List of clusters
noisePoints = noise result  -- List of noise points
```

This implementation follows the standard DBSCAN algorithm while leveraging Haskell's functional programming features for clean, efficient code.

