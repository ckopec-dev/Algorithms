# K-Medoids Clustering in Haskell

Here's an implementation of the K-Medoids clustering algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module KMedoids where

import Data.List (minimumBy, foldl', groupBy, sortOn)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Point type representing n-dimensional coordinates
type Point = [Double]

-- Cluster type
type Cluster = ([Point], Point)  -- (points, medoid)

-- Distance function (Euclidean distance)
distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ sum $ zipWith (\x y -> (x - y) ** 2) p1 p2

-- Calculate total distance from all points to their medoid
totalDistance :: [Point] -> Point -> Double
totalDistance points medoid = sum $ map (distance medoid) points

-- Find the medoid for a cluster (point that minimizes total distance)
findMedoid :: [Point] -> Point
findMedoid points
  | null points = []
  | otherwise = minimumBy (comparing totalDist) points
  where
    totalDist p = totalDistance points p

-- Initialize k random medoids from the dataset
initializeMedoids :: [Point] -> Int -> IO [Point]
initializeMedoids points k = do
  indices <- replicateM k $ randomRIO (0, length points - 1)
  return [points !! i | i <- indices]

-- Assign points to the nearest medoid
assignPointsToMedoids :: [Point] -> [Point] -> [Cluster]
assignPointsToMedoids points medoids = 
  let clusters = map (flip map points . assignPointToMedoid) medoids
      medoidPoints = zipWith (,) clusters medoids
  in map (\(pts, med) -> (pts, med)) medoidPoints

assignPointToMedoid :: Point -> Point -> Point
assignPointToMedoid point medoid = 
  if null medoid then point
  else let distances = map (distance point) medoid
       in head $ filter (\m -> distance point m == minimum distances) medoid

-- Update medoids by finding the point that minimizes total distance in each cluster
updateMedoids :: [Cluster] -> [Cluster]
updateMedoids clusters = map updateCluster clusters
  where
    updateCluster (points, _) = 
      let newMedoid = findMedoid points
      in (points, newMedoid)

-- K-Medoids clustering algorithm
kMedoids :: [Point] -> Int -> Int -> IO [Cluster]
kMedoids points k maxIterations = do
  -- Initialize random medoids
  medoids <- initializeMedoids points k
  let clusters = assignPointsToMedoids points medoids
  
  -- Iterate until convergence or max iterations
  let finalClusters = kMedoidsIterate points clusters maxIterations
  return finalClusters

kMedoidsIterate :: [Point] -> [Cluster] -> Int -> [Cluster]
kMedoidsIterate points clusters maxIter = 
  let newClusters = updateMedoids clusters
      assignments = assignPointsToMedoids points (map snd newClusters)
      newClusters' = updateMedoids assignments
  in if maxIter <= 0 || clusters == newClusters'
     then newClusters
     else kMedoidsIterate points newClusters' (maxIter - 1)

-- Example usage
example :: IO ()
example = do
  -- Sample 2D data points
  let dataPoints = [[1.0, 2.0], [1.5, 1.8], [5.0, 8.0], [8.0, 8.0], 
                   [1.0, 0.6], [9.0, 11.0], [8.0, 2.0], [10.0, 2.0], 
                   [9.0, 3.0]]
  
  putStrLn "Data points:"
  mapM_ print dataPoints
  
  putStrLn "\nRunning K-Medoids clustering (k=3):"
  clusters <- kMedoids dataPoints 3 100
  
  putStrLn "Final clusters:"
  mapM_ (\(points, medoid) -> do
    putStrLn $ "Medoid: " ++ show medoid
    putStrLn $ "Points: " ++ show points
    putStrLn "") clusters

-- Alternative simpler implementation for small datasets
simpleKMedoids :: [Point] -> Int -> [Cluster]
simpleKMedoids points k = 
  let medoids = take k points  -- Simple approach: first k points as medoids
      clusters = assignPointsToMedoids points medoids
      updatedClusters = updateMedoids clusters
  in updatedClusters

-- Helper function to print clusters nicely
printClusters :: [Cluster] -> IO ()
printClusters clusters = do
  putStrLn "Clustering Results:"
  putStrLn "=================="
  mapM_ (\(points, medoid) -> do
    putStrLn $ "Medoid: " ++ show medoid
    putStrLn $ "Points: " ++ show points
    putStrLn "") clusters
```

## Usage Example

```haskell
-- To run the example:
main :: IO ()
main = do
  let dataPoints = [[1.0, 2.0], [1.5, 1.8], [5.0, 8.0], [8.0, 8.0], 
                   [1.0, 0.6], [9.0, 11.0], [8.0, 2.0], [10.0, 2.0], 
                   [9.0, 3.0]]
  
  putStrLn "Input data points:"
  mapM_ print dataPoints
  
  clusters <- kMedoids dataPoints 3 100
  printClusters clusters
```

## Key Features

1. **Point Representation**: Uses lists of `Double` for n-dimensional points
2. **Distance Calculation**: Implements Euclidean distance
3. **Medoid Selection**: Finds the point within each cluster that minimizes total distance
4. **Convergence**: Iteratively updates clusters until convergence
5. **Random Initialization**: Randomly selects initial medoids
6. **Flexible Input**: Works with any number of dimensions

## Algorithm Steps

1. Initialize k random medoids
2. Assign each point to the nearest medoid
3. For each cluster, find the point that minimizes total distance to all points in the cluster
4. Update medoids to these new points
5. Repeat until convergence or maximum iterations reached

This implementation demonstrates the core concepts of K-Medoids clustering in a functional Haskell style.

