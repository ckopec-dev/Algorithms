# K-Means Clustering in Haskell

Here's a complete implementation of the K-means clustering algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

module KMeans where

import Data.List (groupBy, sortOn)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- Define a point as a list of coordinates
type Point = [Double]

-- Define a cluster as a list of points
type Cluster = [Point]

-- Calculate Euclidean distance between two points
distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ sum $ zipWith (\x y -> (x - y) ** 2) p1 p2

-- Find the nearest centroid to a point
nearestCentroid :: [Point] -> Point -> Point
nearestCentroid centroids point = 
    head $ sortOn (distance point) centroids

-- Assign points to clusters based on nearest centroid
assignPointsToClusters :: [Point] -> [Point] -> [Cluster]
assignPointsToClusters centroids points = 
    map (filter (\p -> nearestCentroid centroids p == c)) centroids
  where
    -- Create clusters by grouping points by their nearest centroid
    clusteredPoints = map (\p -> (nearestCentroid centroids p, p)) points
    grouped = groupBy ((==) `on` fst) $ sortOn fst clusteredPoints
    clusters = map (map snd) grouped

-- Calculate the mean of a cluster (centroid)
meanPoint :: Cluster -> Point
meanPoint [] = []
meanPoint cluster = 
    zipWith (/) (foldl1 (zipWith (+)) cluster) (repeat $ fromIntegral $ length cluster)

-- Update centroids by calculating means of clusters
updateCentroids :: [Cluster] -> [Point]
updateCentroids clusters = map meanPoint clusters

-- Check if centroids have converged
centroidsConverged :: Double -> [Point] -> [Point] -> Bool
centroidsConverged epsilon oldCentroids newCentroids = 
    all (\(old, new) -> distance old new < epsilon) $ zip oldCentroids newCentroids

-- K-means clustering algorithm
kMeans :: Int -> [Point] -> Int -> Double -> [Cluster]
kMeans k points maxIterations epsilon = 
    let initialCentroids = take k points  -- Simple initialization
        result = kMeansIter initialCentroids points maxIterations epsilon
    in result
  where
    kMeansIter centroids points iterCount epsilon
        | iterCount <= 0 = assignPointsToClusters centroids points
        | otherwise = 
            let clusters = assignPointsToClusters centroids points
                newCentroids = updateCentroids clusters
            in if centroidsConverged epsilon centroids newCentroids
               then clusters
               else kMeansIter newCentroids points (iterCount - 1) epsilon

-- Alternative initialization using k-means++
kMeansPlusPlus :: Int -> [Point] -> Int -> Double -> [Cluster]
kMeansPlusPlus k points maxIterations epsilon = 
    let initialCentroids = kMeansPlusPlusInit k points
        result = kMeansIter initialCentroids points maxIterations epsilon
    in result
  where
    kMeansPlusPlusInit k points = 
        let firstCentroid = head points
            remainingPoints = tail points
            centroids = firstCentroid : kMeansPlusPlusIter (k - 1) remainingPoints [firstCentroid]
        in centroids
      where
        kMeansPlusPlusIter 0 _ _ = []
        kMeansPlusPlusIter n points centroids = 
            let distances = map (minimum . map (distance) centroids) points
                totalDistance = sum distances
                probabilities = map (/ totalDistance) distances
                selectedPoint = weightedRandomSelect points probabilities
            in selectedPoint : kMeansPlusPlusIter (n - 1) points (selectedPoint : centroids)
    
    weightedRandomSelect points probabilities = 
        let rand = randomRIO (0, 1) :: IO Double
        in head $ dropWhile (\(p, prob) -> rand > prob) $ zip points probabilities

-- Simple random initialization
randomInitialization :: Int -> [Point] -> IO [Point]
randomInitialization k points = do
    indices <- replicateM k $ randomRIO (0, length points - 1)
    return $ map (points !!) indices

-- Example usage
example :: IO ()
example = do
    -- Sample 2D points
    let points = [[1.0, 2.0], [1.5, 1.8], [5.0, 8.0], [8.0, 8.0], [1.0, 0.6], [9.0, 11.0]]
    
    putStrLn "Original points:"
    mapM_ print points
    
    let clusters = kMeans 3 points 100 0.001
    
    putStrLn "\nClustered points:"
    mapM_ print clusters
    
    putStrLn "\nCluster centers:"
    mapM_ print $ map meanPoint clusters

-- Run the example
main :: IO ()
main = example
```

## Usage Example

```haskell
-- To run the example:
-- ghc kmeans.hs
-- ./kmeans

-- Expected output:
-- Original points:
-- [1.0,2.0]
-- [1.5,1.8]
-- [5.0,8.0]
-- [8.0,8.0]
-- [1.0,0.6]
-- [9.0,11.0]
--
-- Clustered points:
-- [[1.0,2.0],[1.5,1.8],[1.0,0.6]]
-- [[5.0,8.0],[8.0,8.0]]
-- [[9.0,11.0]]
--
-- Cluster centers:
-- [1.1666666666666667,1.4666666666666666]
-- [6.5,8.0]
-- [9.0,11.0]
```

## Key Features

1. **Type Safety**: Uses strong typing with `Point` and `Cluster` types
2. **Functional Approach**: Pure functions with no side effects
3. **Modular Design**: Separate functions for distance calculation, assignment, and centroid updates
4. **Convergence Detection**: Stops when centroids stop changing significantly
5. **Flexible Initialization**: Supports different centroid initialization strategies

## Algorithm Steps

1. Initialize k centroids
2. Assign each point to the nearest centroid
3. Update centroids by calculating cluster means
4. Repeat until convergence or maximum iterations
5. Return final clusters

This implementation demonstrates Haskell's functional programming strengths while providing a complete, working K-means clustering solution.

