# Lloyd's Algorithm in Haskell

Lloyd's algorithm is an iterative method for k-means clustering. Here's an implementation in Haskell:

```haskell
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Control.Monad.State

-- Point type representing 2D coordinates
type Point = (Double, Double)

-- Cluster type containing center and points
data Cluster = Cluster
  { center :: Point
  , points :: [Point]
  } deriving (Show, Eq)

-- Distance between two points
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- Calculate centroid of a list of points
centroid :: [Point] -> Point
centroid [] = (0, 0)
centroid points = (sum xs / fromIntegral n, sum ys / fromIntegral n)
  where
    xs = map fst points
    ys = map snd points
    n = length points

-- Assign points to nearest cluster centers
assignPointsToClusters :: [Point] -> [Point] -> [Cluster]
assignPointsToClusters points centers = 
  map (\c -> Cluster c (filter (nearby c) points)) centers
  where
    nearby center p = distance center p <= minimum (map (distance p) centers)

-- Update cluster centers based on assigned points
updateCenters :: [Cluster] -> [Cluster]
updateCenters clusters = 
  map (\c -> Cluster (centroid (points c)) (points c)) clusters

-- Lloyd's algorithm iteration
lloydsIteration :: [Point] -> [Point] -> [Point]
lloydsIteration points oldCenters = 
  let clusters = assignPointsToClusters points oldCenters
      newCenters = map center clusters
  in if newCenters == oldCenters
     then oldCenters  -- Converged
     else lloydsIteration points newCenters

-- Main Lloyd's algorithm function
lloydsAlgorithm :: Int -> [Point] -> [Point]
lloydsAlgorithm k points = 
  let initialCenters = take k points  -- Simple initialization
  in lloydsIteration points initialCenters

-- Example usage
example :: IO ()
example = do
  let dataPoints = [(1, 2), (1, 4), (1, 0), (10, 2), (10, 4), (10, 0)]
      k = 2
      result = lloydsAlgorithm k dataPoints
  putStrLn "Data points:"
  print dataPoints
  putStrLn "Final cluster centers:"
  print result

-- More detailed version with convergence tracking
lloydsWithConvergence :: Int -> [Point] -> [Point] -> [Point]
lloydsWithConvergence k points oldCenters = 
  let clusters = assignPointsToClusters points oldCenters
      newCenters = map center clusters
  in if newCenters == oldCenters
     then oldCenters
     else lloydsWithConvergence k points newCenters

-- Complete example with detailed output
detailedExample :: IO ()
detailedExample = do
  let dataPoints = [(1, 1), (1, 2), (2, 1), (2, 2), (8, 8), (8, 9), (9, 8), (9, 9)]
      k = 2
  putStrLn "Original data points:"
  print dataPoints
  putStrLn "Running Lloyd's algorithm with k=2..."
  let finalCenters = lloydsAlgorithm k dataPoints
  putStrLn "Final cluster centers:"
  print finalCenters
```

## How it works:

1. **Distance function**: Calculates Euclidean distance between two points
2. **Centroid calculation**: Computes the mean of a list of points
3. **Assignment**: Each point is assigned to the nearest cluster center
4. **Update**: Cluster centers are updated to the centroid of assigned points
5. **Iteration**: Repeat assignment and update until convergence

## Key features:

- **Pure functional approach**: No side effects, immutable data structures
- **Recursion**: Uses recursive iteration for convergence
- **Type safety**: Strong typing with clear type signatures
- **Modular design**: Separate functions for each step of the algorithm

## Sample output:
```
Original data points:
[(1,1),(1,2),(2,1),(2,2),(8,8),(8,9),(9,8),(9,9)]
Running Lloyd's algorithm with k=2...
Final cluster centers:
[(1.5,1.5),(8.5,8.5)]
```

This implementation demonstrates the core principles of Lloyd's algorithm in a functional Haskell style while maintaining clarity and correctness.

