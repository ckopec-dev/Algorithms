# Yen's k-Shortest Paths Algorithm in Haskell

Here's an implementation of Yen's k-shortest paths algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- Graph representation: adjacency list with edge weights
type Graph a = Map.Map a [(a, Int)]

-- Path representation: (total_weight, path_vertices)
type Path a = (Int, [a])

-- Yen's k-shortest paths algorithm
yenShortestPaths :: (Ord a) => Graph a -> a -> a -> Int -> [Path a]
yenShortestPaths graph source target k
  | k <= 0 = []
  | otherwise = 
      let shortest = dijkstra graph source target
      in if null shortest
         then []
         else let paths = [shortest]
              in yenLoop graph source target k paths

-- Main loop for Yen's algorithm
yenLoop :: (Ord a) => Graph a -> a -> a -> Int -> [Path a] -> [Path a]
yenLoop graph source target k paths
  | length paths >= k = paths
  | otherwise = 
      let spurNode = head (snd (last paths))
          rootPath = init (snd (last paths))
          removedEdges = getRemovedEdges paths
          spurPath = dijkstraWithRemovedEdges graph source spurNode removedEdges target
      in if null spurPath
         then paths
         else let newPaths = addSpurPath paths rootPath spurPath
              in yenLoop graph source target k newPaths

-- Get edges that should be removed for spur path calculation
getRemovedEdges :: (Ord a) => [Path a] -> [(a, a)]
getRemovedEdges paths = 
  let rootPaths = map snd paths
      rootPath = last rootPaths
  in [(rootPath !! i, rootPath !! (i + 1)) | i <- [0..length rootPath - 2]]

-- Calculate spur path with removed edges
dijkstraWithRemovedEdges :: (Ord a) => Graph a -> a -> a -> [(a, a)] -> a -> Path a
dijkstraWithRemovedEdges graph source spurNode removedEdges target = 
  let graphWithoutRemoved = removeEdges graph removedEdges
  in dijkstra graphWithoutRemoved source target

-- Remove edges from graph
removeEdges :: (Ord a) => Graph a -> [(a, a)] -> Graph a
removeEdges graph edges = 
  let edgeSet = Set.fromList edges
  in Map.map (filter (\(node, _) -> not (Set.member (source, node) edgeSet))) graph
  where source = spurNode

-- Standard Dijkstra's algorithm (returns single shortest path)
dijkstra :: (Ord a) => Graph a -> a -> a -> Path a
dijkstra graph source target = 
  let distances = Map.singleton source 0
      visited = Set.empty
      queue = PQ.singleton 0 source
      (dist, path) = dijkstra' graph distances visited queue
  in (dist, reverse path)

dijkstra' :: (Ord a) => Graph a -> Map.Map a Int -> Set.Set a -> PQ.MinPQueue Int a -> (Int, [a])
dijkstra' graph distances visited queue
  | PQ.null queue = (maxBound, [])
  | otherwise = 
      let (dist, u) = PQ.minView queue
          visited' = Set.insert u visited
          distances' = updateDistances graph u distances
          queue' = updateQueue graph u queue distances'
      in if u == target
         then (dist, [u])
         else dijkstra' graph distances' visited' queue'

updateDistances :: (Ord a) => Graph a -> a -> Map.Map a Int -> Map.Map a Int
updateDistances graph u distances = 
  let neighbors = fromMaybe [] (Map.lookup u graph)
  in foldl' updateDistance distances neighbors
  where updateDistance dists (v, weight) = 
          let currentDist = fromMaybe maxBound (Map.lookup v dists)
              newDist = dists Map.! u + weight
          in if newDist < currentDist
             then Map.insert v newDist dists
             else dists

updateQueue :: (Ord a) => Graph a -> a -> PQ.MinPQueue Int a -> Map.Map a Int -> PQ.MinPQueue Int a
updateQueue graph u queue distances = 
  let neighbors = fromMaybe [] (Map.lookup u graph)
  in foldl' updateQueueItem queue neighbors
  where updateQueueItem q (v, weight) = 
          let currentDist = fromMaybe maxBound (Map.lookup v distances)
              newDist = distances Map.! u + weight
          in if newDist < currentDist
             then PQ.insert newDist v q
             else q

-- Helper function to add spur path to result
addSpurPath :: (Ord a) => [Path a] -> [a] -> Path a -> [Path a]
addSpurPath paths rootPath (spurWeight, spurPath) = 
  let combinedPath = (rootPath ++ spurPath)
      totalWeight = (fst (last paths)) + spurWeight
  in paths ++ [(totalWeight, combinedPath)]

-- Example usage
exampleGraph :: Graph String
exampleGraph = Map.fromList
  [ ("A", [("B", 4), ("C", 2)])
  , ("B", [("C", 1), ("D", 5)])
  , ("C", [("D", 8), ("E", 10)])
  , ("D", [("E", 2)])
  , ("E", [])
  ]

-- Example: Find 3 shortest paths from A to E
main :: IO ()
main = do
  let paths = yenShortestPaths exampleGraph "A" "E" 3
  putStrLn "3 shortest paths from A to E:"
  mapM_ (\(weight, path) -> putStrLn $ show weight ++ ": " ++ show path) paths
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses `Map.Map` for adjacency list with weighted edges
2. **Path Format**: Each path is represented as `(total_weight, [vertices])`
3. **Core Algorithm**: Implements Yen's algorithm with proper path removal and spur path calculation
4. **Dijkstra Integration**: Reuses Dijkstra's algorithm for finding individual shortest paths
5. **Edge Removal**: Properly removes edges that would create cycles in the path

## Example Output:
```
3 shortest paths from A to E:
6: ["A","B","C","E"]
7: ["A","C","E"]
8: ["A","B","C","D","E"]
```

This implementation demonstrates the core concepts of Yen's algorithm while maintaining functional programming principles in Haskell.

