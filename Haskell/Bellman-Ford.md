# Bellman-Ford Algorithm in Haskell

Here's an implementation of the Bellman-Ford algorithm in Haskell to find shortest paths from a source vertex to all other vertices in a weighted graph:

```haskell
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Type aliases for clarity
type Vertex = Int
type Weight = Int
type Edge = (Vertex, Vertex, Weight)
type Graph = [Edge]

-- Bellman-Ford algorithm implementation
bellmanFord :: Graph -> Vertex -> Either String [(Vertex, Weight)]
bellmanFord edges source = 
    let vertices = nub (map (\(u, v, w) -> [u, v]) edges >>= id)
        n = length vertices
        -- Initialize distances
        distances = Map.fromList [(v, if v == source then 0 else 1000000) | v <- vertices]
    in if n > 0
        then 
            let distances' = foldl' (relax edges) distances [1..n-1]
            in if hasNegativeCycle edges distances'
                then Left "Graph contains negative cycle"
                else Right [(v, Map.findWithDefault 1000000 v distances') | v <- vertices]
        else Right []

-- Relax edges
relax :: Graph -> Map.Map Vertex Weight -> Int -> Map.Map Vertex Weight
relax edges distances _ = 
    foldl' updateDistance distances edges
  where
    updateDistance dist (u, v, w) = 
        let distU = Map.findWithDefault 1000000 u dist
            distV = Map.findWithDefault 1000000 v dist
        in if distU + w < distV
            then Map.insert v (distU + w) dist
            else dist

-- Check for negative cycles
hasNegativeCycle :: Graph -> Map.Map Vertex Weight -> Bool
hasNegativeCycle edges distances = 
    any (\(u, v, w) -> 
        let distU = Map.findWithDefault 1000000 u distances
            distV = Map.findWithDefault 1000000 v distances
        in distU + w < distV) edges

-- Helper function to get unique elements
nub :: Eq a => [a] -> [a]
nub = nubBy (==)
  where
    nubBy _ [] = []
    nubBy eq (x:xs) = x : nubBy eq (filter (not . eq x) xs)

-- Example usage
main :: IO ()
main = do
    -- Example graph: (from, to, weight)
    let graph = [
            (0, 1, -1),
            (0, 2, 4),
            (1, 2, 3),
            (1, 3, 2),
            (1, 4, 2),
            (3, 2, 5),
            (3, 1, 1),
            (4, 3, -3)
        ]
    
    putStrLn "Graph edges:"
    mapM_ print graph
    
    putStrLn "\nShortest paths from vertex 0:"
    case bellmanFord graph 0 of
        Left err -> putStrLn $ "Error: " ++ err
        Right distances -> do
            putStrLn "Vertex | Distance"
            putStrLn "-------|---------"
            mapM_ (\(v, d) -> putStrLn $ show v ++ "      | " ++ show d) distances
```

## How it works:

1. **Data Types**: 
   - `Edge` represents a weighted edge as `(from, to, weight)`
   - `Graph` is a list of edges

2. **Algorithm Steps**:
   - Initialize distances from source to all vertices (infinity except source = 0)
   - Relax all edges `|V|-1` times
   - Check for negative cycles
   - Return shortest distances or error if negative cycle exists

3. **Key Functions**:
   - `relax`: Updates distances by checking if a shorter path exists
   - `hasNegativeCycle`: Detects negative weight cycles
   - `bellmanFord`: Main algorithm function

## Sample Output:
```
Graph edges:
(0,1,-1)
(0,2,4)
(1,2,3)
(1,3,2)
(1,4,2)
(3,2,5)
(3,1,1)
(4,3,-3)

Shortest paths from vertex 0:
Vertex | Distance
-------|---------
0      | 0
1      | -1
2      | 2
3      | -2
4      | 1
```

This implementation handles negative edge weights and detects negative cycles, making it more robust than Dijkstra's algorithm for certain graph types.

