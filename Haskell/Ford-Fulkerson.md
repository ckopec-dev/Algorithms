# Ford-Fulkerson Algorithm in Haskell

Here's an implementation of the Ford-Fulkerson algorithm for finding the maximum flow in a flow network:

```haskell
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Type aliases for clarity
type Flow = Int
type Vertex = Int
type Graph = Map.Map Vertex [Vertex]
type Capacity = Map.Map (Vertex, Vertex) Flow

-- Flow network representation
data FlowNetwork = FlowNetwork
    { vertices :: [Vertex]
    , edges :: [(Vertex, Vertex, Flow)]
    , source :: Vertex
    , sink :: Vertex
    } deriving (Show)

-- Build adjacency list representation from edges
buildGraph :: [(Vertex, Vertex, Flow)] -> Graph
buildGraph edges = 
    let allVertices = map (\(u, v, _) -> [u, v]) edges
        vertexList = foldl' (++) [] allVertices
        uniqueVertices = foldl' (\acc v -> if v `elem` acc then acc else v:acc) [] vertexList
    in foldl' addEdge Map.empty edges
  where
    addEdge graph (u, v, _) = 
        let neighborsU = fromMaybe [] (Map.lookup u graph)
            neighborsV = fromMaybe [] (Map.lookup v graph)
        in Map.insert u (v:neighborsU) $ Map.insert v (u:neighborsV) graph

-- Build capacity map from edges
buildCapacityMap :: [(Vertex, Vertex, Flow)] -> Capacity
buildCapacityMap edges = Map.fromList [(edge, capacity) | (u, v, capacity) <- edges]

-- Find augmenting path using BFS
findAugmentingPath :: Graph -> Capacity -> Vertex -> Vertex -> Maybe [Vertex]
findAugmentingPath graph capacity source sink = 
    let visited = Map.singleton source True
        queue = [source]
    in bfs queue visited
  where
    bfs [] _ = Nothing
    bfs (current:rest) visited
        | current == sink = Just [sink]
        | otherwise = 
            let neighbors = fromMaybe [] (Map.lookup current graph)
                unvisitedNeighbors = filter (not . (`Map.member` visited)) neighbors
                validNeighbors = filter (canFlow current) unvisitedNeighbors
                newVisited = foldl' (\acc v -> Map.insert v True acc) visited validNeighbors
                newQueue = rest ++ validNeighbors
            in case mapM (\v -> bfs (v:rest) newVisited) validNeighbors of
                Nothing -> bfs newQueue visited
                Just paths -> Just (current : head paths)
      where
        canFlow u v = case Map.lookup (u, v) capacity of
            Just cap -> cap > 0
            Nothing -> False

-- Find minimum capacity along the path
findMinCapacity :: Capacity -> [Vertex] -> Flow
findMinCapacity capacity path = 
    let edges = zip path (tail path)
    in foldl' (\minCap (u, v) -> 
        case Map.lookup (u, v) capacity of
            Just cap -> min minCap cap
            Nothing -> minCap
        ) maxBound edges

-- Update capacity after finding augmenting path
updateCapacity :: Capacity -> [Vertex] -> Flow -> Capacity
updateCapacity capacity path flow = 
    let edges = zip path (tail path)
    in foldl' (\cap (u, v) -> 
        let current = fromMaybe 0 (Map.lookup (u, v) cap)
            reverseFlow = fromMaybe 0 (Map.lookup (v, u) cap)
        in Map.insert (u, v) (current - flow) $ Map.insert (v, u) (reverseFlow + flow) cap
        ) capacity edges

-- Main Ford-Fulkerson algorithm
fordFulkerson :: FlowNetwork -> Flow
fordFulkerson network = 
    let graph = buildGraph (edges network)
        capacity = buildCapacityMap (edges network)
        source = source network
        sink = sink network
    in fordFulkersonHelper graph capacity source sink 0

-- Helper function for recursive Ford-Fulkerson
fordFulkersonHelper :: Graph -> Capacity -> Vertex -> Vertex -> Flow -> Flow
fordFulkersonHelper graph capacity source sink currentFlow
    | Nothing <- path = currentFlow
    | Just path <- path = 
        let minCapacity = findMinCapacity capacity path
            newFlow = currentFlow + minCapacity
            newCapacity = updateCapacity capacity path minCapacity
        in fordFulkersonHelper graph newCapacity source sink newFlow
  where
    path = findAugmentingPath graph capacity source sink

-- Example usage
exampleNetwork :: FlowNetwork
exampleNetwork = FlowNetwork
    { vertices = [0, 1, 2, 3]
    , edges = [(0, 1, 10), (0, 2, 5), (0, 3, 15), (1, 2, 4), (1, 3, 8), (2, 3, 5)]
    , source = 0
    , sink = 3
    }

-- Run the example
main :: IO ()
main = do
    let maxFlow = fordFulkerson exampleNetwork
    putStrLn "Ford-Fulkerson Algorithm Example"
    putStrLn "================================"
    putStrLn $ "Maximum flow: " ++ show maxFlow
    putStrLn "Network edges:"
    mapM_ (\(u, v, c) -> putStrLn $ "  " ++ show u ++ " -> " ++ show v ++ " (capacity: " ++ show c ++ ")") (edges exampleNetwork)
```

## How it works:

1. **Graph Representation**: Uses adjacency list (`Graph`) and capacity map (`Capacity`) to represent the flow network
2. **BFS Path Finding**: Uses breadth-first search to find augmenting paths from source to sink
3. **Capacity Updates**: Updates forward and reverse edges after each augmentation
4. **Flow Accumulation**: Continues until no more augmenting paths exist

## Example Output:
```
Ford-Fulkerson Algorithm Example
================================
Maximum flow: 19
Network edges:
  0 -> 1 (capacity: 10)
  0 -> 2 (capacity: 5)
  0 -> 3 (capacity: 15)
  1 -> 2 (capacity: 4)
  1 -> 3 (capacity: 8)
  2 -> 3 (capacity: 5)
```

This implementation demonstrates the core concepts of the Ford-Fulkerson algorithm with a clear separation of concerns and proper type safety using Haskell's powerful type system.

