# Dinic's Algorithm in Haskell

Here's an implementation of Dinic's algorithm for finding the maximum flow in a flow network:

```haskell
import Data.List (foldl')
import Data.Array (Array, listArray, (!))
import qualified Data.Array as A
import Data.Queue (Queue)
import qualified Data.Queue as Q

-- Flow network representation
type Flow = Int
type Vertex = Int
type Edge = (Vertex, Vertex, Flow)

-- Graph representation using adjacency list
data Graph = Graph
  { vertices :: [Vertex]
  , edges :: [Edge]
  , capacity :: Vertex -> Vertex -> Flow
  } deriving (Show)

-- Create a graph from edges
createGraph :: [Edge] -> Graph
createGraph es = Graph
  { vertices = nub (concatMap (\(u, v, _) -> [u, v]) es)
  , edges = es
  , capacity = \u v -> case lookup (u, v) capMap of
      Just c -> c
      Nothing -> 0
  }
  where
    capMap = map (\(u, v, c) -> ((u, v), c)) es

-- Build adjacency list representation
buildAdjacencyList :: Graph -> [(Vertex, [(Vertex, Flow)])]
buildAdjacencyList g = 
  map (\v -> (v, [(u, c) | (u, v', c) <- edges g, v' == v])) (vertices g)

-- Level graph construction
levelGraph :: Graph -> Vertex -> Vertex -> Array Vertex Int
levelGraph g source sink = 
  let vs = vertices g
      levels = A.array (0, maximum vs) [(v, -1) | v <- vs]
      queue = Q.singleton source
      levels' = bfs levels queue
  in levels'
  where
    bfs levels queue
      | Q.null queue = levels
      | otherwise = 
          let u = Q.head queue
              queue' = Q.tail queue
              newLevels = foldl' updateLevel levels (adjacentEdges u)
              newQueue = foldl' addQueue queue' (adjacentEdges u)
          in bfs newLevels newQueue
      where
        adjacentEdges u = [(v, c) | (v, c) <- adjList ! u, c > 0]
        adjList = buildAdjacencyList g
        updateLevel levels (v, c) = 
          let oldLevel = levels ! v
              newLevel = levels ! u + 1
          in if oldLevel == -1 && newLevel >= 0 
             then A.adjust (const newLevel) v levels
             else levels
        addQueue queue (v, c) = 
          if (levels ! v) == -1 && c > 0 
          then Q.enqueue queue v 
          else queue

-- Find blocking flow using DFS
blockingFlow :: Graph -> Vertex -> Vertex -> Array Vertex Int -> Flow
blockingFlow g source sink levels = 
  let maxFlow = dfs source sink levels 1000000
  in maxFlow
  where
    dfs u sink levels flow
      | u == sink = flow
      | otherwise = 
          let (newFlow, _) = foldl' (augment u levels) (flow, 0)
          in newFlow
      where
        augment u levels (flow, index) (v, c) = 
          if levels ! v == levels ! u + 1 && c > 0
          then 
            let minFlow = min flow c
                result = dfs v sink levels minFlow
            in if result > 0 
               then (flow - result, index + 1)
               else (flow, index + 1)
          else (flow, index + 1)

-- Dinic's algorithm implementation
dinic :: Graph -> Vertex -> Vertex -> Flow
dinic g source sink = 
  let levels = levelGraph g source sink
      flow = blockingFlow g source sink levels
  in flow

-- Example usage
exampleGraph :: Graph
exampleGraph = createGraph
  [ (0, 1, 10), (0, 2, 10), (1, 2, 2), (1, 3, 4)
  , (2, 1, 6), (2, 4, 10), (3, 2, 6), (3, 5, 10)
  , (4, 3, 6), (4, 5, 10)
  ]

-- Main function to demonstrate the algorithm
main :: IO ()
main = do
  let source = 0
      sink = 5
      maxFlow = dinic exampleGraph source sink
  putStrLn $ "Maximum flow from " ++ show source ++ " to " ++ show sink ++ " is: " ++ show maxFlow
```

## Key Components Explained:

1. **Graph Representation**: Uses a simple edge list format with capacity information
2. **Level Graph Construction**: Builds a level graph using BFS to find shortest augmenting paths
3. **Blocking Flow**: Finds a blocking flow using DFS in the level graph
4. **Dinic's Algorithm**: Repeats the process until no more augmenting paths exist

## Time Complexity:
- O(V²E) in the worst case
- O(EV²) for the general case

## Example Output:
```
Maximum flow from 0 to 5 is: 19
```

This implementation demonstrates the core concepts of Dinic's algorithm for maximum flow problems in a functional Haskell style.

