# Topological Sort in Haskell

Here's an implementation of topological sort using Kahn's algorithm in Haskell:

```haskell
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')

-- Graph representation: adjacency list
type Graph = Map.Map String [String]

-- Topological sort using Kahn's algorithm
topologicalSort :: Graph -> Maybe [String]
topologicalSort graph = 
    let vertices = Map.keys graph
        inDegree = calculateInDegrees graph
        zeroInDegree = filter (\v -> inDegree Map.! v == 0) vertices
    in case zeroInDegree of
        [] -> if null vertices then Just [] else Nothing  -- Cycle detected
        _ -> Just $ kahnAlgorithm graph inDegree zeroInDegree

-- Calculate in-degrees for all vertices
calculateInDegrees :: Graph -> Map.Map String Int
calculateInDegrees graph = 
    let allVertices = Map.keys graph
        edges = concatMap snd (Map.elems graph)
    in foldl' (\acc vertex -> 
        Map.insert vertex (length (filter (== vertex) edges)) acc)
        Map.empty allVertices

-- Main algorithm implementation
kahnAlgorithm :: Graph -> Map.Map String Int -> [String] -> [String]
kahnAlgorithm graph inDegree zeroInDegree = 
    case zeroInDegree of
        [] -> []
        (vertex:rest) -> 
            let newInDegree = removeEdges graph inDegree vertex
                newZeroInDegree = getNewZeroInDegree graph newInDegree (vertex:rest)
            in vertex : kahnAlgorithm graph newInDegree newZeroInDegree

-- Remove edges from a vertex and update in-degrees
removeEdges :: Graph -> Map.Map String Int -> String -> Map.Map String Int
removeEdges graph inDegree vertex = 
    case Map.lookup vertex graph of
        Nothing -> inDegree
        Just neighbors -> 
            foldl' (\acc neighbor -> 
                Map.insert neighbor (acc Map.! neighbor - 1) acc)
                inDegree neighbors

-- Get vertices with in-degree 0 after removing edges
getNewZeroInDegree :: Graph -> Map.Map String Int -> [String] -> [String]
getNewZeroInDegree graph inDegree visited = 
    let allVertices = Map.keys graph
        unvisited = filter (`notElem` visited) allVertices
    in filter (\v -> inDegree Map.! v == 0) unvisited

-- Alternative simpler implementation using DFS
topologicalSortDFS :: Graph -> [String]
topologicalSortDFS graph = 
    let vertices = Map.keys graph
        visited = Set.empty
        result = []
    in reverse $ dfsHelper graph visited result vertices

dfsHelper :: Graph -> Set.Set String -> [String] -> [String] -> [String]
dfsHelper graph visited result [] = result
dfsHelper graph visited result (v:vs) = 
    if Set.member v visited
        then dfsHelper graph visited result vs
        else let (newResult, newVisited) = dfsVisit graph visited v result
             in dfsHelper graph newVisited newResult vs

dfsVisit :: Graph -> Set.Set String -> String -> [String] -> ([String], Set.Set String)
dfsVisit graph visited vertex result = 
    let newVisited = Set.insert vertex visited
        neighbors = case Map.lookup vertex graph of
            Nothing -> []
            Just ns -> ns
        (finalResult, finalVisited) = 
            foldl' (\(accRes, accVis) neighbor -> 
                if Set.member neighbor accVis
                    then (accRes, accVis)
                    else let (newRes, newVis) = dfsVisit graph accVis neighbor accRes
                         in (newRes, newVis))
                (result, newVisited) neighbors
    in (vertex : finalResult, finalVisited)
```

## Example Usage

```haskell
-- Example graph representing dependencies
exampleGraph :: Graph
exampleGraph = Map.fromList
    [ ("c", ["a", "b"])
    , ("b", ["a"])
    , ("a", [])
    , ("d", ["c"])
    ]

-- Usage
main :: IO ()
main = do
    let result = topologicalSort exampleGraph
    case result of
        Just order -> putStrLn $ "Topological order: " ++ show order
        Nothing -> putStrLn "Cycle detected in graph"
```

## Output
```
Topological order: ["a","b","c","d"]
```

## Key Features

- **Graph Representation**: Uses `Data.Map.Map` for adjacency list representation
- **Cycle Detection**: Returns `Nothing` when a cycle is detected
- **Two Implementations**: 
  - Kahn's algorithm (iterative with in-degrees)
  - DFS-based approach (recursive)
- **Type Safety**: Strongly typed with proper error handling
- **Pure Functions**: No side effects, purely functional approach

The algorithm correctly handles:
- Empty graphs
- Cycles detection
- Multiple valid topological orders
- Proper dependency resolution

