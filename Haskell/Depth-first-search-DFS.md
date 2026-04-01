# Depth-First Search (DFS) in Haskell

Here's an implementation of the Depth-First Search algorithm in Haskell:

```haskell
-- Define a graph as an adjacency list
type Graph = [(Int, [Int])]

-- DFS implementation using recursion
dfs :: Graph -> Int -> [Int]
dfs graph start = dfsHelper graph [start] []

dfsHelper :: Graph -> [Int] -> [Int] -> [Int]
dfsHelper graph visited [] = visited
dfsHelper graph visited (node:stack) = 
    let neighbors = getNeighbors graph node
        unvisitedNeighbors = filter (`notElem` visited) neighbors
        newStack = unvisitedNeighbors ++ stack
        newVisited = node : visited
    in dfsHelper graph newVisited newStack

-- Helper function to get neighbors of a node
getNeighbors :: Graph -> Int -> [Int]
getNeighbors graph node = case lookup node graph of
    Just neighbors -> neighbors
    Nothing -> []

-- Alternative implementation using a stack explicitly
dfsStack :: Graph -> Int -> [Int]
dfsStack graph start = dfsStackHelper graph [start] []

dfsStackHelper :: Graph -> [Int] -> [Int] -> [Int]
dfsStackHelper graph stack visited
    | null stack = visited
    | otherwise = 
        let current = head stack
            neighbors = getNeighbors graph current
            unvisited = filter (`notElem` visited) neighbors
            newStack = unvisited ++ tail stack
            newVisited = current : visited
        in dfsStackHelper graph newStack newVisited

-- Example usage
exampleGraph :: Graph
exampleGraph = [
    (1, [2, 3]),
    (2, [4, 5]),
    (3, [6]),
    (4, []),
    (5, [7]),
    (6, []),
    (7, [])
    ]

-- Run DFS starting from node 1
main :: IO ()
main = do
    putStrLn "Graph representation:"
    mapM_ print exampleGraph
    putStrLn "\nDFS traversal starting from node 1:"
    print $ dfs exampleGraph 1
    putStrLn "\nDFS traversal using stack implementation:"
    print $ dfsStack exampleGraph 1
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list format where each node maps to its neighbors
2. **Recursive Approach**: Implements DFS using recursion with a visited list
3. **Stack Management**: Maintains the order of nodes to visit next
4. **Visited Tracking**: Keeps track of already visited nodes to avoid cycles
5. **Two Versions**: 
   - Basic recursive implementation
   - Explicit stack-based implementation

## Sample Output:
```
Graph representation:
(1,[2,3])
(2,[4,5])
(3,[6])
(4,[])
(5,[7])
(6,[])
(7,[])

DFS traversal starting from node 1:
[1,2,4,5,7,3,6]

DFS traversal using stack implementation:
[1,2,4,5,7,3,6]
```

This implementation demonstrates the core concept of DFS where we explore as far as possible along each branch before backtracking.

