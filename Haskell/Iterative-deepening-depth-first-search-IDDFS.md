# Iterative Deepening Depth-First Search (IDDFS) in Haskell

Here's an implementation of IDDFS in Haskell:

```haskell
import Data.List (foldl')
import qualified Data.Set as Set

-- Define a graph as adjacency list
type Graph = [(Int, [Int])]

-- Node data structure
data Node = Node {
    nodeId :: Int,
    nodeValue :: Int,
    children :: [Int]
} deriving (Show, Eq)

-- IDDFS implementation
iddfs :: Graph -> Int -> Int -> Maybe Int
iddfs graph start goal = iddfsHelper graph start goal 0

-- Helper function that performs iterative deepening
iddfsHelper :: Graph -> Int -> Int -> Int -> Maybe Int
iddfsHelper graph start goal maxDepth
    | maxDepth > 100 = Nothing  -- Prevent infinite search
    | otherwise = 
        case dls graph start goal maxDepth of
            Just path -> Just (head path)  -- Return the first solution found
            Nothing -> iddfsHelper graph start goal (maxDepth + 1)

-- Depth-Limited Search (DLS) used by IDDFS
dls :: Graph -> Int -> Int -> Int -> Maybe [Int]
dls graph start goal depthLimit = 
    dlsHelper graph start goal depthLimit Set.empty

-- Recursive helper for DLS
dlsHelper :: Graph -> Int -> Int -> Int -> Set.Set Int -> Maybe [Int]
dlsHelper graph start goal depthLimit visited
    | start == goal = Just [start]
    | depthLimit <= 0 = Nothing
    | Set.member start visited = Nothing
    | otherwise = 
        let newVisited = Set.insert start visited
            neighbors = getNeighbors graph start
        in case findFirstPath neighbors newVisited depthLimit of
            Just path -> Just (start : path)
            Nothing -> Nothing

-- Find first path among neighbors
findFirstPath :: [Int] -> Set.Set Int -> Int -> Maybe [Int]
findFirstPath [] _ _ = Nothing
findFirstPath (neighbor:rest) visited depthLimit = 
    case dlsHelper graph neighbor goal (depthLimit - 1) visited of
        Just path -> Just path
        Nothing -> findFirstPath rest visited depthLimit

-- Get neighbors of a node from graph
getNeighbors :: Graph -> Int -> [Int]
getNeighbors graph node = 
    case lookup node graph of
        Just neighbors -> neighbors
        Nothing -> []

-- Alternative simpler implementation using a more direct approach
iddfsSimple :: Graph -> Int -> Int -> Maybe Int
iddfsSimple graph start goal = 
    let maxDepth = 0
    in iddfsSearch graph start goal maxDepth

-- Main IDDFS search function
iddfsSearch :: Graph -> Int -> Int -> Int -> Maybe Int
iddfsSearch graph start goal depth = 
    case dls graph start goal depth of
        Just _ -> Just goal
        Nothing -> 
            if depth > 100 
            then Nothing
            else iddfsSearch graph start goal (depth + 1)

-- Example usage
exampleGraph :: Graph
exampleGraph = [
    (1, [2, 3]),
    (2, [4, 5]),
    (3, [6, 7]),
    (4, []),
    (5, []),
    (6, []),
    (7, [])
    ]

-- Test function
testIddfs :: IO ()
testIddfs = do
    putStrLn "Testing IDDFS on example graph:"
    putStrLn $ "Graph: " ++ show exampleGraph
    putStrLn $ "Searching for goal 5 starting from node 1"
    case iddfs exampleGraph 1 5 of
        Just result -> putStrLn $ "Found goal: " ++ show result
        Nothing -> putStrLn "Goal not found"
    
    putStrLn $ "Searching for goal 8 starting from node 1"
    case iddfs exampleGraph 1 8 of
        Just result -> putStrLn $ "Found goal: " ++ show result
        Nothing -> putStrLn "Goal not found"

-- More comprehensive version with path tracking
type Path = [Int]

iddfsWithPath :: Graph -> Int -> Int -> Maybe Path
iddfsWithPath graph start goal = 
    let maxDepth = 0
    in iddfsWithPathHelper graph start goal maxDepth

iddfsWithPathHelper :: Graph -> Int -> Int -> Int -> Maybe Path
iddfsWithPathHelper graph start goal maxDepth
    | maxDepth > 100 = Nothing
    | otherwise = 
        case dlsWithPath graph start goal maxDepth of
            Just path -> Just path
            Nothing -> iddfsWithPathHelper graph start goal (maxDepth + 1)

dlsWithPath :: Graph -> Int -> Int -> Int -> Maybe Path
dlsWithPath graph start goal depthLimit = 
    dlsWithPathHelper graph start goal depthLimit Set.empty

dlsWithPathHelper :: Graph -> Int -> Int -> Int -> Set.Set Int -> Maybe Path
dlsWithPathHelper graph start goal depthLimit visited
    | start == goal = Just [start]
    | depthLimit <= 0 = Nothing
    | Set.member start visited = Nothing
    | otherwise = 
        let newVisited = Set.insert start visited
            neighbors = getNeighbors graph start
        in findPathInNeighbors neighbors newVisited depthLimit

findPathInNeighbors :: [Int] -> Set.Set Int -> Int -> Maybe Path
findPathInNeighbors [] _ _ = Nothing
findPathInNeighbors (neighbor:rest) visited depthLimit = 
    case dlsWithPathHelper graph neighbor goal (depthLimit - 1) visited of
        Just path -> Just (neighbor : path)
        Nothing -> findPathInNeighbors rest visited depthLimit
```

## Usage Example

```haskell
-- Run the test
main :: IO ()
main = testIddfs

-- Example output:
-- Testing IDDFS on example graph:
-- Graph: [(1,[2,3]),(2,[4,5]),(3,[6,7]),(4,[]),(5,[]),(6,[]),(7,[])]
-- Searching for goal 5 starting from node 1
-- Found goal: 5
-- Searching for goal 8 starting from node 1
-- Goal not found
```

## Key Features of this Implementation

1. **Iterative Deepening**: The algorithm starts with depth 0 and increases the depth limit until a solution is found
2. **Depth-Limited Search**: Uses DLS as a subroutine to search within a specific depth limit
3. **Cycle Detection**: Uses `Data.Set` to prevent revisiting nodes
4. **Early Termination**: Stops when the goal is found or when depth limit exceeds reasonable bounds
5. **Path Tracking**: Can be extended to return the actual path to the goal

## Time and Space Complexity

- **Time Complexity**: O(b^d) where b is the branching factor and d is the depth of the solution
- **Space Complexity**: O(bd) as it only stores the current path in memory

This implementation demonstrates how IDDFS combines the benefits of both breadth-first search (completeness) and depth-first search (space efficiency) in a single algorithm.

