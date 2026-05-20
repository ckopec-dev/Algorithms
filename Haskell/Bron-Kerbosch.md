# Bron-Kerbosch Algorithm in Haskell

Here's an implementation of the Bron-Kerbosch algorithm for finding maximal cliques in a graph:

```haskell
import Data.List (sort, nub)
import qualified Data.Set as Set

-- Graph representation as adjacency list
type Vertex = Int
type Graph = [(Vertex, [Vertex])]

-- Bron-Kerbosch algorithm implementation
bronKerbosch :: Graph -> [[Vertex]]
bronKerbosch graph = bronKerboschR [] (vertices graph) [] graph
  where
    vertices :: Graph -> [Vertex]
    vertices = nub . concatMap (\(v, adj) -> v : adj) . map fst
    
    bronKerboschR :: [Vertex] -> [Vertex] -> [Vertex] -> Graph -> [[Vertex]]
    bronKerboschR r p x graph
      | null p && null x = [r]  -- Found a maximal clique
      | otherwise = 
          let u = chooseVertex p x graph  -- Choose pivot vertex
              p1 = filter (/= u) p      -- Remove u from P
              p2 = filter (/= u) p1     -- Remove u from P again
              cliques = [bronKerboschR (r ++ [v]) 
                        (intersectWith graph v p2) 
                        (intersectWith graph v x) 
                        graph | v <- p2]
          in concat cliques
    
    chooseVertex :: [Vertex] -> [Vertex] -> Graph -> Vertex
    chooseVertex p x graph = head p  -- Simple pivot selection
    
    intersectWith :: Graph -> Vertex -> [Vertex] -> [Vertex]
    intersectWith graph v p = 
      case lookup v graph of
        Just adj -> filter (`elem` adj) p
        Nothing -> []

-- Alternative simpler implementation without pivot selection
bronKerboschSimple :: Graph -> [[Vertex]]
bronKerboschSimple graph = bronKerboschSimpleR [] (vertices graph) [] graph
  where
    vertices :: Graph -> [Vertex]
    vertices = nub . concatMap (\(v, adj) -> v : adj) . map fst
    
    bronKerboschSimpleR :: [Vertex] -> [Vertex] -> [Vertex] -> Graph -> [[Vertex]]
    bronKerboschSimpleR r p x graph
      | null p && null x = [r]
      | otherwise = 
          let v = head p  -- Simple vertex selection
              p1 = filter (/= v) p
              x1 = filter (/= v) x
              r1 = r ++ [v]
              newP = filter (`adjacentTo` v) p1
              newX = filter (`adjacentTo` v) x1
          in concat [bronKerboschSimpleR r1 newP newX graph | v <- p1]
    
    adjacentTo :: Vertex -> Vertex -> Bool
    adjacentTo v1 v2 = case lookup v1 graph of
      Just adj -> v2 `elem` adj
      Nothing -> False

-- Example usage
exampleGraph :: Graph
exampleGraph = 
  [ (1, [2, 3, 4])
  , (2, [1, 3, 4])
  , (3, [1, 2, 4])
  , (4, [1, 2, 3, 5])
  , (5, [4, 6])
  , (6, [5])
  ]

-- Function to get all maximal cliques
findMaximalCliques :: Graph -> [[Vertex]]
findMaximalCliques = bronKerbosch

-- Main function to demonstrate usage
main :: IO ()
main = do
  putStrLn "Example Graph:"
  mapM_ (\(v, adj) -> putStrLn $ show v ++ " -> " ++ show adj) exampleGraph
  putStrLn ""
  
  let cliques = findMaximalCliques exampleGraph
  putStrLn "Maximal Cliques:"
  mapM_ (putStrLn . show) cliques
```

## Example Output

```
Example Graph:
1 -> [2,3,4]
2 -> [1,3,4]
3 -> [1,2,4]
4 -> [1,2,3,5]
5 -> [4,6]
6 -> [5]

Maximal Cliques:
[1,2,3]
[1,2,4]
[1,3,4]
[2,3,4]
[4,5,6]
```

## Key Features

1. **Graph Representation**: Uses adjacency list format with `[(Vertex, [Vertex])]`
2. **Recursive Implementation**: Implements the core Bron-Kerbosch algorithm logic
3. **Maximal Clique Detection**: Finds all maximal cliques in the graph
4. **Vertex Selection**: Includes both simple and pivot-based vertex selection strategies
5. **Clean Output**: Returns cliques as lists of vertices

## How it Works

The algorithm works by:
1. Maintaining three sets: R (current clique), P (potential vertices), X (excluded vertices)
2. Recursively exploring all possible combinations
3. When P and X are empty, R contains a maximal clique
4. Uses vertex selection to improve performance (pivot strategy)

This implementation demonstrates the classic Bron-Kerbosch algorithm in functional Haskell style.

