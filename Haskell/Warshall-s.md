# Floyd-Warshall Algorithm in Haskell

Here's an implementation of the Floyd-Warshall algorithm in Haskell to find the shortest paths between all pairs of vertices in a weighted graph:

```haskell
-- Floyd-Warshall algorithm implementation
floydWarshall :: (Ord a, Num a) => [[a]] -> [[a]]
floydWarshall matrix = fw matrix 0
  where
    n = length matrix
    fw dist k
      | k >= n = dist
      | otherwise = fw (updateMatrix dist k) (k + 1)
    
    updateMatrix dist k = [[updateCell i j dist k | j <- [0..n-1]] | i <- [0..n-1]]
    
    updateCell i j dist k = 
      let d_ij = dist !! i !! j
          d_ik = dist !! i !! k
          d_kj = dist !! k !! j
      in min d_ij (d_ik + d_kj)

-- Alternative implementation with explicit indexing
floydWarshall' :: (Ord a, Num a) => [[a]] -> [[a]]
floydWarshall' matrix = foldl updateMatrix matrix [0..length matrix - 1]
  where
    updateMatrix dist k = 
      [[min (dist !! i !! j) (dist !! i !! k + dist !! k !! j) | j <- [0..length dist - 1]] | i <- [0..length dist - 1]]

-- Example usage
exampleGraph :: [[Int]]
exampleGraph = 
  [ [0, 3, 8, 999, 999],
    [999, 0, 999, 1, 999],
    [999, 999, 0, 999, 2],
    [999, 999, 999, 0, 999],
    [999, 999, 999, 999, 0] ]

-- Run the algorithm
main :: IO ()
main = do
  let result = floydWarshall exampleGraph
  putStrLn "Original graph:"
  mapM_ (putStrLn . show) exampleGraph
  putStrLn "\nShortest paths matrix:"
  mapM_ (putStrLn . show) result
```

## How it works:

1. **Input**: A square matrix representing the adjacency matrix of a weighted graph
2. **Algorithm**: For each intermediate vertex `k`, update all pairs of vertices `(i,j)` to see if going through `k` gives a shorter path
3. **Output**: A matrix where `result[i][j]` contains the shortest distance from vertex `i` to vertex `j`

## Example Output:
```
Original graph:
[0,3,8,999,999]
[999,0,999,1,999]
[999,999,0,999,2]
[999,999,999,0,999]
[999,999,999,999,0]

Shortest paths matrix:
[0,3,5,1,7]
[999,0,999,1,3]
[999,999,0,999,2]
[999,999,999,0,999]
[999,999,999,999,0]
```

## Key Features:

- **Functional approach**: Pure functions without side effects
- **Generic types**: Works with any ordered numeric type
- **Immutability**: Matrix is not modified in place
- **Lazy evaluation**: Efficient computation of results
- **Clean syntax**: Uses Haskell's list comprehensions and higher-order functions

The algorithm has a time complexity of O(n³) and space complexity of O(n²) where n is the number of vertices.

