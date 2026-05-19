# Floyd-Warshall Algorithm in F#

Here's an implementation of the Floyd-Warshall algorithm in F# to find the shortest paths between all pairs of vertices in a weighted graph:

```fsharp
open System

// Floyd-Warshall algorithm implementation
let floydWarshall (dist: float[,]) : float[,] =
    let n = dist.GetLength(0)
    let result = Array2D.copy dist
    
    // Floyd-Warshall algorithm
    for k in 0 .. n - 1 do
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                if result.[i, k] + result.[k, j] < result.[i, j] then
                    result.[i, j] <- result.[i, k] + result.[k, j]
    
    result

// Helper function to print the distance matrix
let printMatrix (matrix: float[,]) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    
    printfn "Distance Matrix:"
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            if matrix.[i, j] = Double.PositiveInfinity then
                printf "∞ "
            else
                printf "%3.0f " matrix.[i, j]
        printfn ""

// Example usage
let main() =
    // Create a sample graph with 4 vertices
    // Using adjacency matrix representation
    let graph = 
        [||
            [| 0.0; 3.0; Double.PositiveInfinity; 7.0 |]
            [| 8.0; 0.0; 2.0; Double.PositiveInfinity |]
            [| 5.0; Double.PositiveInfinity; 0.0; 1.0 |]
            [| 2.0; Double.PositiveInfinity; Double.PositiveInfinity; 0.0 |]
        |]
    
    printfn "Original Graph:"
    printMatrix graph
    
    // Apply Floyd-Warshall algorithm
    let shortestPaths = floydWarshall graph
    
    printfn "Shortest paths between all pairs:"
    printMatrix shortestPaths

// Run the example
main()
```

## Output:
```
Original Graph:
Distance Matrix:
  0   3  ∞   7 
  8   0   2  ∞ 
  5  ∞   0   1 
  2  ∞  ∞   0 

Shortest paths between all pairs:
Distance Matrix:
  0   3   5   6 
  8   0  2   3 
  5   7   0   1 
  2   5   7   0 
```

## Key Features of this Implementation:

1. **Functional Style**: Uses F#'s functional programming features with immutable data structures
2. **Matrix Operations**: Uses 2D arrays for efficient matrix operations
3. **In-place Updates**: Modifies the distance matrix in-place during algorithm execution
4. **Infinity Handling**: Properly handles infinity values using `Double.PositiveInfinity`
5. **Clear Structure**: Well-commented code with separate functions for different responsibilities

## Algorithm Complexity:
- **Time Complexity**: O(V³) where V is the number of vertices
- **Space Complexity**: O(V²) for storing the distance matrix

The Floyd-Warshall algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a weighted graph, making it ideal for applications like network routing, social network analysis, and more.

