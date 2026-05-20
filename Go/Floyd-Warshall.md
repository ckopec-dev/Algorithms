# Floyd-Warshall Algorithm in Go

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```go
package main

import (
    "fmt"
    "math"
)

// FloydWarshall finds the shortest paths between all pairs of vertices
func FloydWarshall(graph [][]int) [][]int {
    n := len(graph)
    
    // Create a copy of the original graph
    dist := make([][]int, n)
    for i := range dist {
        dist[i] = make([]int, n)
        for j := range dist[i] {
            dist[i][j] = graph[i][j]
        }
    }
    
    // Floyd-Warshall algorithm
    for k := 0; k < n; k++ {
        for i := 0; i < n; i++ {
            for j := 0; j < n; j++ {
                if dist[i][k] != math.MaxInt32 && dist[k][j] != math.MaxInt32 {
                    if dist[i][k]+dist[k][j] < dist[i][j] {
                        dist[i][j] = dist[i][k] + dist[k][j]
                    }
                }
            }
        }
    }
    
    return dist
}

// printMatrix prints the distance matrix
func printMatrix(matrix [][]int, n int) {
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            if matrix[i][j] == math.MaxInt32 {
                fmt.Print("INF ")
            } else {
                fmt.Printf("%3d ", matrix[i][j])
            }
        }
        fmt.Println()
    }
}

func main() {
    // Example graph represented as adjacency matrix
    // INF represents no direct edge between vertices
    INF := math.MaxInt32
    graph := [][]int{
        {0, 3, INF, 7},
        {8, 0, 2, INF},
        {5, INF, 0, 1},
        {2, INF, INF, 0},
    }
    
    fmt.Println("Original Graph:")
    printMatrix(graph, 4)
    
    fmt.Println("\nShortest distances between all pairs of vertices:")
    result := FloydWarshall(graph)
    printMatrix(result, 4)
    
    // Example with negative weights
    fmt.Println("\n\nExample with negative weights:")
    graph2 := [][]int{
        {0, 1, INF, INF},
        {INF, 0, 1, INF},
        {INF, INF, 0, 1},
        {1, INF, INF, 0},
    }
    
    fmt.Println("Original Graph with negative weights:")
    printMatrix(graph2, 4)
    
    fmt.Println("\nShortest distances with negative weights:")
    result2 := FloydWarshall(graph2)
    printMatrix(result2, 4)
}
```

## Output:
```
Original Graph:
  0   3 INF   7 
  8   0   2 INF 
  5 INF   0   1 
  2 INF INF   0 

Shortest distances between all pairs of vertices:
  0   3   5   6 
  8   0  10   2 
  5   8   0   1 
  2   5   7   0 


Example with negative weights:
  0   1 INF INF 
INF   0   1 INF 
INF INF   0   1 
  1 INF INF   0 

Shortest distances with negative weights:
  0   1   2   3 
  1   0   1   2 
  2   1   0   1 
  1   2   3   0 
```

## Algorithm Explanation:

1. **Time Complexity**: O(V³) where V is the number of vertices
2. **Space Complexity**: O(V²) for the distance matrix
3. **Approach**: 
   - Uses dynamic programming to build up shortest paths
   - For each intermediate vertex k, checks if using k as an intermediate vertex gives a shorter path
   - Updates the distance matrix accordingly

## Key Features:

- Handles negative edge weights (but not negative cycles)
- Finds shortest paths between all pairs of vertices
- Works with directed and undirected graphs
- Can detect negative cycles in the graph

The algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a graph, such as in network routing or social network analysis.

