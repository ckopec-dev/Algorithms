# Bellman-Ford Algorithm in Go

Here's a complete implementation of the Bellman-Ford algorithm in Go to find shortest paths from a source vertex to all other vertices in a weighted graph:

```go
package main

import (
    "fmt"
    "math"
)

// Edge represents a directed edge in the graph
type Edge struct {
    src, dest, weight int
}

// BellmanFord finds shortest paths from source to all vertices
func BellmanFord(vertices int, edges []Edge, source int) ([]int, bool) {
    // Initialize distances from source to all vertices as infinity
    distances := make([]int, vertices)
    for i := range distances {
        distances[i] = math.MaxInt32
    }
    distances[source] = 0
    
    // Relax edges repeatedly
    for i := 0; i < vertices-1; i++ {
        for _, edge := range edges {
            // If we can find a shorter path, update the distance
            if distances[edge.src] != math.MaxInt32 && 
               distances[edge.src]+edge.weight < distances[edge.dest] {
                distances[edge.dest] = distances[edge.src] + edge.weight
            }
        }
    }
    
    // Check for negative weight cycles
    for _, edge := range edges {
        if distances[edge.src] != math.MaxInt32 && 
           distances[edge.src]+edge.weight < distances[edge.dest] {
            return distances, true // Negative cycle detected
        }
    }
    
    return distances, false // No negative cycle
}

func main() {
    // Example graph with 5 vertices (0, 1, 2, 3, 4)
    vertices := 5
    edges := []Edge{
        {0, 1, -1}, // Edge from 0 to 1 with weight -1
        {0, 2, 4},  // Edge from 0 to 2 with weight 4
        {1, 2, 3},  // Edge from 1 to 2 with weight 3
        {1, 3, 2},  // Edge from 1 to 3 with weight 2
        {1, 4, 2},  // Edge from 1 to 4 with weight 2
        {3, 2, 5},  // Edge from 3 to 2 with weight 5
        {3, 1, 1},  // Edge from 3 to 1 with weight 1
        {4, 3, -3}, // Edge from 4 to 3 with weight -3
    }
    
    source := 0
    distances, hasNegativeCycle := BellmanFord(vertices, edges, source)
    
    if hasNegativeCycle {
        fmt.Println("Graph contains negative weight cycle!")
        return
    }
    
    fmt.Printf("Shortest distances from source vertex %d:\n", source)
    for i, dist := range distances {
        if dist == math.MaxInt32 {
            fmt.Printf("Vertex %d: INF\n", i)
        } else {
            fmt.Printf("Vertex %d: %d\n", i, dist)
        }
    }
    
    // Example with negative cycle
    fmt.Println("\n--- Testing with negative cycle ---")
    edgesWithNegativeCycle := []Edge{
        {0, 1, 1},
        {1, 2, 1},
        {2, 0, -3}, // Creates negative cycle
    }
    
    _, hasNegativeCycle2 := BellmanFord(3, edgesWithNegativeCycle, 0)
    if hasNegativeCycle2 {
        fmt.Println("Graph contains negative weight cycle!")
    }
}
```

## Output:
```
Shortest distances from source vertex 0:
Vertex 0: 0
Vertex 1: -1
Vertex 2: 2
Vertex 3: -2
Vertex 4: 1

--- Testing with negative cycle ---
Graph contains negative weight cycle!
```

## Key Features:

1. **Edge Structure**: Represents directed edges with source, destination, and weight
2. **Distance Initialization**: Sets all distances to infinity except source (0)
3. **Relaxation**: Repeatedly updates distances by checking all edges
4. **Negative Cycle Detection**: Checks for negative weight cycles after V-1 iterations
5. **Return Values**: Returns distances array and boolean indicating negative cycle

## Time Complexity:
- **Time**: O(VE) where V is vertices and E is edges
- **Space**: O(V) for storing distances

## Use Cases:
- Finding shortest paths in graphs with negative edge weights
- Detecting negative weight cycles
- Network routing algorithms
- Financial arbitrage detection

The algorithm is particularly useful when dealing with graphs that may contain negative edge weights, unlike Dijkstra's algorithm which only works with non-negative weights.

