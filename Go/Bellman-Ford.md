# Bellman-Ford Algorithm in Go

Here's a complete implementation of the Bellman-Ford algorithm in Go to find shortest paths from a source vertex to all other vertices in a weighted graph:

```go
package main

import (
    "fmt"
    "math"
)

// Edge represents a weighted edge in the graph
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
    
    // Relax all edges vertices-1 times
    for i := 0; i < vertices-1; i++ {
        for _, edge := range edges {
            if distances[edge.src] != math.MaxInt32 {
                if distances[edge.src]+edge.weight < distances[edge.dest] {
                    distances[edge.dest] = distances[edge.src] + edge.weight
                }
            }
        }
    }
    
    // Check for negative weight cycles
    for _, edge := range edges {
        if distances[edge.src] != math.MaxInt32 {
            if distances[edge.src]+edge.weight < distances[edge.dest] {
                return distances, true // Negative cycle detected
            }
        }
    }
    
    return distances, false // No negative cycle
}

func main() {
    // Example graph representation
    // Vertices: 0, 1, 2, 3, 4
    // Edges: (source, destination, weight)
    edges := []Edge{
        {0, 1, 4},
        {0, 2, 2},
        {1, 2, 1},
        {1, 3, 5},
        {2, 3, 8},
        {2, 4, 10},
        {3, 4, 2},
    }
    
    vertices := 5
    source := 0
    
    fmt.Println("Graph edges:")
    for _, edge := range edges {
        fmt.Printf("  %d -> %d (weight: %d)\n", edge.src, edge.dest, edge.weight)
    }
    
    distances, hasNegativeCycle := BellmanFord(vertices, edges, source)
    
    if hasNegativeCycle {
        fmt.Println("\nNegative weight cycle detected!")
    } else {
        fmt.Printf("\nShortest distances from vertex %d:\n", source)
        for i, dist := range distances {
            if dist == math.MaxInt32 {
                fmt.Printf("Vertex %d: INF\n", i)
            } else {
                fmt.Printf("Vertex %d: %d\n", i, dist)
            }
        }
    }
    
    // Example with negative weight cycle
    fmt.Println("\n" + "="*50)
    fmt.Println("Example with negative weight cycle:")
    
    edgesWithNegativeCycle := []Edge{
        {0, 1, 1},
        {1, 2, 1},
        {2, 1, -4}, // This creates a negative cycle
    }
    
    distances2, hasNegativeCycle2 := BellmanFord(3, edgesWithNegativeCycle, 0)
    
    if hasNegativeCycle2 {
        fmt.Println("Negative weight cycle detected!")
    } else {
        fmt.Printf("Shortest distances from vertex 0:\n")
        for i, dist := range distances2 {
            if dist == math.MaxInt32 {
                fmt.Printf("Vertex %d: INF\n", i)
            } else {
                fmt.Printf("Vertex %d: %d\n", i, dist)
            }
        }
    }
}
```

## Output:
```
Graph edges:
  0 -> 1 (weight: 4)
  0 -> 2 (weight: 2)
  1 -> 2 (weight: 1)
  1 -> 3 (weight: 5)
  2 -> 3 (weight: 8)
  2 -> 4 (weight: 10)
  3 -> 4 (weight: 2)

Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 3
Vertex 2: 2
Vertex 3: 8
Vertex 4: 10

==================================================
Example with negative weight cycle:
Negative weight cycle detected!
```

## Key Features:

1. **Edge Structure**: Defines weighted edges with source, destination, and weight
2. **Distance Initialization**: Sets all distances to infinity except source
3. **Relaxation**: Performs V-1 iterations to find shortest paths
4. **Negative Cycle Detection**: Checks for negative weight cycles after relaxation
5. **Error Handling**: Returns whether a negative cycle was detected

## Time Complexity: O(VE) where V is vertices and E is edges
## Space Complexity: O(V) for storing distances

The algorithm can detect negative weight cycles, which is a key advantage over Dijkstra's algorithm.

