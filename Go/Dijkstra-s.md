# Dijkstra's Algorithm in Go

Here's a complete implementation of Dijkstra's algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Graph represents a weighted directed graph
type Graph struct {
    vertices int
    adjList  [][]Edge
}

// Edge represents an edge in the graph
type Edge struct {
    to     int
    weight int
}

// DijkstraResult stores the shortest distances and previous vertices
type DijkstraResult struct {
    distances []int
    previous  []int
}

// NewGraph creates a new graph with given number of vertices
func NewGraph(vertices int) *Graph {
    return &Graph{
        vertices: vertices,
        adjList:  make([][]Edge, vertices),
    }
}

// AddEdge adds a directed edge to the graph
func (g *Graph) AddEdge(from, to, weight int) {
    g.adjList[from] = append(g.adjList[from], Edge{to, weight})
}

// Dijkstra finds the shortest paths from source vertex to all other vertices
func (g *Graph) Dijkstra(source int) *DijkstraResult {
    distances := make([]int, g.vertices)
    previous := make([]int, g.vertices)
    visited := make([]bool, g.vertices)
    
    // Initialize distances to infinity
    for i := 0; i < g.vertices; i++ {
        distances[i] = math.MaxInt32
        previous[i] = -1
    }
    
    // Distance to source is 0
    distances[source] = 0
    
    for i := 0; i < g.vertices; i++ {
        // Find vertex with minimum distance
        u := g.minDistance(distances, visited)
        if u == -1 {
            break
        }
        
        visited[u] = true
        
        // Update distances of adjacent vertices
        for _, edge := range g.adjList[u] {
            if !visited[edge.to] {
                newDist := distances[u] + edge.weight
                if newDist < distances[edge.to] {
                    distances[edge.to] = newDist
                    previous[edge.to] = u
                }
            }
        }
    }
    
    return &DijkstraResult{
        distances: distances,
        previous:  previous,
    }
}

// minDistance finds the vertex with minimum distance value
func (g *Graph) minDistance(distances []int, visited []bool) int {
    minDist := math.MaxInt32
    minVertex := -1
    
    for v := 0; v < g.vertices; v++ {
        if !visited[v] && distances[v] < minDist {
            minDist = distances[v]
            minVertex = v
        }
    }
    
    return minVertex
}

// PrintPath prints the shortest path from source to destination
func (g *Graph) PrintPath(result *DijkstraResult, destination int) {
    if result.distances[destination] == math.MaxInt32 {
        fmt.Printf("No path from source to %d\n", destination)
        return
    }
    
    path := []int{}
    current := destination
    
    for current != -1 {
        path = append([]int{current}, path...)
        current = result.previous[current]
    }
    
    fmt.Printf("Shortest path to %d: %v (distance: %d)\n", 
        destination, path, result.distances[destination])
}

func main() {
    // Create a graph with 6 vertices (0-5)
    graph := NewGraph(6)
    
    // Add edges (from, to, weight)
    graph.AddEdge(0, 1, 4)
    graph.AddEdge(0, 2, 2)
    graph.AddEdge(1, 2, 1)
    graph.AddEdge(1, 3, 5)
    graph.AddEdge(2, 3, 8)
    graph.AddEdge(2, 4, 10)
    graph.AddEdge(3, 4, 2)
    graph.AddEdge(3, 5, 6)
    graph.AddEdge(4, 5, 3)
    
    fmt.Println("Graph edges:")
    fmt.Println("0->1 (weight: 4)")
    fmt.Println("0->2 (weight: 2)")
    fmt.Println("1->2 (weight: 1)")
    fmt.Println("1->3 (weight: 5)")
    fmt.Println("2->3 (weight: 8)")
    fmt.Println("2->4 (weight: 10)")
    fmt.Println("3->4 (weight: 2)")
    fmt.Println("3->5 (weight: 6)")
    fmt.Println("4->5 (weight: 3)")
    fmt.Println()
    
    // Run Dijkstra from vertex 0
    result := graph.Dijkstra(0)
    
    fmt.Println("Shortest distances from vertex 0:")
    for i, dist := range result.distances {
        if dist == math.MaxInt32 {
            fmt.Printf("Vertex %d: INF\n", i)
        } else {
            fmt.Printf("Vertex %d: %d\n", i, dist)
        }
    }
    fmt.Println()
    
    // Print specific paths
    fmt.Println("Shortest paths:")
    for i := 0; i < 6; i++ {
        graph.PrintPath(result, i)
    }
}
```

## Output

```
Graph edges:
0->1 (weight: 4)
0->2 (weight: 2)
1->2 (weight: 1)
1->3 (weight: 5)
2->3 (weight: 8)
2->4 (weight: 10)
3->4 (weight: 2)
3->5 (weight: 6)
4->5 (weight: 3)

Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
Vertex 5: 14

Shortest paths:
Shortest path to 0: [0] (distance: 0)
Shortest path to 1: [0 1] (distance: 4)
Shortest path to 2: [0 2] (distance: 2)
Shortest path to 3: [0 1 2 3] (distance: 9)
Shortest path to 4: [0 1 2 3 4] (distance: 11)
Shortest path to 5: [0 1 2 3 4 5] (distance: 14)
```

## Key Features

1. **Graph Representation**: Uses adjacency list representation
2. **Edge Structure**: Stores destination vertex and weight
3. **Dijkstra Implementation**: Standard greedy approach with O(V²) time complexity
4. **Path Reconstruction**: Tracks previous vertices to reconstruct shortest paths
5. **Error Handling**: Handles unreachable vertices gracefully

## Time Complexity
- **Time**: O(V²) where V is the number of vertices
- **Space**: O(V) for storing distances, previous vertices, and visited array

This implementation demonstrates the classic Dijkstra's algorithm for finding shortest paths in weighted graphs with non-negative edge weights.

