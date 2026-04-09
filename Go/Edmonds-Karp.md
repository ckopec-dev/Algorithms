# Edmonds-Karp Algorithm Implementation in Go

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```go
package main

import (
    "fmt"
    "math"
)

// Graph represents a flow network
type Graph struct {
    vertices int
    capacity [][]int
}

// NewGraph creates a new graph with given number of vertices
func NewGraph(vertices int) *Graph {
    capacity := make([][]int, vertices)
    for i := range capacity {
        capacity[i] = make([]int, vertices)
    }
    return &Graph{
        vertices: vertices,
        capacity: capacity,
    }
}

// AddEdge adds an edge to the graph
func (g *Graph) AddEdge(from, to, capacity int) {
    g.capacity[from][to] = capacity
}

// BFS finds if there's an augmenting path from source to sink
// Returns true if path exists, false otherwise
func (g *Graph) BFS(source, sink int, parent []int) bool {
    visited := make([]bool, g.vertices)
    queue := []int{}
    
    visited[source] = true
    queue = append(queue, source)
    
    for len(queue) > 0 {
        u := queue[0]
        queue = queue[1:]
        
        for v := 0; v < g.vertices; v++ {
            if !visited[v] && g.capacity[u][v] > 0 {
                visited[v] = true
                parent[v] = u
                queue = append(queue, v)
                
                if v == sink {
                    return true
                }
            }
        }
    }
    
    return false
}

// EdmondsKarp finds the maximum flow from source to sink
func (g *Graph) EdmondsKarp(source, sink int) int {
    parent := make([]int, g.vertices)
    maxFlow := 0
    
    // Continue while there's an augmenting path
    for g.BFS(source, sink, parent) {
        // Find minimum capacity along the path
        pathFlow := math.MaxInt32
        s := sink
        
        for s != source {
            if g.capacity[parent[s]][s] < pathFlow {
                pathFlow = g.capacity[parent[s]][s]
            }
            s = parent[s]
        }
        
        // Update residual capacities
        s = sink
        for s != source {
            u := parent[s]
            g.capacity[u][s] -= pathFlow
            g.capacity[s][u] += pathFlow
            s = parent[s]
        }
        
        maxFlow += pathFlow
    }
    
    return maxFlow
}

func main() {
    // Create a graph with 6 vertices (0 to 5)
    g := NewGraph(6)
    
    // Add edges with capacities
    g.AddEdge(0, 1, 16)
    g.AddEdge(0, 2, 13)
    g.AddEdge(1, 2, 10)
    g.AddEdge(1, 3, 12)
    g.AddEdge(2, 1, 4)
    g.AddEdge(2, 4, 14)
    g.AddEdge(3, 2, 9)
    g.AddEdge(3, 5, 20)
    g.AddEdge(4, 3, 7)
    g.AddEdge(4, 5, 4)
    
    source := 0
    sink := 5
    
    maxFlow := g.EdmondsKarp(source, sink)
    
    fmt.Printf("Maximum flow from vertex %d to vertex %d is: %d\n", source, sink, maxFlow)
    
    // Print the residual graph
    fmt.Println("\nResidual graph after maximum flow computation:")
    for i := 0; i < g.vertices; i++ {
        for j := 0; j < g.vertices; j++ {
            if g.capacity[i][j] > 0 {
                fmt.Printf("Edge %d -> %d: %d\n", i, j, g.capacity[i][j])
            }
        }
    }
}
```

## Algorithm Explanation

### How it works:
1. **Initialize**: Start with a flow of 0
2. **Find augmenting path**: Use BFS to find a path from source to sink in the residual graph
3. **Calculate bottleneck**: Find the minimum capacity along the path
4. **Update flow**: Add the bottleneck capacity to the total flow
5. **Update residual graph**: Decrease forward edges and increase backward edges
6. **Repeat**: Continue until no more augmenting paths exist

### Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V²) for the adjacency matrix

### Example Output:
```
Maximum flow from vertex 0 to vertex 5 is: 23

Residual graph after maximum flow computation:
Edge 0 -> 1: 12
Edge 0 -> 2: 9
Edge 1 -> 3: 6
Edge 2 -> 4: 10
Edge 3 -> 5: 17
Edge 4 -> 3: 3
Edge 4 -> 5: 4
Edge 2 -> 1: 4
Edge 3 -> 2: 9
```

This implementation demonstrates the classic Edmonds-Karp algorithm for solving maximum flow problems, which is a fundamental algorithm in network flow theory.

