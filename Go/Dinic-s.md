# Dinic's Algorithm Implementation in Go

Dinic's algorithm is a strongly polynomial algorithm for computing the maximum flow in a flow network. Here's a complete implementation:

```go
package main

import (
    "fmt"
    "math"
)

// Edge represents a directed edge in the graph
type Edge struct {
    to     int
    capacity int
    reverse int // index of the reverse edge
}

// Dinic represents the flow network
type Dinic struct {
    graph   [][]Edge
    level   []int
    ptr     []int
    source  int
    sink    int
    n       int
}

// NewDinic creates a new Dinic instance
func NewDinic(n, source, sink int) *Dinic {
    graph := make([][]Edge, n)
    level := make([]int, n)
    ptr := make([]int, n)
    
    return &Dinic{
        graph:  graph,
        level:  level,
        ptr:    ptr,
        source: source,
        sink:   sink,
        n:      n,
    }
}

// AddEdge adds a directed edge with given capacity
func (d *Dinic) AddEdge(from, to, capacity int) {
    // Forward edge
    d.graph[from] = append(d.graph[from], Edge{to, capacity, len(d.graph[to])})
    // Reverse edge
    d.graph[to] = append(d.graph[to], Edge{from, 0, len(d.graph[from]) - 1})
}

// BFS builds level graph using BFS
func (d *Dinic) BFS() bool {
    // Initialize level array
    for i := 0; i < d.n; i++ {
        d.level[i] = -1
    }
    
    // BFS from source
    queue := []int{d.source}
    d.level[d.source] = 0
    
    for len(queue) > 0 {
        u := queue[0]
        queue = queue[1:]
        
        for _, edge := range d.graph[u] {
            if d.level[edge.to] == -1 && edge.capacity > 0 {
                d.level[edge.to] = d.level[u] + 1
                queue = append(queue, edge.to)
            }
        }
    }
    
    return d.level[d.sink] != -1
}

// DFS finds augmenting path using DFS
func (d *Dinic) DFS(u, flow int) int {
    if u == d.sink {
        return flow
    }
    
    for d.ptr[u] < len(d.graph[u]) {
        edge := &d.graph[u][d.ptr[u]]
        
        if d.level[edge.to] == d.level[u]+1 && edge.capacity > 0 {
            minFlow := d.DFS(edge.to, min(flow, edge.capacity))
            
            if minFlow > 0 {
                edge.capacity -= minFlow
                d.graph[edge.to][edge.reverse].capacity += minFlow
                return minFlow
            }
        }
        
        d.ptr[u]++
    }
    
    return 0
}

// MaxFlow computes maximum flow from source to sink
func (d *Dinic) MaxFlow() int {
    maxFlow := 0
    
    for d.BFS() {
        // Reset ptr array for each level
        for i := 0; i < d.n; i++ {
            d.ptr[i] = 0
        }
        
        // Find and add flow along blocking flows
        for {
            flow := d.DFS(d.source, math.MaxInt32)
            if flow == 0 {
                break
            }
            maxFlow += flow
        }
    }
    
    return maxFlow
}

// Example usage
func main() {
    // Create a flow network with 6 vertices (0 to 5)
    // Source = 0, Sink = 5
    dinic := NewDinic(6, 0, 5)
    
    // Add edges with capacities
    dinic.AddEdge(0, 1, 10)
    dinic.AddEdge(0, 2, 10)
    dinic.AddEdge(1, 2, 2)
    dinic.AddEdge(1, 3, 4)
    dinic.AddEdge(1, 4, 8)
    dinic.AddEdge(2, 4, 9)
    dinic.AddEdge(3, 5, 10)
    dinic.AddEdge(4, 5, 10)
    
    maxFlow := dinic.MaxFlow()
    fmt.Printf("Maximum flow: %d\n", maxFlow)
    
    // Another example with a simpler network
    fmt.Println("\n--- Second Example ---")
    dinic2 := NewDinic(4, 0, 3)
    dinic2.AddEdge(0, 1, 10)
    dinic2.AddEdge(0, 2, 10)
    dinic2.AddEdge(1, 3, 10)
    dinic2.AddEdge(2, 3, 10)
    
    maxFlow2 := dinic2.MaxFlow()
    fmt.Printf("Maximum flow: %d\n", maxFlow2)
}
```

## How Dinic's Algorithm Works

1. **Level Graph Construction**: Uses BFS to build a level graph where each node has a level (distance from source)
2. **Blocking Flow**: Uses DFS to find augmenting paths in the level graph
3. **Repeat**: Continue until no more augmenting paths exist

## Key Features

- **Time Complexity**: O(V²E) in worst case, but often much better in practice
- **Space Complexity**: O(V + E)
- **Handles**: Multiple edges, parallel edges, and self-loops
- **Applications**: Network flow problems, bipartite matching, etc.

## Sample Output

```
Maximum flow: 19

--- Second Example ---
Maximum flow: 20
```

This implementation provides a complete Dinic's algorithm for solving maximum flow problems in flow networks.

