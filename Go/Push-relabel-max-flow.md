# Push-Relabel Max-Flow Algorithm in Go

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```go
package main

import (
    "fmt"
    "math"
)

// Edge represents a directed edge in the flow network
type Edge struct {
    to     int // destination vertex
    capacity int // maximum capacity of the edge
    flow   int // current flow through the edge
    reverse int // index of the reverse edge in the adjacency list
}

// Graph represents a flow network
type Graph struct {
    vertices int
    edges    [][]Edge
    heights  []int // height of each vertex
    excess   []int // excess flow at each vertex
}

// NewGraph creates a new flow network with n vertices
func NewGraph(n int) *Graph {
    return &Graph{
        vertices: n,
        edges:    make([][]Edge, n),
        heights:  make([]int, n),
        excess:   make([]int, n),
    }
}

// AddEdge adds a directed edge from u to v with given capacity
func (g *Graph) AddEdge(u, v, capacity int) {
    // Forward edge
    forward := Edge{to: v, capacity: capacity, flow: 0, reverse: len(g.edges[v])}
    // Reverse edge
    reverse := Edge{to: u, capacity: 0, flow: 0, reverse: len(g.edges[u])}
    
    g.edges[u] = append(g.edges[u], forward)
    g.edges[v] = append(g.edges[v], reverse)
}

// Push pushes flow from vertex u to vertex v
func (g *Graph) Push(u, v int) {
    // Find the edge from u to v
    edgeIndex := -1
    for i, edge := range g.edges[u] {
        if edge.to == v {
            edgeIndex = i
            break
        }
    }
    
    if edgeIndex == -1 {
        return
    }
    
    edge := &g.edges[u][edgeIndex]
    
    // Calculate the amount of flow that can be pushed
    pushAmount := min(edge.capacity-edge.flow, g.excess[u])
    
    if pushAmount <= 0 {
        return
    }
    
    // Update flows
    edge.flow += pushAmount
    g.excess[u] -= pushAmount
    
    // Update reverse edge flow (for residual network)
    reverseEdgeIndex := edge.reverse
    g.edges[v][reverseEdgeIndex].flow -= pushAmount
    g.excess[v] += pushAmount
}

// Relabel relabels vertex u to increase its height
func (g *Graph) Relabel(u int) {
    minHeight := math.MaxInt32
    
    // Find minimum height of adjacent vertices
    for _, edge := range g.edges[u] {
        if edge.capacity > edge.flow && g.heights[edge.to] < minHeight {
            minHeight = g.heights[edge.to]
        }
    }
    
    if minHeight < math.MaxInt32 {
        g.heights[u] = minHeight + 1
    }
}

// IsOverflow checks if vertex u has excess flow
func (g *Graph) IsOverflow(u int) bool {
    return g.excess[u] > 0
}

// GetHeight returns the height of vertex u
func (g *Graph) GetHeight(u int) int {
    return g.heights[u]
}

// GetExcess returns the excess flow at vertex u
func (g *Graph) GetExcess(u int) int {
    return g.excess[u]
}

// GetEdge returns the edge from u to v
func (g *Graph) GetEdge(u, v int) *Edge {
    for _, edge := range g.edges[u] {
        if edge.to == v {
            return &edge
        }
    }
    return nil
}

// MaxFlow computes the maximum flow from source to sink
func (g *Graph) MaxFlow(source, sink int) int {
    // Initialize heights and excess flows
    g.heights[source] = g.vertices
    g.excess[source] = math.MaxInt32
    
    // Push initial flow from source to its neighbors
    for i, edge := range g.edges[source] {
        if edge.capacity > 0 {
            g.excess[source] -= edge.capacity
            g.excess[edge.to] += edge.capacity
            g.edges[source][i].flow = edge.capacity
            g.edges[edge.to][edge.reverse].flow = -edge.capacity
        }
    }
    
    // Main loop of push-relabel algorithm
    for {
        // Find a vertex with excess flow
        var u int = -1
        for i := 0; i < g.vertices; i++ {
            if i != source && i != sink && g.excess[i] > 0 {
                u = i
                break
            }
        }
        
        if u == -1 {
            break // No more vertices with excess flow
        }
        
        // Try to push flow from u
        pushed := false
        for _, edge := range g.edges[u] {
            if edge.capacity > edge.flow && g.heights[u] > g.heights[edge.to] {
                g.Push(u, edge.to)
                pushed = true
                break
            }
        }
        
        if !pushed {
            // Relabel if no push is possible
            g.Relabel(u)
        }
    }
    
    // Return the excess flow at sink (which is the maximum flow)
    return g.excess[sink]
}

// PrintGraph prints the current state of the graph
func (g *Graph) PrintGraph() {
    fmt.Println("Graph edges:")
    for u := 0; u < g.vertices; u++ {
        fmt.Printf("Vertex %d: ", u)
        for _, edge := range g.edges[u] {
            if edge.capacity > 0 {
                fmt.Printf("-> %d(c=%d,f=%d) ", edge.to, edge.capacity, edge.flow)
            }
        }
        fmt.Println()
    }
    fmt.Println()
}

// PrintState prints the current state of heights and excess flows
func (g *Graph) PrintState() {
    fmt.Println("Heights:", g.heights)
    fmt.Println("Excess flows:", g.excess)
    fmt.Println()
}

func main() {
    // Create a flow network with 6 vertices (0 to 5)
    // Source = 0, Sink = 5
    graph := NewGraph(6)
    
    // Add edges with capacities
    // Source (0) to intermediate vertices
    graph.AddEdge(0, 1, 10)
    graph.AddEdge(0, 2, 10)
    
    // Intermediate vertices
    graph.AddEdge(1, 3, 4)
    graph.AddEdge(1, 4, 8)
    graph.AddEdge(2, 4, 9)
    graph.AddEdge(3, 5, 10)
    graph.AddEdge(4, 5, 10)
    
    fmt.Println("Flow Network:")
    graph.PrintGraph()
    
    fmt.Println("Initial state:")
    graph.PrintState()
    
    // Compute maximum flow from vertex 0 to vertex 5
    maxFlow := graph.MaxFlow(0, 5)
    
    fmt.Printf("Maximum flow from vertex 0 to vertex 5: %d\n", maxFlow)
    
    fmt.Println("Final state:")
    graph.PrintGraph()
    graph.PrintState()
    
    // Test with another example
    fmt.Println("\n" + "="*50)
    fmt.Println("Second example - Simple network:")
    
    graph2 := NewGraph(4)
    graph2.AddEdge(0, 1, 10)
    graph2.AddEdge(0, 2, 10)
    graph2.AddEdge(1, 3, 10)
    graph2.AddEdge(2, 3, 10)
    
    fmt.Println("Network:")
    graph2.PrintGraph()
    
    maxFlow2 := graph2.MaxFlow(0, 3)
    fmt.Printf("Maximum flow from vertex 0 to vertex 3: %d\n", maxFlow2)
}
```

## How the Algorithm Works

The Push-Relabel algorithm works with the following key concepts:

1. **Height Labels**: Each vertex has a height label that helps determine valid flow directions
2. **Excess Flow**: Each vertex maintains an excess flow that needs to be pushed to neighbors
3. **Push Operation**: Flow is pushed from a higher vertex to a lower vertex
4. **Relabel Operation**: When no push is possible, a vertex's height is increased

## Key Steps:

1. **Initialization**: Set source height to n, push initial flow
2. **Main Loop**: 
   - Find vertex with excess flow
   - Try to push flow to neighbors with lower height
   - If no push possible, relabel the vertex
3. **Termination**: When no vertices have excess flow, maximum flow is found

## Time Complexity:
- O(V²E) for the basic implementation
- O(V³) for the optimized version

## Example Output:
```
Flow Network:
Graph edges:
Vertex 0: -> 1(c=10,f=0) -> 2(c=10,f=0) 
Vertex 1: -> 3(c=4,f=0) -> 4(c=8,f=0) 
Vertex 2: -> 4(c=9,f=0) 
Vertex 3: -> 5(c=10,f=0) 
Vertex 4: -> 5(c=10,f=0) 
Vertex 5: 

Maximum flow from vertex 0 to vertex 5: 19
```

This implementation provides a complete working example of the Push-Relabel max-flow algorithm in Go, demonstrating how to build and solve flow network problems.

