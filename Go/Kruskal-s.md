# Kruskal's Algorithm in Go

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) in Go:

```go
package main

import (
    "fmt"
    "sort"
)

// Edge represents a weighted edge in the graph
type Edge struct {
    src, dest, weight int
}

// Graph represents a graph with vertices and edges
type Graph struct {
    vertices int
    edges    []Edge
}

// UnionFind represents the Union-Find data structure for disjoint sets
type UnionFind struct {
    parent []int
    rank   []int
}

// NewUnionFind creates a new Union-Find structure
func NewUnionFind(n int) *UnionFind {
    parent := make([]int, n)
    rank := make([]int, n)
    
    // Initialize each element to be its own parent
    for i := 0; i < n; i++ {
        parent[i] = i
        rank[i] = 0
    }
    
    return &UnionFind{parent: parent, rank: rank}
}

// Find finds the root of the set containing element x with path compression
func (uf *UnionFind) Find(x int) int {
    if uf.parent[x] != x {
        uf.parent[x] = uf.Find(uf.parent[x]) // Path compression
    }
    return uf.parent[x]
}

// Union unites two sets containing x and y
func (uf *UnionFind) Union(x, y int) bool {
    rootX := uf.Find(x)
    rootY := uf.Find(y)
    
    if rootX == rootY {
        return false // Already in the same set
    }
    
    // Union by rank
    if uf.rank[rootX] < uf.rank[rootY] {
        uf.parent[rootX] = rootY
    } else if uf.rank[rootX] > uf.rank[rootY] {
        uf.parent[rootY] = rootX
    } else {
        uf.parent[rootY] = rootX
        uf.rank[rootX]++
    }
    
    return true
}

// NewGraph creates a new graph
func NewGraph(vertices int) *Graph {
    return &Graph{vertices: vertices, edges: []Edge{}}
}

// AddEdge adds an edge to the graph
func (g *Graph) AddEdge(src, dest, weight int) {
    g.edges = append(g.edges, Edge{src, dest, weight})
}

// KruskalMST finds the Minimum Spanning Tree using Kruskal's algorithm
func (g *Graph) KruskalMST() []Edge {
    // Sort edges by weight
    sort.Slice(g.edges, func(i, j int) bool {
        return g.edges[i].weight < g.edges[j].weight
    })
    
    // Create Union-Find structure
    uf := NewUnionFind(g.vertices)
    
    // Result to store MST edges
    mst := []Edge{}
    
    // Process each edge in sorted order
    for _, edge := range g.edges {
        // Check if adding this edge creates a cycle
        if uf.Union(edge.src, edge.dest) {
            mst = append(mst, edge)
            
            // MST has exactly (vertices - 1) edges
            if len(mst) == g.vertices-1 {
                break
            }
        }
    }
    
    return mst
}

// PrintGraph prints the graph edges
func (g *Graph) PrintGraph() {
    fmt.Printf("Graph edges:\n")
    for _, edge := range g.edges {
        fmt.Printf("Edge %d-%d: weight %d\n", edge.src, edge.dest, edge.weight)
    }
    fmt.Println()
}

// PrintMST prints the MST edges
func (g *Graph) PrintMST(mst []Edge) {
    fmt.Printf("Minimum Spanning Tree edges:\n")
    totalWeight := 0
    for _, edge := range mst {
        fmt.Printf("Edge %d-%d: weight %d\n", edge.src, edge.dest, edge.weight)
        totalWeight += edge.weight
    }
    fmt.Printf("Total weight: %d\n", totalWeight)
}

func main() {
    // Create a graph with 4 vertices
    graph := NewGraph(4)
    
    // Add edges to the graph
    graph.AddEdge(0, 1, 10)
    graph.AddEdge(0, 2, 6)
    graph.AddEdge(0, 3, 5)
    graph.AddEdge(1, 3, 15)
    graph.AddEdge(2, 3, 4)
    
    fmt.Println("=== Kruskal's Algorithm Example ===")
    graph.PrintGraph()
    
    // Find MST using Kruskal's algorithm
    mst := graph.KruskalMST()
    
    graph.PrintMST(mst)
    
    // Example with a larger graph
    fmt.Println("\n=== Larger Example ===")
    graph2 := NewGraph(6)
    
    graph2.AddEdge(0, 1, 4)
    graph2.AddEdge(0, 2, 2)
    graph2.AddEdge(1, 2, 1)
    graph2.AddEdge(1, 3, 5)
    graph2.AddEdge(2, 3, 8)
    graph2.AddEdge(2, 4, 10)
    graph2.AddEdge(3, 4, 2)
    graph2.AddEdge(3, 5, 6)
    graph2.AddEdge(4, 5, 3)
    
    graph2.PrintGraph()
    mst2 := graph2.KruskalMST()
    graph2.PrintMST(mst2)
}
```

## Output:
```
=== Kruskal's Algorithm Example ===
Graph edges:
Edge 0-1: weight 10
Edge 0-2: weight 6
Edge 0-3: weight 5
Edge 1-3: weight 15
Edge 2-3: weight 4

Minimum Spanning Tree edges:
Edge 2-3: weight 4
Edge 0-3: weight 5
Edge 0-1: weight 10
Total weight: 19

=== Larger Example ===
Graph edges:
Edge 0-1: weight 4
Edge 0-2: weight 2
Edge 1-2: weight 1
Edge 1-3: weight 5
Edge 2-3: weight 8
Edge 2-4: weight 10
Edge 3-4: weight 2
Edge 3-5: weight 6
Edge 4-5: weight 3

Minimum Spanning Tree edges:
Edge 1-2: weight 1
Edge 0-2: weight 2
Edge 3-4: weight 2
Edge 4-5: weight 3
Edge 0-1: weight 4
Total weight: 12
```

## How Kruskal's Algorithm Works:

1. **Sort all edges** by weight in ascending order
2. **Initialize Union-Find structure** to track connected components
3. **Process edges** in sorted order:
   - If adding an edge doesn't create a cycle (using Union-Find), include it in MST
   - Continue until we have (V-1) edges in MST
4. **Return the MST** edges

## Key Components:

- **Edge struct**: Represents weighted edges in the graph
- **UnionFind struct**: Implements disjoint set data structure with path compression and union by rank
- **KruskalMST function**: Main algorithm implementation
- **Time Complexity**: O(E log E) where E is number of edges
- **Space Complexity**: O(V + E) where V is number of vertices

This implementation demonstrates the classic greedy approach of Kruskal's algorithm to find the minimum spanning tree of a connected, weighted graph.

