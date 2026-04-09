# Minimum Spanning Tree Algorithm in Go

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Go:

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
    
    // Result MST edges
    mst := []Edge{}
    
    // Process edges in sorted order
    for _, edge := range g.edges {
        // Check if adding this edge creates a cycle
        if uf.Union(edge.src, edge.dest) {
            mst = append(mst, edge)
            
            // MST has V-1 edges
            if len(mst) == g.vertices-1 {
                break
            }
        }
    }
    
    return mst
}

// PrintMST prints the MST edges
func PrintMST(mst []Edge) {
    fmt.Println("Minimum Spanning Tree Edges:")
    fmt.Println("Source Dest Weight")
    fmt.Println("-------------------")
    for _, edge := range mst {
        fmt.Printf("%d      %d     %d\n", edge.src, edge.dest, edge.weight)
    }
}

func main() {
    // Create a graph with 4 vertices
    graph := NewGraph(4)
    
    // Add edges (source, destination, weight)
    graph.AddEdge(0, 1, 10)
    graph.AddEdge(0, 2, 6)
    graph.AddEdge(0, 3, 5)
    graph.AddEdge(1, 3, 15)
    graph.AddEdge(2, 3, 4)
    
    fmt.Println("Graph edges:")
    fmt.Println("Source Dest Weight")
    fmt.Println("-------------------")
    for _, edge := range graph.edges {
        fmt.Printf("%d      %d     %d\n", edge.src, edge.dest, edge.weight)
    }
    fmt.Println()
    
    // Find MST
    mst := graph.KruskalMST()
    
    // Print result
    PrintMST(mst)
    
    // Calculate total weight
    totalWeight := 0
    for _, edge := range mst {
        totalWeight += edge.weight
    }
    fmt.Printf("\nTotal weight of MST: %d\n", totalWeight)
}
```

## Output:
```
Graph edges:
Source Dest Weight
-------------------
0      1     10
0      2     6
0      3     5
1      3     15
2      3     4

Minimum Spanning Tree Edges:
Source Dest Weight
-------------------
2      3     4
0      3     5
0      1     10

Total weight of MST: 19
```

## Algorithm Explanation:

1. **Kruskal's Algorithm**: Uses a greedy approach to find the MST
2. **Steps**:
   - Sort all edges by weight in ascending order
   - Initialize Union-Find structure for cycle detection
   - Process edges in sorted order
   - Add edge to MST if it doesn't create a cycle
   - Continue until V-1 edges are added

3. **Time Complexity**: O(E log E) where E is the number of edges
4. **Space Complexity**: O(V + E) where V is vertices and E is edges

This implementation demonstrates the classic MST problem solution using Kruskal's algorithm with Union-Find data structure for efficient cycle detection.

