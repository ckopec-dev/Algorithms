# Minimum Spanning Tree Algorithm in Go

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) using Go:

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

// UnionFind represents the Union-Find data structure for disjoint sets
type UnionFind struct {
    parent []int
    rank   []int
}

// NewUnionFind creates a new UnionFind structure
func NewUnionFind(n int) *UnionFind {
    parent := make([]int, n)
    rank := make([]int, n)
    
    // Initialize each node as its own parent
    for i := 0; i < n; i++ {
        parent[i] = i
        rank[i] = 0
    }
    
    return &UnionFind{parent: parent, rank: rank}
}

// Find finds the root of the set containing x with path compression
func (uf *UnionFind) Find(x int) int {
    if uf.parent[x] != x {
        uf.parent[x] = uf.Find(uf.parent[x]) // Path compression
    }
    return uf.parent[x]
}

// Union unites two sets and returns true if they were different sets
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

// KruskalMST finds the Minimum Spanning Tree using Kruskal's algorithm
func KruskalMST(vertices int, edges []Edge) []Edge {
    // Sort edges by weight
    sort.Slice(edges, func(i, j int) bool {
        return edges[i].weight < edges[j].weight
    })
    
    // Create Union-Find structure
    uf := NewUnionFind(vertices)
    
    var mst []Edge
    edgeCount := 0
    
    // Process each edge in sorted order
    for i := 0; i < len(edges) && edgeCount < vertices-1; i++ {
        edge := edges[i]
        
        // Check if adding this edge creates a cycle
        if uf.Union(edge.src, edge.dest) {
            mst = append(mst, edge)
            edgeCount++
        }
    }
    
    return mst
}

func main() {
    // Example graph with 4 vertices and 5 edges
    vertices := 4
    edges := []Edge{
        {0, 1, 10},
        {0, 2, 6},
        {0, 3, 5},
        {1, 3, 15},
        {2, 3, 4},
    }
    
    fmt.Println("Original edges (src, dest, weight):")
    for _, edge := range edges {
        fmt.Printf("(%d, %d, %d)\n", edge.src, edge.dest, edge.weight)
    }
    
    // Find MST
    mst := KruskalMST(vertices, edges)
    
    fmt.Println("\nMinimum Spanning Tree edges:")
    totalWeight := 0
    for _, edge := range mst {
        fmt.Printf("(%d, %d, %d)\n", edge.src, edge.dest, edge.weight)
        totalWeight += edge.weight
    }
    
    fmt.Printf("\nTotal weight of MST: %d\n", totalWeight)
}
```

## Output:
```
Original edges (src, dest, weight):
(0, 1, 10)
(0, 2, 6)
(0, 3, 5)
(1, 3, 15)
(2, 3, 4)

Minimum Spanning Tree edges:
(2, 3, 4)
(0, 3, 5)
(0, 1, 10)

Total weight of MST: 19
```

## Algorithm Explanation:

1. **Edge Sorting**: All edges are sorted by weight in ascending order
2. **Union-Find Initialization**: Create a Union-Find structure to track connected components
3. **Edge Processing**: Process each edge from smallest to largest weight
4. **Cycle Detection**: Use Union-Find to check if adding an edge creates a cycle
5. **MST Construction**: If no cycle is created, add the edge to MST

## Time Complexity:
- Sorting edges: O(E log E)
- Union-Find operations: O(E α(V)) where α is the inverse Ackermann function
- Overall: O(E log E)

## Space Complexity:
- O(V + E) for storing the graph and Union-Find structure

This implementation demonstrates Kruskal's algorithm, which is one of the most common approaches for finding minimum spanning trees in weighted graphs.

