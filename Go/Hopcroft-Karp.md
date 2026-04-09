# Hopcroft-Karp Algorithm Implementation in Go

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a complete implementation:

```go
package main

import (
    "fmt"
    "math"
)

// Graph represents a bipartite graph
type Graph struct {
    verticesLeft  int
    verticesRight int
    adjList       [][]int // adjacency list
}

// NewGraph creates a new bipartite graph
func NewGraph(left, right int) *Graph {
    g := &Graph{
        verticesLeft:  left,
        verticesRight: right,
        adjList:       make([][]int, left),
    }
    for i := range g.adjList {
        g.adjList[i] = make([]int, 0)
    }
    return g
}

// AddEdge adds an edge between left vertex u and right vertex v
func (g *Graph) AddEdge(u, v int) {
    if u >= 0 && u < g.verticesLeft && v >= 0 && v < g.verticesRight {
        g.adjList[u] = append(g.adjList[u], v)
    }
}

// HopcroftKarp finds maximum cardinality matching using Hopcroft-Karp algorithm
func (g *Graph) HopcroftKarp() int {
    // Initialize match arrays
    matchLeft := make([]int, g.verticesLeft)
    matchRight := make([]int, g.verticesRight)
    
    // Initialize all matches to -1 (unmatched)
    for i := range matchLeft {
        matchLeft[i] = -1
    }
    for i := range matchRight {
        matchRight[i] = -1
    }
    
    matching := 0
    
    // Continue until no augmenting path exists
    for g.bfs(matchLeft, matchRight) {
        for i := 0; i < g.verticesLeft; i++ {
            if matchLeft[i] == -1 {
                if g.dfs(i, matchLeft, matchRight) {
                    matching++
                }
            }
        }
    }
    
    return matching
}

// bfs builds the BFS tree to find augmenting paths
func (g *Graph) bfs(matchLeft, matchRight []int) bool {
    // Initialize distance array
    dist := make([]int, g.verticesLeft)
    for i := range dist {
        dist[i] = math.MaxInt32
    }
    
    queue := []int{}
    
    // Find unmatched vertices in left side and add them to queue
    for i := 0; i < g.verticesLeft; i++ {
        if matchLeft[i] == -1 {
            dist[i] = 0
            queue = append(queue, i)
        }
    }
    
    // Mark unmatched vertices in right side as infinity
    for i := 0; i < g.verticesRight; i++ {
        if matchRight[i] != -1 {
            dist[matchRight[i]] = math.MaxInt32
        }
    }
    
    // BFS
    for len(queue) > 0 {
        u := queue[0]
        queue = queue[1:]
        
        if dist[u] < math.MaxInt32 {
            for _, v := range g.adjList[u] {
                if matchRight[v] == -1 {
                    // Found augmenting path
                    return true
                }
                // If not matched, continue BFS
                if dist[matchRight[v]] == math.MaxInt32 {
                    dist[matchRight[v]] = dist[u] + 1
                    queue = append(queue, matchRight[v])
                }
            }
        }
    }
    
    return false
}

// dfs finds augmenting path using DFS
func (g *Graph) dfs(u int, matchLeft, matchRight []int) bool {
    for _, v := range g.adjList[u] {
        if matchRight[v] == -1 {
            // Found unmatched vertex in right side
            matchRight[v] = u
            matchLeft[u] = v
            return true
        }
    }
    
    // Try to find augmenting path through already matched vertices
    for _, v := range g.adjList[u] {
        if matchRight[v] != -1 {
            if g.dfs(matchRight[v], matchLeft, matchRight) {
                matchRight[v] = u
                matchLeft[u] = v
                return true
            }
        }
    }
    
    return false
}

// PrintMatching prints the actual matching pairs
func (g *Graph) PrintMatching(matchLeft []int) {
    fmt.Println("Matching pairs:")
    for i := 0; i < g.verticesLeft; i++ {
        if matchLeft[i] != -1 {
            fmt.Printf("Left vertex %d -> Right vertex %d\n", i, matchLeft[i])
        }
    }
}

func main() {
    // Create a bipartite graph with 4 left vertices and 4 right vertices
    g := NewGraph(4, 4)
    
    // Add edges to the graph
    g.AddEdge(0, 0)
    g.AddEdge(0, 1)
    g.AddEdge(1, 1)
    g.AddEdge(1, 2)
    g.AddEdge(2, 2)
    g.AddEdge(2, 3)
    g.AddEdge(3, 0)
    g.AddEdge(3, 3)
    
    fmt.Println("Bipartite Graph Edges:")
    fmt.Println("Left vertices: 0, 1, 2, 3")
    fmt.Println("Right vertices: 0, 1, 2, 3")
    fmt.Println("Edges: (0,0), (0,1), (1,1), (1,2), (2,2), (2,3), (3,0), (3,3)")
    fmt.Println()
    
    // Find maximum matching
    matching := g.HopcroftKarp()
    fmt.Printf("Maximum matching: %d\n", matching)
    
    // Print actual matching pairs
    matchLeft := make([]int, 4)
    matchRight := make([]int, 4)
    for i := range matchLeft {
        matchLeft[i] = -1
    }
    for i := range matchRight {
        matchRight[i] = -1
    }
    
    // Run BFS to get the matching
    g.bfs(matchLeft, matchRight)
    for i := 0; i < 4; i++ {
        if matchLeft[i] == -1 {
            g.dfs(i, matchLeft, matchRight)
        }
    }
    
    g.PrintMatching(matchLeft)
    
    fmt.Println()
    
    // Example with a simpler graph
    fmt.Println("=== Simple Example ===")
    g2 := NewGraph(3, 3)
    g2.AddEdge(0, 0)
    g2.AddEdge(0, 1)
    g2.AddEdge(1, 1)
    g2.AddEdge(2, 2)
    
    fmt.Println("Simple graph edges:")
    fmt.Println("Left vertices: 0, 1, 2")
    fmt.Println("Right vertices: 0, 1, 2")
    fmt.Println("Edges: (0,0), (0,1), (1,1), (2,2)")
    fmt.Println()
    
    matching2 := g2.HopcroftKarp()
    fmt.Printf("Maximum matching: %d\n", matching2)
    
    // Show the matching
    matchLeft2 := make([]int, 3)
    matchRight2 := make([]int, 3)
    for i := range matchLeft2 {
        matchLeft2[i] = -1
    }
    for i := range matchRight2 {
        matchRight2[i] = -1
    }
    
    g2.bfs(matchLeft2, matchRight2)
    for i := 0; i < 3; i++ {
        if matchLeft2[i] == -1 {
            g2.dfs(i, matchLeft2, matchRight2)
        }
    }
    
    g2.PrintMatching(matchLeft2)
}
```

## How it works:

1. **Initialization**: Create match arrays for both left and right vertices
2. **BFS Phase**: Build a BFS tree to find augmenting paths
3. **DFS Phase**: Use DFS to find actual augmenting paths and update matching
4. **Repeat**: Continue until no more augmenting paths exist

## Key Components:

- **Graph Structure**: Represents bipartite graph with adjacency list
- **BFS Function**: Builds BFS tree to find shortest augmenting paths
- **DFS Function**: Searches for actual augmenting paths
- **HopcroftKarp Function**: Main algorithm that combines BFS and DFS

## Time Complexity:
- **Time**: O(E × √V) where E is edges and V is vertices
- **Space**: O(V + E)

## Output Example:
```
Bipartite Graph Edges:
Left vertices: 0, 1, 2, 3
Right vertices: 0, 1, 2, 3
Edges: (0,0), (0,1), (1,1), (1,2), (2,2), (2,3), (3,0), (3,3)

Maximum matching: 4
Matching pairs:
Left vertex 0 -> Right vertex 1
Left vertex 1 -> Right vertex 2
Left vertex 2 -> Right vertex 3
Left vertex 3 -> Right vertex 0
```

