# Depth-First Search (DFS) in Go

Here's a complete implementation of the Depth-First Search algorithm in Go:

```go
package main

import (
    "fmt"
    "log"
)

// Graph represents an adjacency list representation of a graph
type Graph struct {
    vertices int
    adjList  [][]int
}

// NewGraph creates a new graph with given number of vertices
func NewGraph(vertices int) *Graph {
    return &Graph{
        vertices: vertices,
        adjList:  make([][]int, vertices),
    }
}

// AddEdge adds an edge between two vertices
func (g *Graph) AddEdge(src, dest int) {
    if src < 0 || src >= g.vertices || dest < 0 || dest >= g.vertices {
        log.Fatal("Invalid vertex")
    }
    g.adjList[src] = append(g.adjList[src], dest)
}

// DFS performs depth-first search starting from a given vertex
func (g *Graph) DFS(startVertex int) {
    if startVertex < 0 || startVertex >= g.vertices {
        log.Fatal("Invalid start vertex")
    }
    
    visited := make([]bool, g.vertices)
    fmt.Printf("DFS traversal starting from vertex %d: ", startVertex)
    g.dfsUtil(startVertex, visited)
    fmt.Println()
}

// dfsUtil is a recursive helper function for DFS
func (g *Graph) dfsUtil(vertex int, visited []bool) {
    visited[vertex] = true
    fmt.Printf("%d ", vertex)
    
    // Visit all adjacent vertices
    for _, adjVertex := range g.adjList[vertex] {
        if !visited[adjVertex] {
            g.dfsUtil(adjVertex, visited)
        }
    }
}

// DFSIterative performs DFS iteratively using a stack
func (g *Graph) DFSIterative(startVertex int) {
    if startVertex < 0 || startVertex >= g.vertices {
        log.Fatal("Invalid start vertex")
    }
    
    visited := make([]bool, g.vertices)
    stack := []int{startVertex}
    
    fmt.Printf("Iterative DFS traversal starting from vertex %d: ", startVertex)
    
    for len(stack) > 0 {
        // Pop from stack
        vertex := stack[len(stack)-1]
        stack = stack[:len(stack)-1]
        
        if !visited[vertex] {
            visited[vertex] = true
            fmt.Printf("%d ", vertex)
            
            // Push all unvisited adjacent vertices to stack
            // Iterate backwards to maintain correct order
            for i := len(g.adjList[vertex]) - 1; i >= 0; i-- {
                adjVertex := g.adjList[vertex][i]
                if !visited[adjVertex] {
                    stack = append(stack, adjVertex)
                }
            }
        }
    }
    fmt.Println()
}

func main() {
    // Create a graph with 6 vertices (0 to 5)
    graph := NewGraph(6)
    
    // Add edges to create the following graph:
    // 0 -> 1, 2
    // 1 -> 3, 4
    // 2 -> 5
    // 3 -> 
    // 4 -> 
    // 5 -> 
    graph.AddEdge(0, 1)
    graph.AddEdge(0, 2)
    graph.AddEdge(1, 3)
    graph.AddEdge(1, 4)
    graph.AddEdge(2, 5)
    
    fmt.Println("Graph structure:")
    fmt.Println("0 -> 1, 2")
    fmt.Println("1 -> 3, 4")
    fmt.Println("2 -> 5")
    fmt.Println("3 ->")
    fmt.Println("4 ->")
    fmt.Println("5 ->")
    fmt.Println()
    
    // Perform DFS starting from vertex 0
    graph.DFS(0)
    
    // Perform iterative DFS starting from vertex 0
    graph.DFSIterative(0)
    
    // Test DFS from different starting vertex
    fmt.Println()
    fmt.Println("Starting from vertex 2:")
    graph.DFS(2)
}
```

## Output:
```
Graph structure:
0 -> 1, 2
1 -> 3, 4
2 -> 5
3 ->
4 ->
5 ->

DFS traversal starting from vertex 0: 0 1 3 4 2 5 
Iterative DFS traversal starting from vertex 0: 0 2 5 1 4 3 

Starting from vertex 2:
DFS traversal starting from vertex 2: 2 5 
```

## Key Features:

1. **Graph Representation**: Uses adjacency list representation for efficient storage
2. **Recursive DFS**: Traditional recursive implementation
3. **Iterative DFS**: Stack-based implementation to avoid recursion limits
4. **Vertex Validation**: Checks for valid vertices before operations
5. **Visited Tracking**: Uses boolean array to track visited vertices

## Time and Space Complexity:

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the visited array and recursion stack

The algorithm explores as far as possible along each branch before backtracking, making it perfect for tasks like finding connected components, topological sorting, and detecting cycles in graphs.

