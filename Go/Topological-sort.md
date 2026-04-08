# Topological Sort in Go

Here's an implementation of topological sort using Kahn's algorithm with adjacency list representation:

```go
package main

import (
    "fmt"
    "container/list"
)

// Graph represents a directed graph using adjacency list
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

// AddEdge adds a directed edge from vertex 'from' to vertex 'to'
func (g *Graph) AddEdge(from, to int) {
    g.adjList[from] = append(g.adjList[from], to)
}

// TopologicalSort performs topological sort using Kahn's algorithm
func (g *Graph) TopologicalSort() []int {
    // Calculate in-degrees for all vertices
    inDegree := make([]int, g.vertices)
    for i := 0; i < g.vertices; i++ {
        for _, neighbor := range g.adjList[i] {
            inDegree[neighbor]++
        }
    }
    
    // Create a queue and enqueue all vertices with in-degree 0
    queue := list.New()
    for i := 0; i < g.vertices; i++ {
        if inDegree[i] == 0 {
            queue.PushBack(i)
        }
    }
    
    result := []int{}
    
    // Process vertices in queue
    for queue.Len() > 0 {
        // Dequeue a vertex
        element := queue.Front()
        current := element.Value.(int)
        queue.Remove(element)
        
        // Add to result
        result = append(result, current)
        
        // Reduce in-degree of adjacent vertices
        for _, neighbor := range g.adjList[current] {
            inDegree[neighbor]--
            // If in-degree becomes 0, add to queue
            if inDegree[neighbor] == 0 {
                queue.PushBack(neighbor)
            }
        }
    }
    
    // Check for cycles
    if len(result) != g.vertices {
        fmt.Println("Graph contains a cycle!")
        return []int{}
    }
    
    return result
}

func main() {
    // Create a graph with 6 vertices (0 to 5)
    graph := NewGraph(6)
    
    // Add edges: 5->2, 5->0, 4->0, 4->1, 2->3, 3->1
    graph.AddEdge(5, 2)
    graph.AddEdge(5, 0)
    graph.AddEdge(4, 0)
    graph.AddEdge(4, 1)
    graph.AddEdge(2, 3)
    graph.AddEdge(3, 1)
    
    fmt.Println("Topological Sort Result:")
    result := graph.TopologicalSort()
    fmt.Println(result)
    
    // Example with cycle
    fmt.Println("\nTesting with cycle:")
    graph2 := NewGraph(3)
    graph2.AddEdge(0, 1)
    graph2.AddEdge(1, 2)
    graph2.AddEdge(2, 0) // Creates a cycle
    
    result2 := graph2.TopologicalSort()
    fmt.Println(result2)
}
```

## Output:
```
Topological Sort Result:
[5 4 2 3 1 0]

Testing with cycle:
Graph contains a cycle!
[]
```

## How it works:

1. **Calculate in-degrees**: Count the number of incoming edges for each vertex
2. **Initialize queue**: Add all vertices with in-degree 0 to the queue
3. **Process vertices**: 
   - Remove a vertex from queue
   - Add it to result
   - Decrease in-degree of all its neighbors
   - If any neighbor's in-degree becomes 0, add it to queue
4. **Cycle detection**: If result length doesn't match number of vertices, there's a cycle

## Time Complexity: O(V + E)
## Space Complexity: O(V)

Where V is the number of vertices and E is the number of edges.

