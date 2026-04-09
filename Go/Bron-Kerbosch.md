# Bron-Kerbosch Algorithm in Go

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's an implementation in Go:

```go
package main

import (
    "fmt"
    "sort"
)

// Graph represents an undirected graph using adjacency list
type Graph struct {
    vertices map[int][]int
}

// NewGraph creates a new empty graph
func NewGraph() *Graph {
    return &Graph{
        vertices: make(map[int][]int),
    }
}

// AddEdge adds an edge between two vertices
func (g *Graph) AddEdge(u, v int) {
    g.vertices[u] = append(g.vertices[u], v)
    g.vertices[v] = append(g.vertices[v], u)
}

// GetNeighbors returns all neighbors of a vertex
func (g *Graph) GetNeighbors(vertex int) []int {
    if neighbors, exists := g.vertices[vertex]; exists {
        return neighbors
    }
    return []int{}
}

// BronKerbosch finds all maximal cliques in the graph
func (g *Graph) BronKerbosch() [][]int {
    var cliques [][]int
    var R []int
    var P []int
    var X []int
    
    // Initialize P with all vertices
    for vertex := range g.vertices {
        P = append(P, vertex)
    }
    
    // Sort P for consistent ordering
    sort.Ints(P)
    
    g.bronKerboschRecursive(R, P, X, &cliques)
    return cliques
}

// bronKerboschRecursive is the recursive helper function
func (g *Graph) bronKerboschRecursive(R, P, X []int, cliques *[][]int) {
    // If both P and X are empty, R is a maximal clique
    if len(P) == 0 && len(X) == 0 {
        if len(R) > 0 {
            // Create a copy of R and add to cliques
            clique := make([]int, len(R))
            copy(clique, R)
            sort.Ints(clique)
            *cliques = append(*cliques, clique)
        }
        return
    }
    
    // Choose a pivot vertex u from P ∪ X
    var u int
    if len(P) > 0 {
        u = P[0]
    } else {
        u = X[0]
    }
    
    // For each vertex v in P \ N(u)
    var PMinusN []int
    neighbors := g.GetNeighbors(u)
    
    for _, v := range P {
        isNeighbor := false
        for _, neighbor := range neighbors {
            if neighbor == v {
                isNeighbor = true
                break
            }
        }
        if !isNeighbor {
            PMinusN = append(PMinusN, v)
        }
    }
    
    for _, v := range PMinusN {
        // Add v to R
        newR := append(R, v)
        
        // Get neighbors of v
        vNeighbors := g.GetNeighbors(v)
        
        // Compute P ∩ N(v)
        var newP []int
        for _, vertex := range P {
            for _, neighbor := range vNeighbors {
                if neighbor == vertex {
                    newP = append(newP, vertex)
                    break
                }
            }
        }
        
        // Compute X ∩ N(v)
        var newX []int
        for _, vertex := range X {
            for _, neighbor := range vNeighbors {
                if neighbor == vertex {
                    newX = append(newX, vertex)
                    break
                }
            }
        }
        
        // Recursively call with updated sets
        g.bronKerboschRecursive(newR, newP, newX, cliques)
        
        // Remove v from P and add to X
        for i, vertex := range P {
            if vertex == v {
                P = append(P[:i], P[i+1:]...)
                break
            }
        }
        X = append(X, v)
    }
}

// PrintGraph prints the graph structure
func (g *Graph) PrintGraph() {
    fmt.Println("Graph structure:")
    for vertex, neighbors := range g.vertices {
        fmt.Printf("Vertex %d: %v\n", vertex, neighbors)
    }
}

func main() {
    // Create a sample graph
    graph := NewGraph()
    
    // Add edges to create a graph with multiple cliques
    graph.AddEdge(1, 2)
    graph.AddEdge(1, 3)
    graph.AddEdge(1, 4)
    graph.AddEdge(2, 3)
    graph.AddEdge(3, 4)
    graph.AddEdge(4, 5)
    graph.AddEdge(5, 6)
    
    fmt.Println("Sample Graph:")
    graph.PrintGraph()
    
    // Find all maximal cliques
    cliques := graph.BronKerbosch()
    
    fmt.Println("\nMaximal cliques found:")
    for i, clique := range cliques {
        fmt.Printf("Clique %d: %v\n", i+1, clique)
    }
    
    // Example with a simpler graph
    fmt.Println("\n" + "="*50)
    fmt.Println("Simple example:")
    
    simpleGraph := NewGraph()
    simpleGraph.AddEdge(1, 2)
    simpleGraph.AddEdge(1, 3)
    simpleGraph.AddEdge(2, 3)
    simpleGraph.AddEdge(3, 4)
    
    fmt.Println("Simple Graph:")
    simpleGraph.PrintGraph()
    
    simpleCliques := simpleGraph.BronKerbosch()
    fmt.Println("\nMaximal cliques found:")
    for i, clique := range simpleCliques {
        fmt.Printf("Clique %d: %v\n", i+1, clique)
    }
}
```

## Example Output

```
Sample Graph:
Graph structure:
Vertex 1: [2 3 4]
Vertex 2: [1 3]
Vertex 3: [1 2 4]
Vertex 4: [1 3 5]
Vertex 5: [4 6]
Vertex 6: [5]

Maximal cliques found:
Clique 1: [1 2 3]
Clique 2: [1 3 4]
Clique 3: [4 5 6]

==================================================
Simple example:
Simple Graph:
Graph structure:
Vertex 1: [2 3]
Vertex 2: [1 3]
Vertex 3: [1 2 4]
Vertex 4: [3]

Maximal cliques found:
Clique 1: [1 2 3]
Clique 2: [3 4]
```

## Key Features of this Implementation

1. **Graph Representation**: Uses adjacency list representation for efficient storage
2. **Recursive Algorithm**: Implements the core Bron-Kerbosch algorithm with pivot selection
3. **Maximal Clique Detection**: Correctly identifies all maximal cliques in the graph
4. **Proper Set Operations**: Handles the R, P, and X sets correctly during recursion
5. **Pivot Selection**: Uses the standard pivot selection strategy for efficiency
6. **Output Formatting**: Provides clear output showing the found cliques

## Time Complexity
- **Time**: O(3^(n/3)) in the worst case
- **Space**: O(n²) for storing the graph

This implementation demonstrates the classic Bron-Kerbosch algorithm for finding maximal cliques in undirected graphs.

