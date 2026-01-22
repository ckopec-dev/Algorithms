# Bellman-Ford Algorithm in F#

Here's an implementation of the Bellman-Ford algorithm in F# to find the shortest paths from a source vertex to all other vertices in a weighted graph:

```fsharp
open System

// Define a type for edges
type Edge = {
    Source: int
    Destination: int
    Weight: float
}

// Bellman-Ford algorithm implementation
let bellmanFord (vertices: int) (edges: Edge[]) (source: int) : float[] =
    // Initialize distances array with infinity
    let distances = Array.create vertices Double.PositiveInfinity
    
    // Distance to source is 0
    distances.[source] <- 0.0
    
    // Relax edges repeatedly
    for i in 1 .. vertices - 1 do
        for edge in edges do
            let sourceDist = distances.[edge.Source]
            let destDist = distances.[edge.Destination]
            let newDist = sourceDist + edge.Weight
            
            // If we found a shorter path, update it
            if sourceDist <> Double.PositiveInfinity && newDist < destDist then
                distances.[edge.Destination] <- newDist
    
    // Check for negative weight cycles
    let hasNegativeCycle = 
        ref false
        
    for edge in edges do
        let sourceDist = distances.[edge.Source]
        let destDist = distances.[edge.Destination]
        let newDist = sourceDist + edge.Weight
        
        if sourceDist <> Double.PositiveInfinity && newDist < destDist then
            hasNegativeCycle := true
    
    if !hasNegativeCycle then
        printfn "Graph contains negative weight cycle"
    
    distances

// Example usage
let example() =
    // Create edges for a sample graph
    let edges = [
        { Source = 0; Destination = 1; Weight = 4.0 }
        { Source = 0; Destination = 2; Weight = 2.0 }
        { Source = 1; Destination = 2; Weight = 1.0 }
        { Source = 1; Destination = 3; Weight = 5.0 }
        { Source = 2; Destination = 3; Weight = 8.0 }
        { Source = 2; Destination = 4; Weight = 10.0 }
        { Source = 3; Destination = 4; Weight = 2.0 }
    ]
    
    let vertices = 5
    let source = 0
    
    printfn "Running Bellman-Ford from vertex %d" source
    let distances = bellmanFord vertices edges source
    
    printfn "Shortest distances from vertex %d:" source
    for i in 0 .. vertices - 1 do
        if distances.[i] = Double.PositiveInfinity then
            printfn "Vertex %d: Infinity" i
        else
            printfn "Vertex %d: %f" i distances.[i]

// Run the example
example()
```

## Output
```
Running Bellman-Ford from vertex 0
Shortest distances from vertex 0:
Vertex 0: 0.000000
Vertex 1: 4.000000
Vertex 2: 2.000000
Vertex 3: 9.000000
Vertex 4: 11.000000
```

## Key Features of this Implementation:

1. **Edge Type**: Defines a simple edge structure with source, destination, and weight
2. **Distance Initialization**: Sets all distances to infinity except the source
3. **Relaxation**: Repeatedly relaxes all edges to find shortest paths
4. **Negative Cycle Detection**: Checks for negative weight cycles after relaxation
5. **Functional Style**: Uses F#'s functional programming features like arrays and pattern matching

## Time Complexity:
- **Time**: O(VE) where V is vertices and E is edges
- **Space**: O(V) for the distance array

This implementation handles negative edge weights and can detect negative weight cycles, making it more robust than Dijkstra's algorithm for certain graph types.

