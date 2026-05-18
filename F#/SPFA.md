# SPFA Algorithm in F#

Here's an implementation of the Shortest Path Faster Algorithm (SPFA) in F#:

```fsharp
open System
open System.Collections.Generic

// Define a graph edge
type Edge = {
    Source: int
    Destination: int
    Weight: int
}

// SPFA algorithm implementation
let spfa (graph: Edge list) (numVertices: int) (startVertex: int) : int[] =
    // Initialize distances array with infinity
    let distances = Array.create numVertices Int32.MaxValue
    
    // Initialize queue for BFS-like processing
    let queue = new Queue<int>()
    
    // Track vertices in queue to avoid duplicates
    let inQueue = Array.create numVertices false
    
    // Set start vertex distance to 0
    distances.[startVertex] <- 0
    queue.Enqueue(startVertex)
    inQueue.[startVertex] <- true
    
    // Process vertices
    while queue.Count > 0 do
        let current = queue.Dequeue()
        inQueue.[current] <- false
        
        // Check all edges from current vertex
        let edgesFromCurrent = 
            graph 
            |> List.filter (fun e -> e.Source = current)
        
        for edge in edgesFromCurrent do
            let newDistance = distances.[current] + edge.Weight
            
            // If we found a shorter path, update distance and queue
            if newDistance < distances.[edge.Destination] then
                distances.[edge.Destination] <- newDistance
                
                // Add to queue if not already there
                if not inQueue.[edge.Destination] then
                    queue.Enqueue(edge.Destination)
                    inQueue.[edge.Destination] <- true
    
    distances

// Example usage
let example() =
    // Create a sample graph
    let edges = [
        { Source = 0; Destination = 1; Weight = 4 }
        { Source = 0; Destination = 2; Weight = 2 }
        { Source = 1; Destination = 2; Weight = 1 }
        { Source = 1; Destination = 3; Weight = 5 }
        { Source = 2; Destination = 3; Weight = 8 }
        { Source = 2; Destination = 4; Weight = 10 }
        { Source = 3; Destination = 4; Weight = 2 }
    ]
    
    let numVertices = 5
    let startVertex = 0
    
    // Run SPFA
    let result = spfa edges numVertices startVertex
    
    // Print results
    printfn "Shortest distances from vertex %d:" startVertex
    for i in 0 .. numVertices - 1 do
        if result.[i] = Int32.MaxValue then
            printfn "Vertex %d: Infinity" i
        else
            printfn "Vertex %d: %d" i result.[i]

// Run the example
example()
```

## Output
```
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses a list of edges with source, destination, and weight
2. **Queue-based Processing**: Implements BFS-like queue processing for efficient updates
3. **Duplicate Prevention**: Uses `inQueue` array to prevent vertices from being processed multiple times
4. **Distance Updates**: Only updates distances when a shorter path is found
5. **Return Format**: Returns an array where each index represents the shortest distance to that vertex

## Time Complexity:
- **Best Case**: O(E) where E is the number of edges
- **Average Case**: O(E) 
- **Worst Case**: O(VE) where V is the number of vertices

## Space Complexity: O(V + E)

The SPFA algorithm is particularly useful for graphs with negative edge weights and can detect negative cycles, making it more versatile than Dijkstra's algorithm in certain scenarios.

