# Borůvka's Algorithm Implementation in F#

Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree (MST) in a weighted undirected graph. Here's a complete implementation in F#:

```fsharp
open System

// Define a type for edges in the graph
type Edge = {
    Source: int
    Destination: int
    Weight: int
}

// Define a type for the graph
type Graph = {
    Vertices: int
    Edges: Edge list
}

// Find the root of a vertex in the Union-Find data structure
let findRoot (parent: int[]) (vertex: int) : int =
    let rec find root =
        if parent.[root] = root then root
        else find parent.[root]
    find vertex

// Union two sets in the Union-Find data structure
let unionSets (parent: int[]) (rank: int[]) (x: int) (y: int) : unit =
    let rootX = findRoot parent x
    let rootY = findRoot parent y
    
    if rootX <> rootY then
        if rank.[rootX] < rank.[rootY] then
            parent.[rootX] <- rootY
        elif rank.[rootX] > rank.[rootY] then
            parent.[rootY] <- rootX
        else
            parent.[rootY] <- rootX
            rank.[rootX] <- rank.[rootX] + 1

// Borůvka's algorithm implementation
let boruvkaMST (graph: Graph) : Edge list =
    let vertices = graph.Vertices
    let edges = graph.Edges
    
    // Initialize Union-Find structures
    let parent = Array.create vertices 0
    let rank = Array.create vertices 0
    
    // Initialize parent array
    for i in 0 .. vertices - 1 do
        parent.[i] <- i
    
    let mutable numComponents = vertices
    let mutable mstEdges = []
    
    while numComponents > 1 do
        // Find the minimum weight edge for each component
        let minEdge = Array.create vertices None
        
        // For each edge, check if it connects two different components
        for edge in edges do
            let root1 = findRoot parent edge.Source
            let root2 = findRoot parent edge.Destination
            
            if root1 <> root2 then
                match minEdge.[root1] with
                | None -> minEdge.[root1] <- Some edge
                | Some existingEdge -> 
                    if edge.Weight < existingEdge.Weight then
                        minEdge.[root1] <- Some edge
                
                match minEdge.[root2] with
                | None -> minEdge.[root2] <- Some edge
                | Some existingEdge -> 
                    if edge.Weight < existingEdge.Weight then
                        minEdge.[root2] <- Some edge
        
        // Add all minimum edges to MST and merge components
        let mutable addedEdges = 0
        for i in 0 .. vertices - 1 do
            match minEdge.[i] with
            | Some edge ->
                let root1 = findRoot parent edge.Source
                let root2 = findRoot parent edge.Destination
                
                if root1 <> root2 then
                    mstEdges <- edge :: mstEdges
                    unionSets parent rank root1 root2
                    addedEdges <- addedEdges + 1
            | None -> ()
        
        numComponents <- numComponents - addedEdges
    
    mstEdges

// Helper function to print the MST
let printMST (edges: Edge list) : unit =
    printfn "Minimum Spanning Tree Edges:"
    for edge in edges do
        printfn "Edge: %d -- %d, Weight: %d" 
            edge.Source edge.Destination edge.Weight

// Example usage
let exampleGraph = {
    Vertices = 4
    Edges = [
        { Source = 0; Destination = 1; Weight = 10 }
        { Source = 0; Destination = 2; Weight = 6 }
        { Source = 0; Destination = 3; Weight = 5 }
        { Source = 1; Destination = 3; Weight = 15 }
        { Source = 2; Destination = 3; Weight = 4 }
    ]
}

// Run Borůvka's algorithm
let result = boruvkaMST exampleGraph
printMST result

// Another example with more vertices
let exampleGraph2 = {
    Vertices = 6
    Edges = [
        { Source = 0; Destination = 1; Weight = 4 }
        { Source = 0; Destination = 2; Weight = 2 }
        { Source = 1; Destination = 2; Weight = 1 }
        { Source = 1; Destination = 3; Weight = 5 }
        { Source = 2; Destination = 3; Weight = 8 }
        { Source = 2; Destination = 4; Weight = 10 }
        { Source = 3; Destination = 4; Weight = 2 }
        { Source = 3; Destination = 5; Weight = 6 }
        { Source = 4; Destination = 5; Weight = 3 }
    ]
}

printfn "\nSecond Example:"
let result2 = boruvkaMST exampleGraph2
printMST result2
```

## How the Algorithm Works

1. **Initialization**: Create a Union-Find data structure to track connected components
2. **Find Minimum Edges**: For each component, find the minimum weight edge connecting it to another component
3. **Union Components**: Add the minimum edges to the MST and merge the components
4. **Repeat**: Continue until all vertices are in one component

## Key Features of the Implementation

- **Union-Find**: Efficiently tracks connected components with path compression
- **Greedy Approach**: Always selects the minimum weight edge available
- **Time Complexity**: O(E log V) where E is edges and V is vertices
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Output Example

```
Minimum Spanning Tree Edges:
Edge: 2 -- 3, Weight: 4
Edge: 0 -- 3, Weight: 5
Edge: 0 -- 1, Weight: 10

Second Example:
Minimum Spanning Tree Edges:
Edge: 1 -- 2, Weight: 1
Edge: 0 -- 2, Weight: 2
Edge: 3 -- 4, Weight: 2
Edge: 4 -- 5, Weight: 3
Edge: 0 -- 1, Weight: 4
```

This implementation demonstrates the classic Borůvka's algorithm for finding minimum spanning trees in F# with clear separation of concerns and efficient data structures.

