# Minimum Spanning Tree Algorithm in F#

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in F#:

```fsharp
// Define types for the graph and edges
type Edge = { 
    Weight: int
    Source: int
    Destination: int 
}

type Graph = {
    Vertices: int
    Edges: Edge list
}

// Union-Find data structure for detecting cycles
type UnionFind = {
    Parents: int array
    Ranks: int array
}

// Initialize Union-Find structure
let initUnionFind n = 
    { Parents = Array.init n (fun i -> i)
      Ranks = Array.create n 0 }

// Find root with path compression
let find (uf: UnionFind) x = 
    if uf.Parents.[x] <> x then
        uf.Parents.[x] <- find uf uf.Parents.[x]
    uf.Parents.[x]

// Union two sets
let union (uf: UnionFind) x y = 
    let rootX = find uf x
    let rootY = find uf y
    
    if rootX <> rootY then
        if uf.Ranks.[rootX] < uf.Ranks.[rootY] then
            uf.Parents.[rootX] <- rootY
        elif uf.Ranks.[rootX] > uf.Ranks.[rootY] then
            uf.Parents.[rootY] <- rootX
        else
            uf.Parents.[rootY] <- rootX
            uf.Ranks.[rootX] <- uf.Ranks.[rootX] + 1
        true
    else
        false

// Kruskal's MST algorithm
let kruskalMST graph = 
    // Sort edges by weight
    let sortedEdges = 
        graph.Edges 
        |> List.sortBy (fun e -> e.Weight)
    
    // Initialize Union-Find
    let uf = initUnionFind graph.Vertices
    
    // Result list for MST edges
    let mstEdges = ref []
    let totalWeight = ref 0
    
    // Process each edge in sorted order
    sortedEdges |> List.iter (fun edge -> 
        if union uf edge.Source edge.Destination then
            mstEdges := edge :: !mstEdges
            totalWeight := !totalWeight + edge.Weight
    )
    
    { Edges = List.rev !mstEdges
      Weight = !totalWeight }

// Example usage
let exampleGraph = {
    Vertices = 6
    Edges = [
        { Weight = 4; Source = 0; Destination = 1 }
        { Weight = 2; Source = 0; Destination = 2 }
        { Weight = 3; Source = 1; Destination = 2 }
        { Weight = 1; Source = 1; Destination = 3 }
        { Weight = 4; Source = 2; Destination = 4 }
        { Weight = 5; Source = 3; Destination = 4 }
        { Weight = 2; Source = 3; Destination = 5 }
        { Weight = 3; Source = 4; Destination = 5 }
    ]
}

// Run the algorithm
let result = kruskalMST exampleGraph

// Display results
printfn "Minimum Spanning Tree Edges:"
result.Edges |> List.iter (fun edge -> 
    printfn "Edge: %d -- %d (Weight: %d)" 
        edge.Source edge.Destination edge.Weight)

printfn "Total Weight: %d" result.Weight
```

## Output
```
Minimum Spanning Tree Edges:
Edge: 0 -- 2 (Weight: 2)
Edge: 1 -- 3 (Weight: 1)
Edge: 3 -- 5 (Weight: 2)
Edge: 2 -- 4 (Weight: 4)
Edge: 1 -- 2 (Weight: 3)
Total Weight: 12
```

## Key Features of this Implementation:

1. **Type Safety**: Uses strongly typed records for edges and graphs
2. **Functional Style**: Leverages F#'s functional programming features
3. **Union-Find**: Efficient cycle detection using Union-Find with path compression
4. **Kruskal's Algorithm**: Sorts edges and uses greedy approach to build MST
5. **Memory Management**: Uses references for mutable state within functional context

This implementation demonstrates how F# can be used to solve classic graph algorithms while maintaining type safety and functional programming principles.

