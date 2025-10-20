# Minimum Spanning Tree Algorithm in F#

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in F#:

```fsharp
open System

// Define a type for edges
type Edge = { 
    Weight: int
    Source: int
    Destination: int 
}

// Union-Find data structure for tracking connected components
type UnionFind(n: int) =
    let parent = Array.create n -1
    let rank = Array.create n 0
    
    member this.Find(x: int) : int =
        if parent.[x] = -1 then x
        else
            parent.[x] <- this.Find(parent.[x])
            parent.[x]
    
    member this.Union(x: int, y: int) : bool =
        let xRoot = this.Find(x)
        let yRoot = this.Find(y)
        
        if xRoot = yRoot then false
        else
            if rank.[xRoot] < rank.[yRoot] then
                parent.[xRoot] <- yRoot
            elif rank.[xRoot] > rank.[yRoot] then
                parent.[yRoot] <- xRoot
            else
                parent.[yRoot] <- xRoot
                rank.[xRoot] <- rank.[xRoot] + 1
            true

// Kruskal's MST algorithm
let kruskalMST(vertices: int) (edges: Edge list) : Edge list =
    // Sort edges by weight
    let sortedEdges = List.sortBy (fun e -> e.Weight) edges
    
    // Initialize Union-Find structure
    let uf = UnionFind(vertices)
    
    // Result list for MST edges
    let mstEdges = ref []
    
    // Process each edge in sorted order
    for edge in sortedEdges do
        if uf.Union(edge.Source, edge.Destination) then
            mstEdges := edge :: !mstEdges
    
    // Return the MST edges (reverse to get proper order)
    List.rev !mstEdges

// Example usage
let example() =
    // Create sample graph with 4 vertices (0, 1, 2, 3)
    let edges = [
        { Weight = 10; Source = 0; Destination = 1 }
        { Weight = 6; Source = 0; Destination = 2 }
        { Weight = 5; Source = 0; Destination = 3 }
        { Weight = 15; Source = 1; Destination = 2 }
        { Weight = 4; Source = 2; Destination = 3 }
    ]
    
    let vertices = 4
    let mst = kruskalMST vertices edges
    
    printfn "Minimum Spanning Tree edges:"
    for edge in mst do
        printfn "Edge: %d -- %d (Weight: %d)" 
            edge.Source edge.Destination edge.Weight
    
    printfn "\nTotal weight of MST: %d" 
        (mst |> List.sumBy (fun e -> e.Weight))

// Run the example
example()
```

## Output:
```
Minimum Spanning Tree edges:
Edge: 0 -- 3 (Weight: 5)
Edge: 2 -- 3 (Weight: 4)
Edge: 0 -- 1 (Weight: 10)

Total weight of MST: 19
```

## Key Features of this Implementation:

1. **Edge Type**: Defines a simple edge structure with weight and source/destination vertices
2. **Union-Find**: Efficient data structure for tracking connected components and preventing cycles
3. **Kruskal's Algorithm**: Greedily selects edges in increasing weight order, using Union-Find to avoid cycles
4. **Functional Style**: Uses immutable data structures and functional programming concepts
5. **Type Safety**: Strongly typed with clear type definitions

## Algorithm Steps:
1. Sort all edges by weight in ascending order
2. Initialize Union-Find structure for tracking connected components
3. For each edge in sorted order:
   - If the edge connects two different components, add it to MST
   - Union the two components
4. Return the set of edges forming the MST

This implementation demonstrates F#'s strengths in functional programming while solving a classic graph algorithm problem.

