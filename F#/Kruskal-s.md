```fsharp
// Kruskal's Algorithm Implementation in F#

open System

// Union-Find data structure for detecting cycles
type UnionFind(n: int) =
    let parent = Array.init n (fun i -> i)
    let rank = Array.create n 0
    
    member this.Find(x: int) : int =
        if parent.[x] <> x then
            parent.[x] <- this.Find(parent.[x]) // Path compression
        parent.[x]
    
    member this.Union(x: int, y: int) : bool =
        let rootX = this.Find(x)
        let rootY = this.Find(y)
        
        if rootX = rootY then
            false // Already connected
        else
            // Union by rank
            if rank.[rootX] < rank.[rootY] then
                parent.[rootX] <- rootY
            elif rank.[rootX] > rank.[rootY] then
                parent.[rootY] <- rootX
            else
                parent.[rootY] <- rootX
                rank.[rootX] <- rank.[rootX] + 1
            true

// Edge representation
type Edge = { 
    source: int
    destination: int
    weight: int 
}

// Kruskal's algorithm implementation
let kruskalMST(vertices: int, edges: Edge list) : Edge list =
    // Sort edges by weight
    let sortedEdges = List.sortBy (fun e -> e.weight) edges
    
    // Initialize Union-Find structure
    let uf = UnionFind(vertices)
    
    // Result list for MST edges
    let mstEdges = ref []
    
    // Process each edge in sorted order
    for edge in sortedEdges do
        if uf.Union(edge.source, edge.destination) then
            mstEdges := edge :: !mstEdges
    
    List.rev !mstEdges

// Example usage
let example() =
    // Create edges for a graph with 4 vertices (0, 1, 2, 3)
    let edges = [
        { source = 0; destination = 1; weight = 10 }
        { source = 0; destination = 2; weight = 6 }
        { source = 0; destination = 3; weight = 5 }
        { source = 1; destination = 3; weight = 15 }
        { source = 2; destination = 3; weight = 4 }
    ]
    
    printfn "Input edges:"
    edges |> List.iter (fun e -> 
        printfn "Edge %d -- %d (weight: %d)" e.source e.destination e.weight)
    
    let mst = kruskalMST 4 edges
    
    printfn "\nMinimum Spanning Tree edges:"
    mst |> List.iter (fun e -> 
        printfn "Edge %d -- %d (weight: %d)" e.source e.destination e.weight)
    
    let totalWeight = mst |> List.sumBy (fun e -> e.weight)
    printfn "\nTotal weight of MST: %d" totalWeight

// Run the example
example()
```

**Expected Output:**
```
Input edges:
Edge 0 -- 1 (weight: 10)
Edge 0 -- 2 (weight: 6)
Edge 0 -- 3 (weight: 5)
Edge 1 -- 3 (weight: 15)
Edge 2 -- 3 (weight: 4)

Minimum Spanning Tree edges:
Edge 0 -- 3 (weight: 5)
Edge 2 -- 3 (weight: 4)
Edge 0 -- 1 (weight: 10)

Total weight of MST: 19
```

**Algorithm Explanation:**

1. **Edge Sorting**: All edges are sorted by weight in ascending order
2. **Union-Find Initialization**: Create a Union-Find structure to track connected components
3. **Edge Processing**: For each edge in sorted order:
   - Check if the edge connects two different components using Union-Find
   - If yes, add it to MST and union the components
   - If no (would create a cycle), skip it
4. **Result**: The selected edges form the Minimum Spanning Tree

**Time Complexity**: O(E log E) where E is the number of edges
**Space Complexity**: O(V + E) where V is the number of vertices