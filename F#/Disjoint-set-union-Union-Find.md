# Disjoint Set Union (Union-Find) Algorithm in F#

Here's an implementation of the Union-Find data structure in F# with path compression and union by rank optimizations:

```fsharp
// Disjoint Set Union (Union-Find) implementation in F#
type UnionFind(n: int) =
    // Parent array where parent[i] represents the parent of element i
    let parent = Array.create n -1
    
    // Rank array to keep track of tree depth for union by rank optimization
    let rank = Array.create n 0
    
    // Find with path compression
    member this.Find(x: int) : int =
        if parent.[x] = -1 then
            x  // x is the root
        else
            // Path compression: make all nodes point directly to root
            parent.[x] <- this.Find(parent.[x])
            parent.[x]
    
    // Union operation with union by rank
    member this.Union(x: int, y: int) : bool =
        let rootX = this.Find(x)
        let rootY = this.Find(y)
        
        if rootX = rootY then
            false  // Already in the same set
        else
            // Union by rank: attach smaller tree under root of larger tree
            if rank.[rootX] < rank.[rootY] then
                parent.[rootX] <- rootY
            elif rank.[rootX] > rank.[rootY] then
                parent.[rootY] <- rootX
            else
                // If ranks are equal, choose one as root and increment its rank
                parent.[rootY] <- rootX
                rank.[rootX] <- rank.[rootX] + 1
            true
    
    // Check if two elements are in the same set
    member this.Connected(x: int, y: int) : bool =
        this.Find(x) = this.Find(y)
    
    // Get the number of elements
    member this.Size = n

// Example usage
let example() =
    // Create a Union-Find structure with 6 elements (0-5)
    let uf = UnionFind(6)
    
    printfn "Initial state - All elements are separate sets"
    
    // Perform some union operations
    uf.Union(0, 1)
    printfn "Union(0, 1) - Elements 0 and 1 are now connected"
    
    uf.Union(2, 3)
    printfn "Union(2, 3) - Elements 2 and 3 are now connected"
    
    uf.Union(4, 5)
    printfn "Union(4, 5) - Elements 4 and 5 are now connected"
    
    // Check connections
    printfn "Are 0 and 2 connected? %b" (uf.Connected(0, 2))  // Should be false
    printfn "Are 0 and 1 connected? %b" (uf.Connected(0, 1))  // Should be true
    
    uf.Union(1, 3)
    printfn "Union(1, 3) - Connect sets containing 1 and 3"
    
    printfn "Are 0 and 2 connected now? %b" (uf.Connected(0, 2))  // Should be true
    
    // Find operations to see current structure
    printfn "Find(0): %d" (uf.Find(0))
    printfn "Find(1): %d" (uf.Find(1))
    printfn "Find(2): %d" (uf.Find(2))
    printfn "Find(3): %d" (uf.Find(3))

// Run the example
example()
```

## Key Features of this Implementation:

### 1. **Path Compression**
- The `Find` operation compresses the path by making all nodes point directly to the root
- This optimization makes subsequent find operations faster

### 2. **Union by Rank**
- The `Union` operation always attaches the smaller tree under the root of the larger tree
- This keeps the tree balanced and maintains efficient operations

### 3. **Time Complexity**
- **Find**: Nearly O(1) amortized time complexity
- **Union**: Nearly O(1) amortized time complexity
- **Space**: O(n) where n is the number of elements

### 4. **Common Use Cases**
- Finding connected components in graphs
- Detecting cycles in graphs
- Kruskal's algorithm for minimum spanning tree
- Image processing (connected component labeling)

## Sample Output:
```
Initial state - All elements are separate sets
Union(0, 1) - Elements 0 and 1 are now connected
Union(2, 3) - Elements 2 and 3 are now connected
Union(4, 5) - Elements 4 and 5 are now connected
Are 0 and 2 connected? false
Are 0 and 1 connected? true
Union(1, 3) - Connect sets containing 1 and 3
Are 0 and 2 connected now? true
Find(0): 1
Find(1): 1
Find(2): 1
Find(3): 1
```

This implementation provides an efficient way to manage disjoint sets with near-constant time operations for both union and find operations.