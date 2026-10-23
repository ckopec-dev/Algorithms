```fsharp
// Union-Find (Disjoint Set) implementation in F#

type UnionFind =
    { 
        parent: int array
        rank: int array
        count: int
    }

// Create a new UnionFind structure with n elements
let createUnionFind n =
    let parent = Array.init n (fun i -> i)
    let rank = Array.create n 0
    { parent = parent; rank = rank; count = n }

// Find the root of element x with path compression
let rec find uf x =
    if uf.parent.[x] <> x then
        uf.parent.[x] <- find uf uf.parent.[x]  // Path compression
    uf.parent.[x]

// Union two elements x and y
let union uf x y =
    let rootX = find uf x
    let rootY = find uf y
    
    if rootX <> rootY then
        // Union by rank
        if uf.rank.[rootX] < uf.rank.[rootY] then
            uf.parent.[rootX] <- rootY
        elif uf.rank.[rootX] > uf.rank.[rootY] then
            uf.parent.[rootY] <- rootX
        else
            uf.parent.[rootY] <- rootX
            uf.rank.[rootX] <- uf.rank.[rootX] + 1
        uf.count <- uf.count - 1

// Check if two elements are connected
let connected uf x y =
    find uf x = find uf y

// Get the number of connected components
let componentCount uf =
    uf.count

// Example usage
let example () =
    // Create UnionFind with 6 elements (0-5)
    let uf = createUnionFind 6
    
    printfn "Initial components: %d" (componentCount uf)
    
    // Perform some unions
    union uf 0 1
    printfn "After uniting 0 and 1: %d components" (componentCount uf)
    
    union uf 2 3
    printfn "After uniting 2 and 3: %d components" (componentCount uf)
    
    union uf 4 5
    printfn "After uniting 4 and 5: %d components" (componentCount uf)
    
    // Connect two previously disconnected components
    union uf 1 3
    printfn "After uniting 1 and 3: %d components" (componentCount uf)
    
    // Check connections
    printfn "Are 0 and 2 connected? %b" (connected uf 0 2)
    printfn "Are 0 and 4 connected? %b" (connected uf 0 4)
    
    // Show final structure
    printfn "Final parent array: %A" uf.parent
    printfn "Final rank array: %A" uf.rank

// Run the example
example ()
```

This implementation demonstrates:

1. **Data Structure**: A UnionFind type with parent array, rank array, and component count
2. **Key Operations**:
   - `createUnionFind`: Initialize with n elements
   - `find`: Find root with path compression optimization
   - `union`: Merge two components using union by rank
   - `connected`: Check if two elements are in the same component
   - `componentCount`: Get number of connected components

3. **Optimizations**:
   - Path compression in find operation
   - Union by rank for efficient merging
   - Immutable-like behavior with functional approach

The example shows creating 6 elements, performing several unions, and checking connectivity between different pairs of elements.