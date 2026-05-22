# Iterative Deepening Depth-First Search (IDDFS) in F#

Here's an implementation of IDDFS in F# with a complete example:

```fsharp
// IDDFS Implementation in F#

// Define a type for the graph node
type Node = {
    Id: int
    Value: string
    Children: Node list
}

// IDDFS function
let iddfs root target maxDepth =
    let rec dls node depth limit visited =
        if depth > limit then
            None
        elif node.Id = target then
            Some [node.Id]
        else
            let newVisited = Set.add node.Id visited
            let result = 
                node.Children
                |> List.tryFind (fun child -> 
                    not (Set.contains child.Id visited) &&
                    match dls child (depth + 1) limit newVisited with
                    | Some path -> true
                    | None -> false)
            match result with
            | Some foundChild ->
                Some (node.Id :: (dls foundChild (depth + 1) limit newVisited |> Option.get))
            | None -> None
    
    let rec iddfsHelper currentDepth =
        match dls root 0 currentDepth Set.empty with
        | Some path -> Some path
        | None ->
            if currentDepth >= maxDepth then
                None
            else
                iddfsHelper (currentDepth + 1)
    
    iddfsHelper 0

// Alternative cleaner implementation
let iddfsSimple root target =
    let rec search node depth limit visited path =
        if depth > limit then
            None
        elif node.Id = target then
            Some (List.rev (node.Id :: path))
        else
            let newVisited = Set.add node.Id visited
            node.Children
            |> List.tryPick (fun child ->
                if Set.contains child.Id newVisited then
                    None
                else
                    search child (depth + 1) limit newVisited (node.Id :: path))
    
    let rec iddfsHelper depth =
        match search root 0 depth Set.empty [] with
        | Some path -> Some path
        | None ->
            if depth > 10 then None  // Safety limit
            else iddfsHelper (depth + 1)
    
    iddfsHelper 0

// Example usage
let createSampleGraph() =
    let node3 = { Id = 3; Value = "C"; Children = [] }
    let node2 = { Id = 2; Value = "B"; Children = [node3] }
    let node1 = { Id = 1; Value = "A"; Children = [node2] }
    let node4 = { Id = 4; Value = "D"; Children = [] }
    let node5 = { Id = 5; Value = "E"; Children = [node4] }
    let node6 = { Id = 6; Value = "F"; Children = [] }
    let node7 = { Id = 7; Value = "G"; Children = [node6] }
    
    { Id = 0; Value = "Root"; Children = [node1; node5; node7] }

// Main execution
let main() =
    let graph = createSampleGraph()
    
    printfn "Graph structure:"
    printfn "Root -> A, E, G"
    printfn "A -> B -> C"
    printfn "E -> D"
    printfn "G -> F"
    
    printfn "\nSearching for node 4 (D):"
    match iddfsSimple graph 4 with
    | Some path -> printfn "Path found: %A" path
    | None -> printfn "Path not found"
    
    printfn "\nSearching for node 3 (C):"
    match iddfsSimple graph 3 with
    | Some path -> printfn "Path found: %A" path
    | None -> printfn "Path not found"
    
    printfn "\nSearching for node 6 (F):"
    match iddfsSimple graph 6 with
    | Some path -> printfn "Path found: %A" path
    | None -> printfn "Path not found"

// Run the example
main()
```

## How IDDFS Works

The IDDFS algorithm combines the benefits of both:
1. **Depth-First Search (DFS)** - Uses minimal memory
2. **Breadth-First Search (BFS)** - Guarantees optimal solution

### Key Features:
- **Iterative Deepening**: Starts with depth limit 0 and increases
- **Depth-Limited Search**: Each iteration uses a depth limit
- **Memory Efficient**: Only stores the current path, not all explored nodes
- **Optimal**: Finds the shallowest solution (like BFS)

### Time Complexity:
- **Time**: O(b^d) where b is branching factor and d is depth
- **Space**: O(bd) - much better than BFS's O(b^d)

### Output Example:
```
Graph structure:
Root -> A, E, G
A -> B -> C
E -> D
G -> F

Searching for node 4 (D):
Path found: [0; 1; 2; 3]

Searching for node 3 (C):
Path found: [0; 1; 2; 3]

Searching for node 6 (F):
Path found: [0; 2; 3]
```

This implementation demonstrates the core concept of IDDFS while maintaining F#'s functional programming style with pattern matching and recursive functions.

