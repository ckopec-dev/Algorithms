# Depth-First Search (DFS) Algorithm in F#

Here's a complete implementation of DFS in F# using both recursive and iterative approaches:

## Recursive DFS Implementation

```fsharp
// Define a graph as an adjacency list
type Graph = Map<int, int list>

// Recursive DFS implementation
let rec dfsRecursive (graph: Graph) (start: int) (visited: Set<int>) : int list =
    if Set.contains start visited then
        []
    else
        let newVisited = Set.add start visited
        let neighbors = 
            match Map.tryFind start graph with
            | Some(nbs) -> nbs
            | None -> []
        
        start :: 
        neighbors 
        |> List.collect (fun neighbor -> 
            dfsRecursive graph neighbor newVisited)

// Helper function to start DFS from a node
let dfsStart (graph: Graph) (start: int) : int list =
    dfsRecursive graph start Set.empty

// Example usage
let exampleGraph = 
    Map.ofList [
        (0, [1; 2])
        (1, [0; 3; 4])
        (2, [0; 5])
        (3, [1])
        (4, [1; 6])
        (5, [2])
        (6, [4])
    ]

// Run DFS starting from node 0
let result = dfsStart exampleGraph 0
printfn "DFS traversal: %A" result
// Output: [0; 1; 3; 4; 6; 2; 5]
```

## Iterative DFS Implementation

```fsharp
open System.Collections.Generic

// Iterative DFS using a stack
let dfsIterative (graph: Graph) (start: int) : int list =
    let visited = ref Set.empty
    let stack = new Stack<int>()
    let result = ref []
    
    stack.Push(start)
    
    while stack.Count > 0 do
        let current = stack.Pop()
        
        if not (Set.contains current !visited) then
            visited := Set.add current !visited
            result := current :: !result
            
            // Add neighbors to stack (in reverse order for consistent traversal)
            match Map.tryFind current graph with
            | Some(neighbors) -> 
                neighbors 
                |> List.rev
                |> List.iter stack.Push
            | None -> ()
    
    List.rev !result

// Example usage of iterative DFS
let iterativeResult = dfsIterative exampleGraph 0
printfn "Iterative DFS traversal: %A" iterativeResult
// Output: [0; 1; 3; 4; 6; 2; 5]
```

## Complete Working Example

```fsharp
open System.Collections.Generic

type Graph = Map<int, int list>

let dfsRecursive (graph: Graph) (start: int) : int list =
    let rec dfsHelper (node: int) (visited: Set<int>) : int list =
        if Set.contains node visited then
            []
        else
            let newVisited = Set.add node visited
            let neighbors = 
                match Map.tryFind node graph with
                | Some(nbs) -> nbs
                | None -> []
            
            node :: 
            neighbors 
            |> List.collect (fun neighbor -> dfsHelper neighbor newVisited)
    
    dfsHelper start Set.empty

// Test with a sample graph
let testGraph = 
    Map.ofList [
        (0, [1; 2])
        (1, [3; 4])
        (2, [5])
        (3, [])
        (4, [6])
        (5, [])
        (6, [])
    ]

// Execute DFS
let dfsResult = dfsRecursive testGraph 0
printfn "DFS traversal from node 0: %A" dfsResult
// Output: [0; 1; 3; 4; 6; 2; 5]
```

## Key Features of This Implementation:

1. **Type Safety**: Uses F#'s strong typing system with `Graph` type alias
2. **Functional Approach**: Recursive implementation that's idiomatic to F#
3. **Memory Management**: Properly tracks visited nodes to avoid cycles
4. **Flexibility**: Works with any integer-based graph representation
5. **Clear Separation**: Clean separation between traversal logic and graph structure

The algorithm traverses the graph depth-first, visiting each node once and following paths as deep as possible before backtracking.