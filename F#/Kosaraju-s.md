# Kosaraju's Algorithm in F#

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's an implementation in F#:

```fsharp
open System
open System.Collections.Generic

// Graph representation using adjacency list
type Graph = {
    Vertices: Set<int>
    Edges: Map<int, Set<int>>
}

// Kosaraju's algorithm implementation
let kosarajuAlgorithm (graph: Graph) : Set<Set<int>> =
    let vertices = graph.Vertices
    let edges = graph.Edges
    
    // Step 1: Get finishing times using DFS
    let rec dfsFinish (visited: Set<int>) (stack: int list) (vertex: int) : int list =
        if Set.contains vertex visited then
            stack
        else
            let newVisited = Set.add vertex visited
            let neighbors = 
                match Map.tryFind vertex edges with
                | Some n -> n
                | None -> Set.empty
            
            let newStack = 
                neighbors 
                |> Set.toList 
                |> List.fold (fun acc v -> dfsFinish newVisited acc v) stack
            
            vertex :: newStack
    
    // Get finishing order
    let finishOrder = 
        vertices 
        |> Set.toList 
        |> List.fold (fun (visited, stack) vertex -> 
            if Set.contains vertex visited then
                (visited, stack)
            else
                let newStack = dfsFinish visited stack vertex
                (Set.add vertex visited, newStack)
        ) (Set.empty, [])
        |> snd
        |> List.rev
    
    // Step 2: Create transpose graph
    let transposeGraph = 
        let transposedEdges = ref Map.empty
        edges 
        |> Map.iter (fun from toSet ->
            toSet 
            |> Set.iter (fun toVertex ->
                transposedEdges := 
                    Map.add toVertex (Set.add from (Map.tryFind toVertex !transposedEdges |> Option.defaultValue Set.empty)) !transposedEdges
            )
        )
        !transposedEdges
    
    // Step 3: DFS on transpose graph in reverse finish order
    let rec dfsComponent (visited: Set<int>) (component: Set<int>) (vertex: int) : Set<int> =
        if Set.contains vertex visited then
            component
        else
            let newVisited = Set.add vertex visited
            let neighbors = 
                match Map.tryFind vertex transposeGraph with
                | Some n -> n
                | None -> Set.empty
            
            let newComponent = Set.add vertex component
            
            neighbors 
            |> Set.toList 
            |> List.fold (fun acc v -> dfsComponent newVisited acc v) newComponent
    
    // Find all SCCs
    let rec findSCCs (visited: Set<int>) (sccs: Set<Set<int>>) (order: int list) : Set<Set<int>> =
        match order with
        | [] -> sccs
        | vertex :: rest ->
            if Set.contains vertex visited then
                findSCCs visited sccs rest
            else
                let component = dfsComponent visited Set.empty vertex
                let newVisited = Set.union visited component
                findSCCs newVisited (Set.add component sccs) rest
    
    findSCCs Set.empty Set.empty finishOrder

// Helper function to create a graph
let createGraph (vertices: int list) (edges: (int * int) list) : Graph =
    let vertexSet = Set.ofList vertices
    let edgeMap = 
        edges 
        |> List.groupBy fst
        |> Map.ofList
        |> Map.map (fun _ values -> 
            values 
            |> List.map snd 
            |> Set.ofList
        )
    
    { Vertices = vertexSet; Edges = edgeMap }

// Example usage
let example1() =
    // Create a sample graph
    let vertices = [0; 1; 2; 3; 4; 5]
    let edges = [(0, 1); (1, 2); (2, 0); (1, 3); (3, 4); (4, 5); (5, 3)]
    
    let graph = createGraph vertices edges
    
    printfn "Graph vertices: %A" vertices
    printfn "Graph edges: %A" edges
    printfn "Strongly Connected Components:"
    
    let sccs = kosarajuAlgorithm graph
    sccs 
    |> Set.iter (fun component -> 
        printfn "  %A" (Set.toList component)
    )

let example2() =
    // Another example with more complex structure
    let vertices = [0; 1; 2; 3; 4; 5; 6; 7]
    let edges = [(0, 1); (1, 2); (2, 3); (3, 0); (1, 4); (4, 5); (5, 6); (6, 4); (7, 6); (7, 8); (8, 9); (9, 7)]
    
    let graph = createGraph vertices edges
    
    printfn "\nSecond example:"
    printfn "Graph vertices: %A" vertices
    printfn "Graph edges: %A" edges
    printfn "Strongly Connected Components:"
    
    let sccs = kosarajuAlgorithm graph
    sccs 
    |> Set.iter (fun component -> 
        printfn "  %A" (Set.toList component)
    )

// Run examples
example1()
example2()
```

## How the Algorithm Works

1. **First DFS Pass**: Perform DFS on the original graph to determine the finishing times of vertices
2. **Transpose Graph**: Create a transpose of the original graph (reverse all edges)
3. **Second DFS Pass**: Perform DFS on the transposed graph in the order of decreasing finishing times from the first pass
4. **SCC Identification**: Each DFS tree in the second pass corresponds to a strongly connected component

## Key Features of This Implementation

- **Functional Style**: Uses immutable data structures and recursive functions
- **Generic**: Works with any integer vertex labels
- **Efficient**: O(V + E) time complexity
- **Clear Separation**: Well-defined steps for each phase of the algorithm
- **Error Handling**: Gracefully handles missing vertices and edges

## Sample Output
```
Graph vertices: [0; 1; 2; 3; 4; 5]
Graph edges: [(0, 1); (1, 2); (2, 0); (1, 3); (3, 4); (4, 5); (5, 3)]
Strongly Connected Components:
  [0; 1; 2]
  [3; 4; 5]
```

This implementation demonstrates how F#'s functional programming features can be used to implement classical graph algorithms effectively.

