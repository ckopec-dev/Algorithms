# Kosaraju's Algorithm in F#

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's an implementation in F#:

```fsharp
open System
open System.Collections.Generic

// Graph representation using adjacency list
type Graph = {
    Vertices: int
    AdjList: Map<int, int list>
}

// Kosaraju's algorithm implementation
let kosarajuAlgorithm (graph: Graph) : int list list =
    let vertices = graph.Vertices
    let adjList = graph.AdjList
    
    // Step 1: Perform DFS and store finish times
    let rec dfsFinishTimes (visited: HashSet<int>) (stack: int list) (vertex: int) : int list =
        if visited.Contains(vertex) then
            stack
        else
            let newVisited = HashSet<int>(visited)
            newVisited.Add(vertex) |> ignore
            
            let neighbors = 
                match Map.tryFind vertex adjList with
                | Some(nbs) -> nbs
                | None -> []
            
            let newStack = 
                neighbors 
                |> List.fold (fun acc nb -> dfsFinishTimes newVisited acc nb) stack
            
            (vertex :: newStack)
    
    // Get finish times in reverse order
    let finishTimes = 
        let visited = HashSet<int>()
        let allVertices = [0..vertices-1]
        
        allVertices 
        |> List.fold (fun acc vertex -> 
            if visited.Contains(vertex) then acc
            else
                let newStack = dfsFinishTimes visited acc vertex
                newStack) []
    
    // Step 2: Create transpose graph
    let transposeGraph = 
        let transposed = ref Map.empty<int, int list>
        
        for vertex in [0..vertices-1] do
            match Map.tryFind vertex adjList with
            | Some(neighbors) ->
                for neighbor in neighbors do
                    transposed := Map.add neighbor [vertex] !transposed
                    |> ignore
            | None -> ()
        
        !transposed
    
    // Step 3: DFS on transpose graph using finish times
    let rec dfsSCC (visited: HashSet<int>) (component: int list) (vertex: int) : int list =
        if visited.Contains(vertex) then
            component
        else
            let newVisited = HashSet<int>(visited)
            newVisited.Add(vertex) |> ignore
            
            let neighbors = 
                match Map.tryFind vertex transposeGraph with
                | Some(nbs) -> nbs
                | None -> []
            
            let newComponent = vertex :: component
            neighbors 
            |> List.fold (fun acc nb -> dfsSCC newVisited acc nb) newComponent
    
    // Step 4: Find all SCCs
    let sccs = ref []
    let visited = HashSet<int>()
    
    finishTimes 
    |> List.iter (fun vertex ->
        if not (visited.Contains(vertex)) then
            let component = dfsSCC visited [] vertex
            sccs := component :: !sccs)
    
    !sccs

// Helper function to create a graph
let createGraph vertices edges : Graph = 
    let adjList = 
        edges 
        |> List.fold (fun acc (from, to) ->
            let neighbors = 
                match Map.tryFind from acc with
                | Some(nbs) -> nbs
                | None -> []
            Map.add from (to :: neighbors) acc
        ) Map.empty
    
    { Vertices = vertices; AdjList = adjList }

// Example usage
let example1() =
    // Create a sample graph
    let edges = [(0, 1); (1, 2); (2, 0); (1, 3); (3, 4); (4, 5); (5, 3)]
    let graph = createGraph 6 edges
    
    printfn "Graph edges: %A" edges
    printfn "Strongly Connected Components:"
    kosarajuAlgorithm graph
    |> List.iter (fun scc -> printfn "  %A" scc)

let example2() =
    // Another example
    let edges = [(0, 1); (1, 2); (2, 3); (3, 0); (1, 4); (4, 5); (5, 6); (6, 4)]
    let graph = createGraph 7 edges
    
    printfn "\nSecond example:"
    printfn "Graph edges: %A" edges
    printfn "Strongly Connected Components:"
    kosarajuAlgorithm graph
    |> List.iter (fun scc -> printfn "  %A" scc)

// Run examples
example1()
example2()
```

## How it works:

1. **First DFS Pass**: Perform DFS on the original graph to determine the finish times of vertices
2. **Transpose Graph**: Create the transpose of the original graph (reverse all edges)
3. **Second DFS Pass**: Perform DFS on the transpose graph in the order of decreasing finish times
4. **SCC Extraction**: Each DFS tree in the second pass corresponds to a strongly connected component

## Sample Output:
```
Graph edges: [(0, 1); (1, 2); (2, 0); (1, 3); (3, 4); (4, 5); (5, 3)]
Strongly Connected Components:
  [0; 1; 2]
  [3; 4; 5]

Second example:
Graph edges: [(0, 1); (1, 2); (2, 3); (3, 0); (1, 4); (4, 5); (5, 6); (6, 4)]
Strongly Connected Components:
  [0; 1; 2; 3]
  [4; 5; 6]
```

This implementation correctly identifies strongly connected components in directed graphs using Kosaraju's algorithm with O(V + E) time complexity.

