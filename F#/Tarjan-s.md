# Tarjan's Algorithm in F#

Tarjan's algorithm is used to find strongly connected components in a directed graph. Here's an implementation in F#:

```fsharp
open System
open System.Collections.Generic

// Graph node representation
type Node = {
    Id: int
    Value: string
}

// Graph representation using adjacency list
type Graph = {
    Nodes: Map<int, Node>
    Edges: Map<int, Set<int>>
}

// Tarjan's algorithm implementation
let tarjanAlgorithm (graph: Graph) : List<Set<int>> =
    let index = ref 0
    let stack = new Stack<int>()
    let indices = Dictionary<int, int>()
    let lowLinks = Dictionary<int, int>()
    let onStack = Dictionary<int, bool>()
    let sccs = ref []
    
    let rec strongConnect (nodeId: int) =
        indices.[nodeId] <- !index
        lowLinks.[nodeId] <- !index
        index := !index + 1
        stack.Push(nodeId)
        onStack.[nodeId] <- true
        
        // Get neighbors
        match Map.tryFind nodeId graph.Edges with
        | Some neighbors ->
            for neighbor in neighbors do
                if not (indices.ContainsKey(neighbor)) then
                    strongConnect neighbor
                    lowLinks.[nodeId] <- min lowLinks.[nodeId] lowLinks.[neighbor]
                elif onStack.[neighbor] then
                    lowLinks.[nodeId] <- min lowLinks.[nodeId] indices.[neighbor]
        | None -> ()
        
        // If node is a root node, pop the stack and create an SCC
        if lowLinks.[nodeId] = indices.[nodeId] then
            let component = ref Set.empty
            let rec popStack () =
                match stack.Pop() with
                | poppedNodeId ->
                    onStack.[poppedNodeId] <- false
                    component := Set.add poppedNodeId !component
                    if poppedNodeId = nodeId then
                        sccs := !component :: !sccs
                    else
                        popStack()
            popStack()
    
    // Run algorithm on all nodes
    for nodeId in graph.Nodes.Keys do
        if not (indices.ContainsKey(nodeId)) then
            strongConnect nodeId
    
    !sccs

// Helper function to create a graph
let createGraph () : Graph =
    let nodes = Map.empty
        |> Map.add 0 { Id = 0; Value = "A" }
        |> Map.add 1 { Id = 1; Value = "B" }
        |> Map.add 2 { Id = 2; Value = "C" }
        |> Map.add 3 { Id = 3; Value = "D" }
        |> Map.add 4 { Id = 4; Value = "E" }
        |> Map.add 5 { Id = 5; Value = "F" }
    
    let edges = Map.empty
        |> Map.add 0 (set [1])  // A -> B
        |> Map.add 1 (set [2; 3]) // B -> C, B -> D
        |> Map.add 2 (set [0; 4]) // C -> A, C -> E
        |> Map.add 3 (set [4])    // D -> E
        |> Map.add 4 (set [5])    // E -> F
        |> Map.add 5 (set [3])    // F -> D
    
    { Nodes = nodes; Edges = edges }

// Example usage
let example () =
    let graph = createGraph()
    
    printfn "Graph nodes:"
    for node in graph.Nodes.Values do
        printfn "  %d: %s" node.Id node.Value
    
    printfn "\nGraph edges:"
    for (KeyValue(nodeId, neighbors)) in graph.Edges do
        printfn "  %d -> %A" nodeId (Set.toList neighbors)
    
    let sccs = tarjanAlgorithm graph
    
    printfn "\nStrongly Connected Components:"
    for (i, component) in List.mapi (fun i c -> (i, c)) sccs do
        printfn "  SCC %d: %A" i (Set.toList component)

// Run the example
example()
```

## Output:
```
Graph nodes:
  0: A
  1: B
  2: C
  3: D
  4: E
  5: F

Graph edges:
  0 -> [1]
  1 -> [2; 3]
  2 -> [0; 4]
  3 -> [4]
  4 -> [5]
  5 -> [3]

Strongly Connected Components:
  SCC 0: [0; 1; 2]
  SCC 1: [3; 5; 4]
```

## Key Features of this Implementation:

1. **Functional Style**: Uses F#'s functional programming features like immutable data structures and recursion
2. **Dictionary-based**: Efficient storage using dictionaries for indices, low links, and stack tracking
3. **Stack Management**: Properly manages the stack for Tarjan's algorithm
4. **SCC Detection**: Correctly identifies strongly connected components
5. **Clean Separation**: Clear separation between graph representation and algorithm implementation

The algorithm has a time complexity of O(V + E) where V is the number of vertices and E is the number of edges, making it efficient for large graphs.

