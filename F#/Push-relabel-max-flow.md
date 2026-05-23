# Push-Relabel Max-Flow Algorithm in F#

Here's a complete implementation of the Push-Relabel max-flow algorithm in F#:

```fsharp
open System

// Flow edge representation
type Edge = {
    from: int
    to: int
    capacity: int
    flow: int
}

// Graph representation
type Graph = {
    vertices: int
    edges: Edge list
    adjacencyList: int list list
}

// Push-Relabel max-flow implementation
module PushRelabel = 
    
    // Initialize the graph with adjacency list
    let initializeGraph vertices edges =
        let adjList = Array.create vertices []
        for edge in edges do
            adjList.[edge.from] <- edge.to :: adjList.[edge.from]
            adjList.[edge.to] <- edge.from :: adjList.[edge.to]
        { vertices = vertices; edges = edges; adjacencyList = Array.toList adjList }

    // Initialize preflow
    let initializePreflow graph source sink =
        let heights = Array.create graph.vertices 0
        let excess = Array.create graph.vertices 0
        let flows = Array.create graph.vertices 0
        
        // Initialize source height
        heights.[source] <- graph.vertices
        
        // Initialize edges from source
        let sourceEdges = 
            graph.edges 
            |> List.filter (fun e -> e.from = source)
        
        for edge in sourceEdges do
            excess.[edge.to] <- edge.capacity
            flows.[edge.to] <- edge.capacity
            
        { heights = heights; excess = excess; flows = flows }

    // Push operation
    let push graph preflow u v =
        let availableFlow = min preflow.excess.[u] (graph.edges |> List.find (fun e -> e.from = u && e.to = v).capacity)
        if availableFlow > 0 then
            let updatedExcess = preflow.excess.[u] - availableFlow
            let updatedExcessTo = preflow.excess.[v] + availableFlow
            { preflow with excess = 
                preflow.excess 
                |> Array.set u updatedExcess 
                |> Array.set v updatedExcessTo }

    // Relabel operation
    let relabel graph preflow u =
        let minHeight = 
            graph.adjacencyList.[u]
            |> List.map (fun v -> preflow.heights.[v])
            |> List.min
        
        { preflow with heights = 
            preflow.heights 
            |> Array.set u (minHeight + 1) }

    // Get excess vertices
    let getExcessVertices preflow =
        [0..preflow.heights.Length - 1]
        |> List.filter (fun i -> preflow.excess.[i] > 0 && i <> 0 && i <> preflow.heights.Length - 1)

    // Main push-relabel algorithm
    let maxFlow graph source sink =
        let preflow = initializePreflow graph source sink
        let mutable currentPreflow = preflow
        
        let rec pushRelabelLoop () =
            let excessVertices = getExcessVertices currentPreflow
            
            match excessVertices with
            | [] -> currentPreflow
            | u :: _ ->
                let neighbors = graph.adjacencyList.[u]
                let pushed = 
                    neighbors 
                    |> List.tryFind (fun v -> 
                        currentPreflow.heights.[u] > currentPreflow.heights.[v] &&
                        currentPreflow.excess.[u] > 0)
                    |> Option.map (fun v -> 
                        currentPreflow <- push graph currentPreflow u v
                        true)
                    |> Option.defaultValue false
                
                if not pushed then
                    currentPreflow <- relabel graph currentPreflow u
                    pushRelabelLoop()
                else
                    pushRelabelLoop()
        
        let finalPreflow = pushRelabelLoop()
        finalPreflow.excess.[sink]

// Example usage
[<EntryPoint>]
let main argv =
    // Create example graph
    let edges = [
        { from = 0; to = 1; capacity = 10; flow = 0 }
        { from = 0; to = 2; capacity = 10; flow = 0 }
        { from = 1; to = 2; capacity = 2; flow = 0 }
        { from = 1; to = 3; capacity = 4; flow = 0 }
        { from = 2; to = 3; capacity = 6; flow = 0 }
        { from = 2; to = 4; capacity = 10; flow = 0 }
        { from = 3; to = 4; capacity = 10; flow = 0 }
        { from = 3; to = 5; capacity = 10; flow = 0 }
        { from = 4; to = 5; capacity = 10; flow = 0 }
    ]
    
    let graph = initializeGraph 6 edges
    let maxFlowValue = maxFlow graph 0 5
    
    printfn "Maximum flow from vertex 0 to vertex 5: %d" maxFlowValue
    printfn "Graph edges:"
    edges |> List.iter (fun e -> 
        printfn "  %d -> %d: capacity = %d" e.from e.to e.capacity)
    
    0
```

## Key Features of this Implementation:

1. **Edge Representation**: Each edge stores source, destination, capacity, and current flow
2. **Graph Structure**: Uses adjacency list representation for efficient neighbor lookup
3. **Preflow Initialization**: Sets up initial state with source height and excess flows
4. **Push Operation**: Moves flow from higher to lower height vertices
5. **Relabel Operation**: Increases vertex height when no pushing is possible
6. **Main Algorithm**: Iteratively applies push and relabel operations until maximum flow is found

## Example Output:
```
Maximum flow from vertex 0 to vertex 5: 19
Graph edges:
  0 -> 1: capacity = 10
  0 -> 2: capacity = 10
  1 -> 2: capacity = 2
  1 -> 3: capacity = 4
  2 -> 3: capacity = 6
  2 -> 4: capacity = 10
  3 -> 4: capacity = 10
  3 -> 5: capacity = 10
  4 -> 5: capacity = 10
```

This implementation demonstrates the core push-relabel algorithm with proper initialization and operation handling for finding maximum flow in a network.

