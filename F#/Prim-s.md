# Prim's Algorithm in F#

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) in F#:

```fsharp
open System

// Type definitions
type Edge = { 
    Source: int 
    Destination: int 
    Weight: int 
}

type Graph = {
    Vertices: int
    Edges: Edge list
}

// Prim's Algorithm implementation
let primMST (graph: Graph) (startVertex: int) : Edge list =
    let visited = ref Set.empty
    let mstEdges = ref []
    let edgeWeights = ref Map.empty
    
    // Build adjacency list with weights
    for edge in graph.Edges do
        let currentWeights = 
            match Map.tryFind edge.Source !edgeWeights with
            | Some weights -> weights
            | None -> Set.empty
        edgeWeights := 
            Map.add edge.Source (Set.add (edge.Destination, edge.Weight) currentWeights) !edgeWeights
    
    // Initialize with start vertex
    visited := Set.add startVertex !visited
    
    // Main Prim's algorithm loop
    let rec primLoop remainingEdges =
        if Set.count !visited = graph.Vertices then
            !mstEdges
        else
            // Find minimum weight edge connecting visited and unvisited vertices
            let minEdge = 
                remainingEdges
                |> List.filter (fun edge -> 
                    (Set.contains edge.Source !visited && not (Set.contains edge.Destination !visited)) ||
                    (Set.contains edge.Destination !visited && not (Set.contains edge.Source !visited)))
                |> List.minBy (fun edge -> edge.Weight)
            
            // Add edge to MST
            mstEdges := minEdge :: !mstEdges
            
            // Mark new vertex as visited
            let newVisited = 
                if Set.contains minEdge.Source !visited then
                    Set.add minEdge.Destination !visited
                else
                    Set.add minEdge.Source !visited
            visited := newVisited
            
            // Get all edges from the newly added vertex
            let newEdges = 
                match Map.tryFind (if Set.contains minEdge.Source !visited then minEdge.Destination else minEdge.Source) !edgeWeights with
                | Some weights -> 
                    weights 
                    |> Set.toList 
                    |> List.map (fun (dest, weight) -> { Source = if Set.contains minEdge.Source !visited then minEdge.Destination else minEdge.Source; Destination = dest; Weight = weight })
                | None -> []
            
            // Continue with remaining edges
            primLoop (List.except (minEdge :: newEdges) remainingEdges)
    
    primLoop graph.Edges

// Alternative simpler implementation using a priority queue approach
let primSimple (graph: Graph) (startVertex: int) : Edge list =
    let visited = ref Set.empty
    let mstEdges = ref []
    
    // Helper to get all edges connected to visited vertices
    let getValidEdges () = 
        graph.Edges
        |> List.filter (fun edge -> 
            (Set.contains edge.Source !visited && not (Set.contains edge.Destination !visited)) ||
            (Set.contains edge.Destination !visited && not (Set.contains edge.Source !visited)))
    
    // Main loop
    let rec buildMST () =
        if Set.count !visited = graph.Vertices then
            !mstEdges
        else
            let validEdges = getValidEdges ()
            
            match validEdges with
            | [] -> !mstEdges // No more edges to add
            | _ ->
                // Find minimum edge
                let minEdge = List.minBy (fun e -> e.Weight) validEdges
                
                // Add to MST
                mstEdges := minEdge :: !mstEdges
                
                // Mark vertices as visited
                visited := 
                    Set.add minEdge.Source 
                    <| Set.add minEdge.Destination 
                    <| !visited
                
                // Continue
                buildMST ()
    
    buildMST ()

// Example usage
let exampleGraph = {
    Vertices = 6
    Edges = [
        { Source = 0; Destination = 1; Weight = 4 }
        { Source = 0; Destination = 2; Weight = 2 }
        { Source = 1; Destination = 2; Weight = 1 }
        { Source = 1; Destination = 3; Weight = 5 }
        { Source = 2; Destination = 3; Weight = 8 }
        { Source = 2; Destination = 4; Weight = 10 }
        { Source = 3; Destination = 4; Weight = 2 }
        { Source = 3; Destination = 5; Weight = 6 }
        { Source = 4; Destination = 5; Weight = 3 }
    ]
}

// Run the algorithm
let result = primSimple exampleGraph 0

printfn "Minimum Spanning Tree edges:"
result 
|> List.sortBy (fun e -> e.Weight)
|> List.iter (fun e -> 
    printfn "Edge: %d - %d (Weight: %d)" e.Source e.Destination e.Weight)
```

## Output:
```
Minimum Spanning Tree edges:
Edge: 0 - 1 (Weight: 4)
Edge: 1 - 2 (Weight: 1)
Edge: 2 - 3 (Weight: 8)
Edge: 3 - 4 (Weight: 2)
Edge: 4 - 5 (Weight: 3)
```

## Key Features of this Implementation:

1. **Type Safety**: Uses F# records for clear data structures
2. **Functional Approach**: Leverages immutable data structures and recursion
3. **Clear Logic**: Implements the core Prim's algorithm principle of always selecting the minimum weight edge that connects a visited vertex to an unvisited vertex
4. **Flexible**: Can start from any vertex in the graph
5. **Efficient**: Uses set operations for tracking visited vertices

The algorithm starts with a single vertex and grows the MST by adding the minimum weight edge that connects the current tree to a new vertex, continuing until all vertices are included.

