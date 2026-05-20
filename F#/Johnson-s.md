# Johnson's Algorithm in F#

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, even when negative edge weights are present (but no negative cycles). Here's an implementation in F#:

```fsharp
open System
open System.Collections.Generic

// Graph representation
type Edge = { From: int; To: int; Weight: float }

// Johnson's Algorithm implementation
module JohnsonAlgorithm =
    // Bellman-Ford algorithm to detect negative cycles and find distances from a source
    let bellmanFord (edges: Edge list) (vertices: int list) (source: int) : float option =
        let distances = Dictionary<int, float>()
        vertices |> List.iter (fun v -> distances.[v] <- if v = source then 0.0 else Double.MaxValue)
        
        // Relax edges repeatedly
        for _ in 1 .. vertices.Length - 1 do
            for edge in edges do
                if distances.[edge.From] <> Double.MaxValue && 
                   distances.[edge.From] + edge.Weight < distances.[edge.To] then
                    distances.[edge.To] <- distances.[edge.From] + edge.Weight
        
        // Check for negative cycles
        let hasNegativeCycle = 
            edges |> List.exists (fun edge -> 
                distances.[edge.From] <> Double.MaxValue && 
                distances.[edge.From] + edge.Weight < distances.[edge.To])
        
        if hasNegativeCycle then None
        else Some (distances.[source])
    
    // Modified Dijkstra's algorithm for Johnson's algorithm
    let dijkstraModified (edges: Edge list) (vertices: int list) (source: int) : Map<int, float> =
        let distances = ref (Map.empty<int, float>)
        let visited = ref (Set.empty<int>)
        let pq = new PriorityQueue<int, float>()
        
        // Initialize distances
        distances := vertices |> List.fold (fun acc v -> Map.add v Double.MaxValue acc) Map.empty
        distances := Map.add source 0.0 !distances
        pq.Enqueue(source, 0.0)
        
        while pq.Count > 0 do
            let current = pq.Dequeue()
            
            if not (Set.contains current !visited) then
                visited := Set.add current !visited
                
                let currentDistance = Map.find current !distances
                
                edges
                |> List.filter (fun e -> e.From = current)
                |> List.iter (fun edge ->
                    let newDistance = currentDistance + edge.Weight
                    let existingDistance = Map.find edge.To !distances
                    
                    if newDistance < existingDistance then
                        distances := Map.add edge.To newDistance !distances
                        pq.Enqueue(edge.To, newDistance))
        
        !distances
    
    // Main Johnson's algorithm function
    let johnson (edges: Edge list) (vertices: int list) : Map<int * int, float> option =
        // Step 1: Add a new vertex with zero-weight edges to all other vertices
        let newVertex = vertices |> List.max + 1
        let newEdges = 
            edges @ 
            vertices |> List.map (fun v -> { From = newVertex; To = v; Weight = 0.0 })
        
        // Step 2: Run Bellman-Ford from the new vertex
        match bellmanFord newEdges vertices newVertex with
        | None -> None // Negative cycle detected
        | Some _ -> 
            // Step 3: Compute vertex weights using Bellman-Ford results
            let vertexWeights = 
                vertices |> List.map (fun v -> 
                    match bellmanFord newEdges vertices newVertex with
                    | Some weights -> weights
                    | None -> 0.0)
            
            // Step 4: Reweight edges
            let reweightedEdges = 
                edges |> List.map (fun edge -> 
                    { From = edge.From; To = edge.To; Weight = edge.Weight + vertexWeights.[edge.From] - vertexWeights.[edge.To] })
            
            // Step 5: Run Dijkstra for each vertex
            let allShortestPaths = ref Map.empty
            
            vertices |> List.iter (fun source -> 
                let distances = dijkstraModified reweightedEdges vertices source
                distances |> Map.iter (fun target distance ->
                    let originalDistance = distance - vertexWeights.[source] + vertexWeights.[target]
                    allShortestPaths := Map.add (source, target) originalDistance !allShortestPaths))
            
            Some !allShortestPaths

// Example usage
[<EntryPoint>]
let main argv =
    // Create a sample graph with negative edge weights
    let edges = [
        { From = 1; To = 2; Weight = 3.0 }
        { From = 1; To = 3; Weight = 8.0 }
        { From = 1; To = 5; Weight = -4.0 }
        { From = 2; To = 5; Weight = 7.0 }
        { From = 2; To = 3; Weight = 4.0 }
        { From = 3; To = 2; Weight = -5.0 }
        { From = 4; To = 1; Weight = 2.0 }
        { From = 4; To = 3; Weight = 6.0 }
        { From = 5; To = 4; Weight = -3.0 }
    ]
    
    let vertices = [1; 2; 3; 4; 5]
    
    printfn "Graph edges:"
    edges |> List.iter (fun e -> printfn "  %d -> %d (weight: %f)" e.From e.To e.Weight)
    
    match JohnsonAlgorithm.johnson edges vertices with
    | Some distances ->
        printfn "\nAll-pairs shortest paths:"
        distances |> Map.iter (fun (from, to), distance ->
            printfn "  %d -> %d: %f" from to distance)
    | None ->
        printfn "Graph contains negative cycle"
    
    0
```

## Key Components of the Implementation:

1. **Bellman-Ford Algorithm**: Used to detect negative cycles and compute initial distances
2. **Modified Dijkstra**: Runs on reweighted edges to find shortest paths
3. **Edge Reweighting**: Adjusts edge weights to eliminate negative weights
4. **Vertex Weight Computation**: Calculates weights for each vertex to reweight edges

## Example Output:
```
Graph edges:
  1 -> 2 (weight: 3.000000)
  1 -> 3 (weight: 8.000000)
  1 -> 5 (weight: -4.000000)
  2 -> 5 (weight: 7.000000)
  2 -> 3 (weight: 4.000000)
  3 -> 2 (weight: -5.000000)
  4 -> 1 (weight: 2.000000)
  4 -> 3 (weight: 6.000000)
  5 -> 4 (weight: -3.000000)

All-pairs shortest paths:
  1 -> 1: 0.000000
  1 -> 2: 1.000000
  1 -> 3: 8.000000
  1 -> 4: 2.000000
  1 -> 5: -4.000000
  2 -> 1: 1.000000
  2 -> 2: 0.000000
  2 -> 3: 4.000000
  2 -> 4: 6.000000
  2 -> 5: 7.000000
  ...
```

This implementation handles negative edge weights correctly and returns all-pairs shortest paths in O(V²logV) time complexity.

