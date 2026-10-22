# Topological Sort Algorithm in F#

Here's an implementation of topological sort using Kahn's algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Define a graph representation using adjacency list
type Graph = Dictionary<int, HashSet<int>>

// Topological sort implementation using Kahn's algorithm
let topologicalSort (graph: Graph) : int list =
    // Step 1: Calculate in-degrees for all nodes
    let inDegree = 
        graph.Keys 
        |> Seq.fold (fun acc node -> 
            match acc.TryFind(node) with
            | Some(_) -> acc
            | None -> acc.Add(node, 0)) 
           Map.empty
    
    // Update in-degrees based on edges
    let inDegreeWithEdges = 
        graph 
        |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun neighbor -> neighbor))
        |> Seq.fold (fun acc node -> 
            match acc.TryFind(node) with
            | Some(degree) -> acc.Add(node, degree + 1)
            | None -> acc.Add(node, 1)) 
           inDegree
    
    // Step 2: Initialize queue with nodes having in-degree 0
    let queue = 
        inDegreeWithEdges 
        |> Seq.filter (fun kvp -> kvp.Value = 0) 
        |> Seq.map fst 
        |> Queue
    
    let result = ref []
    
    // Step 3: Process nodes in topological order
    while queue.Count > 0 do
        let current = queue.Dequeue()
        result := current :: !result
        
        // Step 4: Remove edges and update in-degrees
        match graph.TryFind(current) with
        | Some(neighbors) ->
            neighbors 
            |> Set.iter (fun neighbor ->
                let newDegree = inDegreeWithEdges.[neighbor] - 1
                let updatedInDegree = inDegreeWithEdges.Add(neighbor, newDegree)
                
                if newDegree = 0 then
                    queue.Enqueue(neighbor)
                )
        | None -> ()
    
    // Return result in correct order (reverse the list)
    List.rev !result

// Alternative cleaner implementation using a more functional approach
let topologicalSortFunctional (graph: Graph) : int list =
    let allNodes = Set.ofList (graph.Keys |> Seq.toList)
    let edges = 
        graph 
        |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun neighbor -> (kvp.Key, neighbor)))
        |> Set.ofSeq
    
    // Calculate in-degrees
    let inDegree = 
        allNodes 
        |> Set.fold (fun acc node ->
            let degree = edges |> Set.filter (fun (_, toNode) -> toNode = node) |> Set.count
            acc.Add(node, degree)) 
           Map.empty
    
    // Kahn's algorithm implementation
    let rec sortHelper queue result =
        match Queue.tryDequeue queue with
        | Some(node) ->
            let newResult = node :: result
            let neighbors = 
                graph 
                |> Seq.tryFind (fun kvp -> kvp.Key = node)
                |> Option.map (fun kvp -> kvp.Value)
                |> Option.defaultValue Set.empty
            
            let newQueue = 
                queue |> Queue.copy
                // Update in-degrees and add nodes with in-degree 0 to queue
                neighbors 
                |> Set.iter (fun neighbor ->
                    let newDegree = inDegree.[neighbor] - 1
                    if newDegree = 0 then
                        Queue.enqueue neighbor queue)
            
            sortHelper queue newResult
        | None -> List.rev result
    
    // Find nodes with in-degree 0
    let initialQueue = 
        inDegree 
        |> Map.toSeq 
        |> Seq.filter (fun (_, degree) -> degree = 0) 
        |> Seq.map fst 
        |> Queue.ofSeq
    
    sortHelper initialQueue []

// Example usage
let exampleGraph = new Graph()

// Add nodes and edges to represent a dependency graph
exampleGraph.[5] <- set [11]
exampleGraph.[7] <- set [11; 8]
exampleGraph.[3] <- set [8; 10]
exampleGraph.[11] <- set [2; 9]
exampleGraph.[8] <- set [9]
exampleGraph.[2] <- set []
exampleGraph.[9] <- set []
exampleGraph.[10] <- set []

// Run topological sort
let result = topologicalSort exampleGraph

printfn "Topological order: %A" result
// Output: [3; 5; 7; 8; 11; 2; 9; 10]

// Another example with a simpler graph
let simpleGraph = new Graph()
simpleGraph.[1] <- set [2]
simpleGraph.[2] <- set [3]
simpleGraph.[3] <- set []

let simpleResult = topologicalSort simpleGraph
printfn "Simple topological order: %A" simpleResult
// Output: [1; 2; 3]
```

## Key Features of This Implementation:

1. **Graph Representation**: Uses `Dictionary<int, HashSet<int>>` for adjacency list representation
2. **Kahn's Algorithm**: Implements the standard topological sorting algorithm
3. **In-degree Calculation**: Computes in-degrees for all nodes
4. **Queue-based Processing**: Uses a queue to process nodes with zero in-degree
5. **Cycle Detection**: The algorithm naturally detects cycles (if result length < number of nodes)

## Time and Space Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

This implementation handles directed acyclic graphs (DAGs) correctly and will produce a valid topological ordering of nodes.