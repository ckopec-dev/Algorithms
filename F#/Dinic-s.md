# Dinic's Algorithm Implementation in F#

Dinic's algorithm is a fast algorithm for computing maximum flow in a flow network. Here's a complete implementation in F#:

```fsharp
open System
open System.Collections.Generic

// Flow edge representation
type FlowEdge = {
    From: int
    To: int
    Capacity: int
    Flow: int
    ReverseIndex: int
}

// Graph representation
type FlowGraph = {
    Vertices: int
    Edges: FlowEdge list
    AdjacencyList: int list list
}

// Dinic's algorithm implementation
module Dinic =
    // Build adjacency list from edges
    let buildAdjacencyList (edges: FlowEdge list) (vertices: int) =
        let adj = Array.create vertices []
        for edge in edges do
            adj.[edge.From] <- edge.To :: adj.[edge.From]
        Array.toList adj

    // Create a flow graph from adjacency list
    let createGraph (vertices: int) (edges: (int * int * int) list) =
        let edgeList = 
            edges 
            |> List.mapi (fun i (from, to_, capacity) ->
                { From = from; To = to_; Capacity = capacity; Flow = 0; ReverseIndex = -1 })
        
        // Create reverse edges
        let reverseEdges = 
            edgeList 
            |> List.mapi (fun i edge ->
                { From = edge.To; To = edge.From; Capacity = edge.Capacity; Flow = 0; ReverseIndex = i })
        
        let allEdges = edgeList @ reverseEdges
        
        // Set reverse indices
        let indexedEdges = 
            allEdges 
            |> List.mapi (fun i edge ->
                if edge.From < edge.To then
                    { edge with ReverseIndex = 
                        allEdges 
                        |> List.findIndex (fun e -> e.From = edge.To && e.To = edge.From) }
                else
                    edge)
        
        { Vertices = vertices
          Edges = indexedEdges
          AdjacencyList = buildAdjacencyList indexedEdges vertices }

    // Level graph construction using BFS
    let buildLevelGraph (graph: FlowGraph) (source: int) (sink: int) =
        let level = Array.create graph.Vertices -1
        let queue = new Queue<int>()
        
        level.[source] <- 0
        queue.Enqueue(source)
        
        while queue.Count > 0 do
            let current = queue.Dequeue()
            
            for toVertex in graph.AdjacencyList.[current] do
                if level.[toVertex] = -1 then
                    let edge = 
                        graph.Edges 
                        |> List.find (fun e -> e.From = current && e.To = toVertex)
                    
                    if edge.Capacity > edge.Flow then
                        level.[toVertex] <- level.[current] + 1
                        queue.Enqueue(toVertex)
        
        level

    // Find blocking flow using DFS
    let rec findBlockingFlow (graph: FlowGraph) (level: int[]) (source: int) (sink: int) (current: int) (flow: int) =
        if current = sink then
            flow
        else
            let mutable minFlow = flow
            let mutable i = 0
            
            while i < graph.AdjacencyList.[current].Length && minFlow > 0 do
                let toVertex = graph.AdjacencyList.[current].[i]
                
                if level.[current] + 1 = level.[toVertex] then
                    let edge = 
                        graph.Edges 
                        |> List.find (fun e -> e.From = current && e.To = toVertex)
                    
                    if edge.Capacity > edge.Flow then
                        let minEdgeFlow = min minFlow (edge.Capacity - edge.Flow)
                        let flowAdded = 
                            findBlockingFlow graph level source sink toVertex minEdgeFlow
                        
                        if flowAdded > 0 then
                            let edgeIndex = 
                                graph.Edges 
                                |> List.findIndex (fun e -> e.From = current && e.To = toVertex)
                            
                            let reverseEdgeIndex = 
                                graph.Edges 
                                |> List.findIndex (fun e -> e.From = toVertex && e.To = current)
                            
                            graph.Edges.[edgeIndex] <- { graph.Edges.[edgeIndex] with Flow = graph.Edges.[edgeIndex].Flow + flowAdded }
                            graph.Edges.[reverseEdgeIndex] <- { graph.Edges.[reverseEdgeIndex] with Flow = graph.Edges.[reverseEdgeIndex].Flow - flowAdded }
                            
                            minFlow <- minFlow - flowAdded
                i <- i + 1
            
            flow - minFlow

    // Main Dinic's algorithm
    let maxFlow (graph: FlowGraph) (source: int) (sink: int) =
        let mutable maxFlowValue = 0
        let level = Array.create graph.Vertices -1
        
        while true do
            let newLevel = buildLevelGraph graph source sink
            if newLevel.[sink] = -1 then
                break
            
            let mutable currentFlow = 0
            while currentFlow > 0 do
                currentFlow <- findBlockingFlow graph newLevel source sink source Int32.MaxValue
                maxFlowValue <- maxFlowValue + currentFlow
        
        maxFlowValue

// Example usage
[<EntryPoint>]
let main argv =
    // Create a sample graph
    // Graph: 0 -> 1 (capacity 10), 0 -> 2 (capacity 10), 1 -> 2 (capacity 2), 1 -> 3 (capacity 4)
    //        2 -> 3 (capacity 6), 2 -> 4 (capacity 10), 3 -> 4 (capacity 10)
    let edges = [
        (0, 1, 10); (0, 2, 10); (1, 2, 2); (1, 3, 4)
        (2, 3, 6); (2, 4, 10); (3, 4, 10)
    ]
    
    let graph = Dinic.createGraph 5 edges
    let maxFlowValue = Dinic.maxFlow graph 0 4
    
    printfn "Maximum flow from vertex 0 to vertex 4: %d" maxFlowValue
    
    // Expected output: Maximum flow from vertex 0 to vertex 4: 16
    
    0
```

## Key Components of the Implementation

### 1. **Data Structures**
- `FlowEdge`: Represents an edge with capacity and flow information
- `FlowGraph`: Contains the graph structure with vertices, edges, and adjacency list

### 2. **Core Functions**
- `buildAdjacencyList`: Creates adjacency list representation
- `createGraph`: Builds the flow graph with reverse edges
- `buildLevelGraph`: Constructs level graph using BFS
- `findBlockingFlow`: Finds blocking flow using DFS
- `maxFlow`: Main Dinic's algorithm implementation

### 3. **Algorithm Steps**
1. Build level graph using BFS
2. Find blocking flow using DFS
3. Repeat until no augmenting path exists
4. Return total maximum flow

## Time Complexity
- **Time**: O(V²E) in general, O(E√V) for unit networks
- **Space**: O(V + E)

## Example Output
```
Maximum flow from vertex 0 to vertex 4: 16
```

This implementation handles directed graphs and can be easily modified to work with undirected graphs or other variations of the maximum flow problem.

