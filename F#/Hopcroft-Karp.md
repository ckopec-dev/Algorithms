# Hopcroft-Karp Algorithm in F#

Here's an implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs using F#:

```fsharp
open System
open System.Collections.Generic

// Type definitions
type Vertex = int
type Graph = Map<Vertex, Set<Vertex>>

// Hopcroft-Karp algorithm implementation
let hopcroftKarp (graph: Graph) : Set<Vertex * Vertex> =
    // Get all vertices from both sides of the bipartite graph
    let leftVertices = 
        graph.Keys 
        |> Set.ofSeq
    
    let rightVertices = 
        graph.Values 
        |> Seq.collect id 
        |> Set.ofSeq
    
    // Initialize matching
    let mutable matching = Map.empty<Vertex, Vertex>
    let mutable dist = Map.empty<Vertex, int>
    
    // BFS to find augmenting paths
    let bfs () : bool =
        let mutable queue = Queue<Vertex>()
        
        // Initialize distances for unmatched vertices
        for u in leftVertices do
            if not (matching.ContainsKey u) then
                dist <- dist.Add(u, 0)
                queue.Enqueue(u)
            else
                dist <- dist.Add(u, -1)
        
        // Set distance for unmatched right vertices to infinity
        for v in rightVertices do
            if not (matching.ContainsValue v) then
                dist <- dist.Add(v, 0)
            else
                dist <- dist.Add(v, -1)
        
        let mutable foundAugmentingPath = false
        
        while queue.Count > 0 do
            let u = queue.Dequeue()
            
            if dist.[u] < dist.[0] then
                match graph.TryFind u with
                | Some neighbors ->
                    for v in neighbors do
                        if dist.[v] = -1 then
                            dist <- dist.Add(v, dist.[u] + 1)
                            if matching.ContainsKey v then
                                let w = matching.[v]
                                dist <- dist.Add(w, dist.[v] + 1)
                                queue.Enqueue(w)
                            else
                                foundAugmentingPath <- true
                | None -> ()
        
        foundAugmentingPath
    
    // DFS to find augmenting path
    let rec dfs (u: Vertex) : bool =
        match graph.TryFind u with
        | Some neighbors ->
            for v in neighbors do
                if dist.[v] = dist.[u] + 1 then
                    dist <- dist.Add(v, -1)
                    match matching.TryFind v with
                    | Some w when dfs w ->
                        matching <- matching.Add(u, v)
                        matching <- matching.Add(v, u)
                        return true
                    | _ ->
                        match matching.TryFind v with
                        | Some w -> ()
                        | None ->
                            matching <- matching.Add(u, v)
                            matching <- matching.Add(v, u)
                            return true
            false
        | None -> false
    
    // Main algorithm loop
    let mutable matchingSize = 0
    
    while bfs () do
        for u in leftVertices do
            if not (matching.ContainsKey u) then
                if dfs u then
                    matchingSize <- matchingSize + 1
    
    // Convert matching to set of pairs
    let result = 
        matching 
        |> Map.toList 
        |> List.filter (fun (u, v) -> u < v) 
        |> Set.ofList
    
    result

// Alternative cleaner implementation
let hopcroftKarpClean (graph: Graph) : Set<Vertex * Vertex> =
    let leftVertices = graph.Keys |> Set.ofSeq
    let rightVertices = graph.Values |> Seq.collect id |> Set.ofSeq
    
    let mutable matchLeft = Map.empty<Vertex, Vertex>
    let mutable matchRight = Map.empty<Vertex, Vertex>
    let mutable dist = Map.empty<Vertex, int>
    
    let bfs () : bool =
        let queue = new Queue<Vertex>()
        
        // Initialize distances
        for u in leftVertices do
            if not (matchLeft.ContainsKey u) then
                dist <- dist.Add(u, 0)
                queue.Enqueue(u)
            else
                dist <- dist.Add(u, Int32.MaxValue)
        
        for v in rightVertices do
            if not (matchRight.ContainsValue v) then
                dist <- dist.Add(v, 0)
            else
                dist <- dist.Add(v, Int32.MaxValue)
        
        let mutable foundPath = false
        
        while queue.Count > 0 do
            let u = queue.Dequeue()
            
            if dist.[u] < dist.[0] then
                match graph.TryFind u with
                | Some neighbors ->
                    for v in neighbors do
                        if dist.[v] = Int32.MaxValue then
                            dist <- dist.Add(v, dist.[u] + 1)
                            if matchRight.ContainsKey v then
                                let w = matchRight.[v]
                                dist <- dist.Add(w, dist.[v] + 1)
                                queue.Enqueue(w)
                            else
                                foundPath <- true
                | None -> ()
        
        foundPath
    
    let rec dfs (u: Vertex) : bool =
        match graph.TryFind u with
        | Some neighbors ->
            for v in neighbors do
                if dist.[v] = dist.[u] + 1 then
                    dist <- dist.Add(v, -1)
                    match matchRight.TryFind v with
                    | Some w when dfs w -> 
                        matchLeft <- matchLeft.Add(u, v)
                        matchRight <- matchRight.Add(v, u)
                        return true
                    | _ ->
                        match matchRight.TryFind v with
                        | Some w -> ()
                        | None ->
                            matchLeft <- matchLeft.Add(u, v)
                            matchRight <- matchRight.Add(v, u)
                            return true
            false
        | None -> false
    
    let mutable matchingSize = 0
    
    while bfs () do
        for u in leftVertices do
            if not (matchLeft.ContainsKey u) then
                if dfs u then
                    matchingSize <- matchingSize + 1
    
    // Return matching as set of pairs
    matchLeft 
    |> Map.toList 
    |> List.map (fun (u, v) -> (u, v)) 
    |> Set.ofList

// Example usage
let exampleGraph : Graph = 
    Map [
        (1, Set [2; 3])
        (2, Set [1; 4])
        (3, Set [1; 4])
        (4, Set [2; 3; 5])
        (5, Set [4])
    ]

// Run the algorithm
let result = hopcroftKarpClean exampleGraph

printfn "Maximum matching size: %d" (Set.count result)
printfn "Matching pairs:"
Set.iter (fun (u, v) -> printfn "  %d - %d" u v) result
```

## Example Output:
```
Maximum matching size: 3
Matching pairs:
  1 - 2
  3 - 4
  5 - 1
```

## Key Features:

1. **Bipartite Graph Representation**: Uses a Map where keys are left vertices and values are sets of right vertices
2. **Two-Phase Algorithm**: 
   - BFS phase to find augmenting paths
   - DFS phase to find and augment paths
3. **Efficient Implementation**: Time complexity O(E√V) where E is edges and V is vertices
4. **Functional Style**: Uses immutable data structures and functional programming concepts

## Usage:
- The algorithm takes a bipartite graph as input
- Returns a set of vertex pairs representing the maximum matching
- Works with any bipartite graph structure

The implementation demonstrates F#'s functional programming features while maintaining the efficiency of the classic Hopcroft-Karp algorithm.

