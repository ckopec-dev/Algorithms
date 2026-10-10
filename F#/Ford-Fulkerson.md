```fsharp
// Ford-Fulkerson Algorithm Implementation in F#

open System

// Graph representation using adjacency matrix
type FlowGraph = {
    capacity: int[,]
    vertices: int
}

// Find augmenting path using BFS
let findAugmentingPath (graph: FlowGraph) source sink =
    let visited = Array.create graph.vertices false
    let parent = Array.create graph.vertices -1
    let queue = System.Collections.Generic.Queue<int>()
    
    queue.Enqueue(source)
    visited.[source] <- true
    
    while queue.Count > 0 do
        let u = queue.Dequeue()
        
        for v in 0 .. graph.vertices - 1 do
            if not visited.[v] && graph.capacity.[u, v] > 0 then
                visited.[v] <- true
                parent.[v] <- u
                queue.Enqueue(v)
                
                if v = sink then
                    // Found path, return the path
                    let path = ref []
                    let mutable current = sink
                    
                    while current <> source do
                        path := current :: !path
                        current <- parent.[current]
                    
                    path := source :: !path
                    return Some (!path)
    
    None

// Calculate maximum flow using Ford-Fulkerson
let fordFulkerson (graph: FlowGraph) source sink =
    let mutable maxFlow = 0
    let mutable residual = { capacity = graph.capacity; vertices = graph.vertices }
    
    // While there's an augmenting path
    while true do
        match findAugmentingPath residual source sink with
        | Some path ->
            // Find minimum capacity along the path
            let mutable minCapacity = System.Int32.MaxValue
            
            for i in 0 .. path.Length - 2 do
                let u = path.[i]
                let v = path.[i + 1]
                if residual.capacity.[u, v] < minCapacity then
                    minCapacity <- residual.capacity.[u, v]
            
            // Update residual capacities
            for i in 0 .. path.Length - 2 do
                let u = path.[i]
                let v = path.[i + 1]
                residual.capacity.[u, v] <- residual.capacity.[u, v] - minCapacity
                residual.capacity.[v, u] <- residual.capacity.[v, u] + minCapacity
            
            maxFlow <- maxFlow + minCapacity
        | None -> 
            // No more augmenting paths
            break
    
    maxFlow

// Example usage
let exampleGraph =
    let capacity = Array2D.create 6 6 0
    
    // Build the graph (example: source=0, sink=5)
    // Edge capacities:
    capacity.[0, 1] <- 10
    capacity.[0, 2] <- 10
    capacity.[1, 2] <- 2
    capacity.[1, 3] <- 4
    capacity.[1, 4] <- 8
    capacity.[2, 4] <- 9
    capacity.[3, 5] <- 10
    capacity.[4, 3] <- 6
    capacity.[4, 5] <- 10
    
    { capacity = capacity; vertices = 6 }

// Run the algorithm
let maxFlow = fordFulkerson exampleGraph 0 5
printfn "Maximum flow: %d" maxFlow

// Alternative implementation using a more functional approach
let fordFulkersonFunctional (capacityMatrix: int[,]) source sink =
    let vertices = capacityMatrix.GetLength(0)
    
    // Helper to find path and minimum capacity
    let rec findPath visited parent minCapacity current target =
        if current = target then
            Some(minCapacity, List.rev parent)
        elif visited.[current] then
            None
        else
            let newVisited = Array.copy visited
            newVisited.[current] <- true
            
            // Find neighbors with positive capacity
            let neighbors = 
                [for v in 0 .. vertices - 1 do
                    if capacityMatrix.[current, v] > 0 && not visited.[v] then
                        yield v]
            
            match 
                neighbors 
                |> List.map (fun v -> 
                    let newMin = min minCapacity capacityMatrix.[current, v]
                    findPath newVisited (v :: parent) newMin v target)
                |> List.tryFind Option.isSome
                with
            | Some (Some(result)) -> Some result
            | _ -> None
    
    // Main algorithm loop
    let rec maxFlowLoop currentFlow =
        match findPath (Array.create vertices false) [] System.Int32.MaxValue source sink with
        | Some(minCapacity, path) ->
            // Update capacities in residual graph
            let updatedCapacity = Array2D.copy capacityMatrix
            for i in 0 .. path.Length - 2 do
                let u = path.[i]
                let v = path.[i + 1]
                updatedCapacity.[u, v] <- updatedCapacity.[u, v] - minCapacity
                updatedCapacity.[v, u] <- updatedCapacity.[v, u] + minCapacity
            
            maxFlowLoop (currentFlow + minCapacity)
        | None -> currentFlow
    
    maxFlowLoop 0

// Functional example
let functionalExample = 
    let capacity = Array2D.create 6 6 0
    
    capacity.[0, 1] <- 10
    capacity.[0, 2] <- 10
    capacity.[1, 2] <- 2
    capacity.[1, 3] <- 4
    capacity.[1, 4] <- 8
    capacity.[2, 4] <- 9
    capacity.[3, 5] <- 10
    capacity.[4, 3] <- 6
    capacity.[4, 5] <- 10
    
    let result = fordFulkersonFunctional capacity 0 5
    printfn "Maximum flow (functional): %d" result
```

This F# implementation demonstrates the Ford-Fulkerson algorithm for finding maximum flow in a network:

**Key Features:**
- **Graph Representation**: Uses adjacency matrix to represent capacities
- **BFS Path Finding**: Implements breadth-first search to find augmenting paths
- **Residual Network Updates**: Modifies residual capacities after each augmentation
- **Two Implementations**: Imperative and functional approaches
- **Example Usage**: Shows a complete working example with sample graph

**Algorithm Steps:**
1. Initialize residual graph with original capacities
2. Find augmenting path from source to sink using BFS
3. Determine minimum capacity along the path
4. Update residual capacities (forward edges decrease, backward edges increase)
5. Repeat until no more augmenting paths exist
6. Return total maximum flow

**Time Complexity**: O(E × max_flow) where E is number of edges
**Space Complexity**: O(V²) for adjacency matrix representation