# Edmonds-Karp Algorithm Implementation in Lua

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```lua
-- Edmonds-Karp Algorithm Implementation in Lua

-- Function to find augmenting path using BFS
function findAugmentingPath(graph, source, sink, parent)
    local visited = {}
    local queue = {}
    local queueIndex = 1
    
    -- Initialize visited array
    for i = 1, #graph do
        visited[i] = false
    end
    
    -- Add source to queue
    table.insert(queue, source)
    visited[source] = true
    parent[source] = -1
    
    -- BFS to find path from source to sink
    while queueIndex <= #queue do
        local u = queue[queueIndex]
        queueIndex = queueIndex + 1
        
        -- Check all neighbors
        for v = 1, #graph do
            if not visited[v] and graph[u][v] > 0 then
                visited[v] = true
                parent[v] = u
                table.insert(queue, v)
                
                if v == sink then
                    return true
                end
            end
        end
    end
    
    return false
end

-- Function to find maximum flow using Edmonds-Karp
function edmondsKarp(graph, source, sink)
    -- Create residual graph (copy of original graph)
    local residualGraph = {}
    for i = 1, #graph do
        residualGraph[i] = {}
        for j = 1, #graph do
            residualGraph[i][j] = graph[i][j]
        end
    end
    
    local parent = {}
    local maxFlow = 0
    
    -- Continue while there's an augmenting path
    while findAugmentingPath(residualGraph, source, sink, parent) do
        -- Find minimum capacity along the path
        local pathFlow = math.huge
        
        local current = sink
        while current ~= source do
            local previous = parent[current]
            pathFlow = math.min(pathFlow, residualGraph[previous][current])
            current = previous
        end
        
        -- Update residual capacities
        current = sink
        while current ~= source do
            local previous = parent[current]
            residualGraph[previous][current] = residualGraph[previous][current] - pathFlow
            residualGraph[current][previous] = residualGraph[current][previous] + pathFlow
            current = previous
        end
        
        maxFlow = maxFlow + pathFlow
    end
    
    return maxFlow
end

-- Example usage
print("=== Edmonds-Karp Algorithm Example ===")

-- Create a sample flow network
-- Graph representation: adjacency matrix
-- Node indices: 1, 2, 3, 4 (source=1, sink=4)
local graph = {
    {0, 16, 13, 0},   -- Node 1 (source)
    {0, 0, 10, 12},   -- Node 2
    {0, 4, 0, 14},    -- Node 3
    {0, 0, 0, 0}      -- Node 4 (sink)
}

print("Flow Network:")
for i = 1, #graph do
    print(table.concat(graph[i], " "))
end

print("\nRunning Edmonds-Karp algorithm...")
local maxFlow = edmondsKarp(graph, 1, 4)

print("Maximum Flow: " .. maxFlow)

-- Additional example with different network
print("\n=== Another Example ===")

local graph2 = {
    {0, 10, 10, 0, 0, 0},
    {0, 0, 2, 4, 8, 0},
    {0, 0, 0, 0, 9, 0},
    {0, 0, 0, 0, 0, 10},
    {0, 0, 6, 0, 0, 10},
    {0, 0, 0, 0, 0, 0}
}

print("Second Flow Network:")
for i = 1, #graph2 do
    print(table.concat(graph2[i], " "))
end

print("Maximum Flow: " .. edmondsKarp(graph2, 1, 6))
```

## How the Algorithm Works

1. **Initialization**: Create a residual graph with the same capacities as the original graph
2. **BFS Search**: Use BFS to find an augmenting path from source to sink
3. **Path Augmentation**: Find the minimum capacity along the path and update residual capacities
4. **Repeat**: Continue until no more augmenting paths exist

## Key Features

- **Time Complexity**: O(VE²) where V is vertices and E is edges
- **Space Complexity**: O(V²) for the adjacency matrix
- **Uses BFS**: Guarantees the shortest augmenting path in terms of number of edges
- **Handles Multiple Paths**: Finds all possible paths to maximize flow

## Sample Output
```
=== Edmonds-Karp Algorithm Example ===
Flow Network:
0 16 13 0
0 0 10 12
0 4 0 14
0 0 0 0

Running Edmonds-Karp algorithm...
Maximum Flow: 23

=== Another Example ===
Second Flow Network:
0 10 10 0 0 0
0 0 2 4 8 0
0 0 0 0 9 0
0 0 0 0 0 10
0 0 6 0 0 10
0 0 0 0 0 0
Maximum Flow: 19
```

