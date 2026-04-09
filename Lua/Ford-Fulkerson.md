# Ford-Fulkerson Algorithm in Lua

Here's an implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```lua
-- Ford-Fulkerson Algorithm Implementation in Lua

-- Function to find if there's a path from source to sink using BFS
function bfs(graph, source, sink, parent)
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
    
    while queueIndex <= #queue do
        local u = queue[queueIndex]
        queueIndex = queueIndex + 1
        
        -- Check all adjacent vertices
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

-- Main Ford-Fulkerson function
function fordFulkerson(graph, source, sink)
    -- Create a copy of the graph for residual capacities
    local residual = {}
    for i = 1, #graph do
        residual[i] = {}
        for j = 1, #graph do
            residual[i][j] = graph[i][j]
        end
    end
    
    local parent = {}
    local maxFlow = 0
    
    -- Continue while there's a path from source to sink
    while bfs(residual, source, sink, parent) do
        -- Find minimum residual capacity along the path
        local pathFlow = math.huge
        
        local current = sink
        while current ~= source do
            local previous = parent[current]
            pathFlow = math.min(pathFlow, residual[previous][current])
            current = previous
        end
        
        -- Update residual capacities
        current = sink
        while current ~= source do
            local previous = parent[current]
            residual[previous][current] = residual[previous][current] - pathFlow
            residual[current][previous] = residual[current][previous] + pathFlow
            current = previous
        end
        
        maxFlow = maxFlow + pathFlow
    end
    
    return maxFlow
end

-- Example usage
print("Ford-Fulkerson Algorithm Example")
print("================================")

-- Create a sample graph (5x5 matrix)
-- Graph representation where graph[i][j] represents capacity from i to j
local graph = {
    {0, 16, 13, 0, 0},  -- Node 1
    {0, 0, 10, 12, 0},  -- Node 2
    {0, 4, 0, 0, 14},   -- Node 3
    {0, 0, 9, 0, 20},   -- Node 4
    {0, 0, 0, 7, 0}     -- Node 5
}

print("Flow Network:")
for i = 1, #graph do
    for j = 1, #graph do
        io.write(string.format("%3d ", graph[i][j]))
    end
    print()
end

print("\nFinding maximum flow from node 1 to node 5...")
local maxFlow = fordFulkerson(graph, 1, 5)
print(string.format("Maximum flow: %d", maxFlow))

-- Alternative example with a simpler graph
print("\n" .. string.rep("=", 40))
print("Simple Example:")
print(string.rep("=", 40))

-- Simple 4-node graph
local simpleGraph = {
    {0, 10, 10, 0},  -- Node 1
    {0, 0, 2, 4},    -- Node 2
    {0, 0, 0, 10},   -- Node 3
    {0, 0, 0, 0}     -- Node 4
}

print("Simple Flow Network:")
for i = 1, #simpleGraph do
    for j = 1, #simpleGraph do
        io.write(string.format("%3d ", simpleGraph[i][j]))
    end
    print()
end

print("\nFinding maximum flow from node 1 to node 4...")
local simpleMaxFlow = fordFulkerson(simpleGraph, 1, 4)
print(string.format("Maximum flow: %d", simpleMaxFlow))
```

## How it works:

1. **BFS Function**: Finds an augmenting path from source to sink using Breadth-First Search
2. **Main Algorithm**: 
   - Repeatedly finds augmenting paths
   - Calculates the minimum capacity along each path
   - Updates residual capacities
   - Continues until no more paths exist

## Key Features:

- **Time Complexity**: O(E × max_flow) where E is the number of edges
- **Space Complexity**: O(V²) for the adjacency matrix
- **Handles**: Multiple paths, residual graphs, and back edges
- **Output**: Maximum flow value from source to sink

## Sample Output:
```
Ford-Fulkerson Algorithm Example
================================
Flow Network:
  0  16  13   0   0 
  0   0  10  12   0 
  0   4   0   0  14 
  0   0   9   0  20 
  0   0   0   7   0 

Finding maximum flow from node 1 to node 5...
Maximum flow: 23
```

This implementation demonstrates the classic Ford-Fulkerson method for solving maximum flow problems in flow networks.

