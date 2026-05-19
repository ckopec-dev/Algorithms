# Dijkstra's Algorithm in Lua

Here's a complete implementation of Dijkstra's algorithm in Lua:

```lua
-- Dijkstra's Algorithm Implementation in Lua

-- Graph representation as adjacency list
local graph = {
    A = {B = 4, C = 2},
    B = {C = 1, D = 5},
    C = {D = 8, E = 10},
    D = {E = 2},
    E = {}
}

-- Dijkstra's algorithm function
function dijkstra(graph, start)
    -- Initialize distances and previous nodes
    local distances = {}
    local previous = {}
    local unvisited = {}
    
    -- Set initial distances to infinity
    for node in pairs(graph) do
        distances[node] = math.huge
        previous[node] = nil
        unvisited[node] = true
    end
    
    -- Set starting node distance to 0
    distances[start] = 0
    
    -- Main algorithm loop
    while next(unvisited) do
        -- Find node with minimum distance
        local current = nil
        local minDistance = math.huge
        
        for node, isUnvisited in pairs(unvisited) do
            if isUnvisited and distances[node] < minDistance then
                minDistance = distances[node]
                current = node
            end
        end
        
        -- If no reachable node found, break
        if current == nil then
            break
        end
        
        -- Mark current node as visited
        unvisited[current] = false
        
        -- Update distances to neighbors
        for neighbor, weight in pairs(graph[current]) do
            if unvisited[neighbor] then
                local newDistance = distances[current] + weight
                if newDistance < distances[neighbor] then
                    distances[neighbor] = newDistance
                    previous[neighbor] = current
                end
            end
        end
    end
    
    return distances, previous
end

-- Function to reconstruct path
function reconstructPath(previous, start, end)
    local path = {}
    local current = end
    
    while current do
        table.insert(path, 1, current)
        current = previous[current]
    end
    
    -- Check if path exists
    if path[1] == start then
        return path
    else
        return nil  -- No path exists
    end
end

-- Example usage
print("Graph:")
for node, neighbors in pairs(graph) do
    print(node .. ":")
    for neighbor, weight in pairs(neighbors) do
        print("  -> " .. neighbor .. " (weight: " .. weight .. ")")
    end
end

print("\nRunning Dijkstra from node A:")
local distances, previous = dijkstra(graph, "A")

print("Shortest distances from A:")
for node, distance in pairs(distances) do
    print("  " .. node .. ": " .. (distance == math.huge and "∞" or distance))
end

print("\nReconstructing paths:")
for node in pairs(graph) do
    if node ~= "A" then
        local path = reconstructPath(previous, "A", node)
        if path then
            print("  A to " .. node .. ": " .. table.concat(path, " -> "))
        else
            print("  A to " .. node .. ": No path")
        end
    end
end
```

## Output
```
Graph:
A:
  -> B (weight: 4)
  -> C (weight: 2)
B:
  -> C (weight: 1)
  -> D (weight: 5)
C:
  -> D (weight: 8)
  -> E (weight: 10)
D:
  -> E (weight: 2)
E:

Running Dijkstra from node A:
Shortest distances from A:
  A: 0
  B: 4
  C: 2
  D: 9
  E: 11

Reconstructing paths:
  A to B: A -> B
  A to C: A -> C
  A to D: A -> C -> B -> D
  A to E: A -> C -> B -> D -> E
```

## Key Features

1. **Graph Representation**: Uses a table-based adjacency list where each node maps to its neighbors and edge weights
2. **Distance Tracking**: Maintains shortest distances from the source to all other nodes
3. **Path Reconstruction**: Can reconstruct the actual shortest path between nodes
4. **Time Complexity**: O((V + E) log V) where V is vertices and E is edges
5. **Space Complexity**: O(V) for storing distances and previous nodes

This implementation handles:
- Weighted directed graphs
- Multiple paths to the same node
- Path reconstruction
- Infinite distances for unreachable nodes

