# Topological Sort in Lua

Here's an implementation of topological sort using Kahn's algorithm in Lua:

```lua
-- Function to perform topological sort on a directed acyclic graph
function topological_sort(graph)
    local in_degree = {}
    local queue = {}
    local result = {}
    
    -- Initialize in-degree for all vertices
    for vertex, _ in pairs(graph) do
        in_degree[vertex] = 0
    end
    
    -- Calculate in-degrees for all vertices
    for vertex, neighbors in pairs(graph) do
        for _, neighbor in ipairs(neighbors) do
            in_degree[neighbor] = in_degree[neighbor] + 1
        end
    end
    
    -- Add all vertices with in-degree 0 to queue
    for vertex, degree in pairs(in_degree) do
        if degree == 0 then
            table.insert(queue, vertex)
        end
    end
    
    -- Process vertices in queue
    while #queue > 0 do
        local current = table.remove(queue, 1)
        table.insert(result, current)
        
        -- Reduce in-degree of neighbors
        for _, neighbor in ipairs(graph[current] or {}) do
            in_degree[neighbor] = in_degree[neighbor] - 1
            if in_degree[neighbor] == 0 then
                table.insert(queue, neighbor)
            end
        end
    end
    
    -- Check for cycles
    for _, degree in pairs(in_degree) do
        if degree > 0 then
            return nil, "Cycle detected in graph"
        end
    end
    
    return result
end

-- Example usage
local graph = {
    A = {"B", "C"},
    B = {"D"},
    C = {"D"},
    D = {}
}

print("Graph edges:")
for vertex, neighbors in pairs(graph) do
    print(vertex .. " -> " .. table.concat(neighbors, ", "))
end

local sorted, error = topological_sort(graph)
if error then
    print("Error: " .. error)
else
    print("Topological order: " .. table.concat(sorted, " -> "))
end

-- Example with cycle (will detect cycle)
print("\n--- Testing with cycle ---")
local graph_with_cycle = {
    A = {"B"},
    B = {"C"},
    C = {"A"}
}

local sorted2, error2 = topological_sort(graph_with_cycle)
if error2 then
    print("Error: " .. error2)
else
    print("Topological order: " .. table.concat(sorted2, " -> "))
end
```

## Output:
```
Graph edges:
A -> B, C
B -> D
C -> D
D -> 
Topological order: A -> B -> C -> D

--- Testing with cycle ---
Error: Cycle detected in graph
```

## How it works:

1. **Initialize**: Calculate in-degrees for all vertices
2. **Queue**: Add all vertices with in-degree 0 to a queue
3. **Process**: Remove vertices from queue, add to result, and reduce in-degrees of neighbors
4. **Repeat**: Continue until queue is empty
5. **Cycle Detection**: If any vertex still has in-degree > 0, there's a cycle

## Time Complexity: O(V + E) where V is vertices and E is edges
## Space Complexity: O(V) for the queue and in-degree array

