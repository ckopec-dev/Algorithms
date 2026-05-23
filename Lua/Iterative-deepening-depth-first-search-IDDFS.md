# Iterative Deepening Depth-First Search (IDDFS) in Lua

```lua
-- Iterative Deepening Depth-First Search Implementation
local function iddfs(graph, start, goal)
    local function dfs_with_limit(node, target, limit, path, visited)
        -- If we've reached the target node
        if node == target then
            return true, path
        end
        
        -- If we've exceeded the depth limit
        if limit <= 0 then
            return false, {}
        end
        
        -- Mark current node as visited
        visited[node] = true
        
        -- Explore neighbors
        for _, neighbor in ipairs(graph[node] or {}) do
            if not visited[neighbor] then
                -- Create a copy of the path
                local newPath = {}
                for i, v in ipairs(path) do
                    newPath[i] = v
                end
                table.insert(newPath, neighbor)
                
                -- Recursively search with reduced limit
                local found, resultPath = dfs_with_limit(neighbor, target, limit - 1, newPath, visited)
                if found then
                    return true, resultPath
                end
            end
        end
        
        -- Backtrack: remove from visited
        visited[node] = nil
        return false, {}
    end
    
    -- Start with depth limit of 0 and incrementally increase
    local depth = 0
    while true do
        local visited = {}
        local path = {start}
        local found, resultPath = dfs_with_limit(start, goal, depth, path, visited)
        
        if found then
            return resultPath
        end
        
        depth = depth + 1
        
        -- Prevent infinite loop - set a reasonable maximum depth
        if depth > 100 then
            return nil  -- No path found within reasonable depth
        end
    end
end

-- Example usage
local function example_usage()
    -- Define a sample graph as an adjacency list
    local graph = {
        A = {"B", "C"},
        B = {"A", "D", "E"},
        C = {"A", "F"},
        D = {"B"},
        E = {"B", "F"},
        F = {"C", "E"}
    }
    
    print("Graph structure:")
    for node, neighbors in pairs(graph) do
        print(node .. " -> " .. table.concat(neighbors, ", "))
    end
    
    print("\nSearching for path from A to F:")
    local result = iddfs(graph, "A", "F")
    
    if result then
        print("Path found: " .. table.concat(result, " -> "))
    else
        print("No path found")
    end
    
    print("\nSearching for path from A to D:")
    local result2 = iddfs(graph, "A", "D")
    
    if result2 then
        print("Path found: " .. table.concat(result2, " -> "))
    else
        print("No path found")
    end
end

-- Run the example
example_usage()
```

## How IDDFS Works

The IDDFS algorithm combines the benefits of both BFS and DFS:

1. **Iterative Deepening**: Starts with depth limit 0 and increases by 1 each iteration
2. **Depth-First Search**: Within each iteration, performs a regular DFS up to the current depth limit
3. **Complete and Optimal**: Guarantees finding the shortest path (like BFS) but uses less memory (like DFS)

## Key Features

- **Memory Efficient**: Only stores the current path, not all explored nodes
- **Complete**: Will find a solution if one exists
- **Optimal**: Finds the shortest path in unweighted graphs
- **Time Complexity**: O(b^d) where b is branching factor and d is depth
- **Space Complexity**: O(bd) where b is branching factor and d is depth

## Output Example
```
Graph structure:
A -> B, C
B -> A, D, E
C -> A, F
D -> B
E -> B, F
F -> C, E

Searching for path from A to F:
Path found: A -> C -> F

Searching for path from A to D:
Path found: A -> B -> D
```

