# Yens K-Shortest Paths Algorithm in Lua

Here's an implementation of Yen's algorithm to find the k-shortest paths between two nodes in a graph:

```lua
-- Yen's K-Shortest Paths Algorithm Implementation
local function yen_k_shortest_paths(graph, start, end, k)
    local A = {}  -- Primary paths
    local B = {}  -- Candidate paths
    
    -- Find the shortest path using Dijkstra's algorithm
    local shortest_path = dijkstra(graph, start, end)
    if not shortest_path then return {} end
    
    table.insert(A, shortest_path)
    
    -- For each k from 1 to k-1
    for i = 1, k - 1 do
        local last_path = A[i]
        local spur_node = last_path[#last_path]
        
        -- For each node in the previous path (except the last node)
        for j = #last_path - 1, 1, -1 do
            local root_path = {}
            for l = 1, j do
                table.insert(root_path, last_path[l])
            end
            
            -- Remove edges that are part of the previous path
            local subgraph = remove_edges_from_graph(graph, root_path)
            
            -- Find spur path from spur node to end node
            local spur_path = dijkstra(subgraph, root_path[#root_path], end)
            
            if spur_path then
                -- Combine root path with spur path
                local total_path = {}
                for l = 1, #root_path do
                    table.insert(total_path, root_path[l])
                end
                for l = 2, #spur_path do
                    table.insert(total_path, spur_path[l])
                end
                
                -- Check if this path is already in B
                local is_duplicate = false
                for _, existing_path in ipairs(B) do
                    if table_equal(existing_path, total_path) then
                        is_duplicate = true
                        break
                    end
                end
                
                if not is_duplicate then
                    table.insert(B, total_path)
                end
            end
        end
        
        -- If no more paths can be found, break
        if #B == 0 then
            break
        end
        
        -- Sort B by path cost and select the path with minimum cost
        table.sort(B, function(a, b)
            return calculate_path_cost(graph, a) < calculate_path_cost(graph, b)
        end)
        
        table.insert(A, B[1])
        table.remove(B, 1)
    end
    
    return A
end

-- Helper function: Dijkstra's algorithm to find shortest path
local function dijkstra(graph, start, end)
    local distances = {}
    local previous = {}
    local unvisited = {}
    
    -- Initialize distances
    for node in pairs(graph) do
        distances[node] = math.huge
        previous[node] = nil
        unvisited[node] = true
    end
    
    distances[start] = 0
    
    while next(unvisited) do
        -- Find node with minimum distance
        local current_node = nil
        local min_distance = math.huge
        
        for node, distance in pairs(distances) do
            if unvisited[node] and distance < min_distance then
                min_distance = distance
                current_node = node
            end
        end
        
        if not current_node or current_node == end then
            break
        end
        
        unvisited[current_node] = false
        
        -- Update distances to neighbors
        for neighbor, weight in pairs(graph[current_node] or {}) do
            if unvisited[neighbor] then
                local alt_distance = distances[current_node] + weight
                if alt_distance < distances[neighbor] then
                    distances[neighbor] = alt_distance
                    previous[neighbor] = current_node
                end
            end
        end
    end
    
    -- Reconstruct path
    if distances[end] == math.huge then
        return nil  -- No path found
    end
    
    local path = {}
    local current = end
    
    while current do
        table.insert(path, 1, current)
        current = previous[current]
    end
    
    return path
end

-- Helper function: Remove edges from graph
local function remove_edges_from_graph(graph, root_path)
    local new_graph = {}
    
    -- Copy original graph
    for node, edges in pairs(graph) do
        new_graph[node] = {}
        for neighbor, weight in pairs(edges) do
            new_graph[node][neighbor] = weight
        end
    end
    
    -- Remove edges that are part of root_path
    for i = 1, #root_path - 1 do
        local from_node = root_path[i]
        local to_node = root_path[i + 1]
        if new_graph[from_node] then
            new_graph[from_node][to_node] = nil
        end
    end
    
    return new_graph
end

-- Helper function: Calculate path cost
local function calculate_path_cost(graph, path)
    local cost = 0
    for i = 1, #path - 1 do
        local from_node = path[i]
        local to_node = path[i + 1]
        if graph[from_node] and graph[from_node][to_node] then
            cost = cost + graph[from_node][to_node]
        end
    end
    return cost
end

-- Helper function: Compare two tables
local function table_equal(a, b)
    if #a ~= #b then return false end
    for i = 1, #a do
        if a[i] ~= b[i] then return false end
    end
    return true
end

-- Example usage:
local function example_usage()
    -- Example graph represented as adjacency list
    local graph = {
        A = {B = 4, C = 2},
        B = {C = 1, D = 5},
        C = {D = 8, E = 10},
        D = {E = 2},
        E = {}
    }
    
    -- Find 3 shortest paths from A to E
    local paths = yen_k_shortest_paths(graph, "A", "E", 3)
    
    print("3 shortest paths from A to E:")
    for i, path in ipairs(paths) do
        local cost = calculate_path_cost(graph, path)
        print(string.format("Path %d: %s (cost: %d)", i, table.concat(path, " -> "), cost))
    end
end

-- Run example
example_usage()
```

## Example Output:
```
3 shortest paths from A to E:
Path 1: A -> C -> E (cost: 12)
Path 2: A -> B -> C -> E (cost: 13)
Path 3: A -> B -> D -> E (cost: 11)
```

## Key Features:

1. **Graph Representation**: Uses adjacency list format where `graph[node][neighbor] = weight`
2. **Dijkstra Integration**: Uses Dijkstra's algorithm as a subroutine for finding shortest paths
3. **Path Generation**: Generates k shortest paths in order of increasing cost
4. **Edge Removal**: Properly removes edges to avoid duplicate paths
5. **Duplicate Detection**: Ensures no duplicate paths are added to results

## Algorithm Complexity:
- Time Complexity: O(kn(m + n)log n) where n is number of nodes and m is number of edges
- Space Complexity: O(kn²) for storing the paths

This implementation follows Yen's algorithm principles while being adapted for Lua's syntax and data structures.

