# Prim's Algorithm in Lua

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) in Lua:

```lua
-- Prim's Algorithm Implementation in Lua

-- Function to find the vertex with minimum key value
local function minKey(keys, visited, numVertices)
    local min = math.huge
    local minIndex = -1
    
    for v = 1, numVertices do
        if not visited[v] and keys[v] < min then
            min = keys[v]
            minIndex = v
        end
    end
    
    return minIndex
end

-- Function to print the MST
local function printMST(parent, graph, numVertices)
    print("Edge \tWeight")
    for i = 2, numVertices do
        print(parent[i] .. " - " .. i .. "\t" .. graph[i][parent[i]])
    end
end

-- Prim's algorithm implementation
local function primMST(graph, numVertices)
    -- Initialize arrays
    local keys = {}
    local visited = {}
    local parent = {}
    
    -- Initialize all keys as infinity and visited as false
    for i = 1, numVertices do
        keys[i] = math.huge
        visited[i] = false
        parent[i] = -1
    end
    
    -- Start with vertex 1 (can be any vertex)
    keys[1] = 0
    
    -- Find MST
    for count = 1, numVertices do
        -- Pick the minimum key vertex from the set of vertices not yet included
        local u = minKey(keys, visited, numVertices)
        
        -- Mark the picked vertex as visited
        visited[u] = true
        
        -- Update key values of adjacent vertices
        for v = 1, numVertices do
            if not visited[v] and graph[u][v] ~= 0 and graph[u][v] < keys[v] then
                parent[v] = u
                keys[v] = graph[u][v]
            end
        end
    end
    
    -- Print the constructed MST
    printMST(parent, graph, numVertices)
end

-- Example usage
print("Prim's Algorithm Example")
print("========================")

-- Create a graph represented as adjacency matrix
-- Vertices: 1, 2, 3, 4, 5
local graph = {
    {0, 2, 0, 6, 0},  -- Vertex 1
    {2, 0, 3, 8, 5},  -- Vertex 2
    {0, 3, 0, 0, 7},  -- Vertex 3
    {6, 8, 0, 0, 9},  -- Vertex 4
    {0, 5, 7, 9, 0}   -- Vertex 5
}

local numVertices = 5

print("Graph represented as adjacency matrix:")
for i = 1, numVertices do
    print(table.concat(graph[i], " "))
end

print("\nMinimum Spanning Tree:")
primMST(graph, numVertices)
```

## Output:
```
Prim's Algorithm Example
========================
Graph represented as adjacency matrix:
0 2 0 6 0
2 0 3 8 5
0 3 0 0 7
6 8 0 0 9
0 5 7 9 0

Minimum Spanning Tree:
Edge 	Weight
1 - 2	2
2 - 3	3
1 - 4	6
2 - 5	5
```

## How it works:

1. **Initialization**: Start with any vertex (vertex 1 in this case) and set its key value to 0
2. **Main Loop**: 
   - Find the vertex with minimum key value that hasn't been visited
   - Mark it as visited
   - Update key values of all adjacent vertices
3. **Repeat**: Continue until all vertices are included in the MST

## Time Complexity: O(V²) where V is the number of vertices
## Space Complexity: O(V) for the auxiliary arrays

The algorithm constructs a minimum spanning tree by greedily selecting the smallest weight edge that connects a vertex in the tree to a vertex outside the tree.

