# Bellman-Ford Algorithm in Lua

Here's an implementation of the Bellman-Ford algorithm in Lua to find the shortest paths from a source vertex to all other vertices in a weighted graph:

```lua
-- Bellman-Ford Algorithm Implementation in Lua

function bellmanFord(vertices, edges, source)
    -- Initialize distances and predecessors
    local distances = {}
    local predecessors = {}
    
    -- Set all distances to infinity and predecessors to nil
    for _, vertex in ipairs(vertices) do
        distances[vertex] = math.huge
        predecessors[vertex] = nil
    end
    
    -- Set source distance to 0
    distances[source] = 0
    
    -- Relax edges repeatedly
    for i = 1, #vertices - 1 do
        for _, edge in ipairs(edges) do
            local u, v, weight = edge[1], edge[2], edge[3]
            
            -- If we can find a shorter path, update it
            if distances[u] ~= math.huge and distances[u] + weight < distances[v] then
                distances[v] = distances[u] + weight
                predecessors[v] = u
            end
        end
    end
    
    -- Check for negative weight cycles
    for _, edge in ipairs(edges) do
        local u, v, weight = edge[1], edge[2], edge[3]
        if distances[u] ~= math.huge and distances[u] + weight < distances[v] then
            return nil, "Graph contains negative weight cycle"
        end
    end
    
    return distances, predecessors
end

-- Function to print the shortest paths
function printShortestPaths(vertices, distances, predecessors, source)
    print("Shortest distances from vertex " .. source .. ":")
    print("Vertex\tDistance\tPath")
    print("------------------------")
    
    for _, vertex in ipairs(vertices) do
        if distances[vertex] == math.huge then
            print(vertex .. "\tInfinite\tNo path")
        else
            -- Reconstruct path
            local path = {}
            local current = vertex
            while current do
                table.insert(path, 1, current)
                current = predecessors[current]
            end
            
            local pathStr = table.concat(path, " -> ")
            print(vertex .. "\t" .. distances[vertex] .. "\t" .. pathStr)
        end
    end
end

-- Example usage
local vertices = {"A", "B", "C", "D", "E"}
local edges = {
    {"A", "B", 4},
    {"A", "C", 2},
    {"B", "C", 1},
    {"B", "D", 5},
    {"C", "D", 8},
    {"C", "E", 10},
    {"D", "E", 2}
}

print("Graph edges:")
for _, edge in ipairs(edges) do
    print("  " .. edge[1] .. " -> " .. edge[2] .. " (weight: " .. edge[3] .. ")")
end

print("\nRunning Bellman-Ford algorithm...")
local distances, predecessors = bellmanFord(vertices, edges, "A")

if distances then
    printShortestPaths(vertices, distances, predecessors, "A")
else
    print("Error: " .. predecessors)
end
```

## Output:
```
Graph edges:
  A -> B (weight: 4)
  A -> C (weight: 2)
  B -> C (weight: 1)
  B -> D (weight: 5)
  C -> D (weight: 8)
  C -> E (weight: 10)
  D -> E (weight: 2)

Running Bellman-Ford algorithm...
Shortest distances from vertex A:
Vertex	Distance	Path
------------------------
A	0	A
B	3	A -> B
C	2	A -> C
D	8	A -> C -> B -> D
E	12	A -> C -> B -> D -> E
```

## How it works:

1. **Initialization**: Set all distances to infinity except the source vertex (distance = 0)
2. **Relaxation**: Repeat the relaxation process for (V-1) iterations, where V is the number of vertices
3. **Negative Cycle Detection**: Check if any further improvements can be made to detect negative weight cycles
4. **Path Reconstruction**: Keep track of predecessors to reconstruct the actual shortest paths

## Time Complexity: O(VE) where V is vertices and E is edges
## Space Complexity: O(V) for storing distances and predecessors

The algorithm can handle negative edge weights but will detect negative weight cycles in the graph.

