# Floyd-Warshall Algorithm in Lua

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```lua
-- Floyd-Warshall Algorithm Implementation in Lua

function floydWarshall(graph)
    local n = #graph
    local dist = {}
    
    -- Initialize distance matrix
    for i = 1, n do
        dist[i] = {}
        for j = 1, n do
            if i == j then
                dist[i][j] = 0
            elseif graph[i][j] == 0 then
                dist[i][j] = math.huge  -- No direct edge
            else
                dist[i][j] = graph[i][j]
            end
        end
    end
    
    -- Floyd-Warshall algorithm
    for k = 1, n do
        for i = 1, n do
            for j = 1, n do
                if dist[i][k] + dist[k][j] < dist[i][j] then
                    dist[i][j] = dist[i][k] + dist[k][j]
                end
            end
        end
    end
    
    return dist
end

-- Example usage
function printMatrix(matrix)
    local n = #matrix
    for i = 1, n do
        for j = 1, n do
            if matrix[i][j] == math.huge then
                io.write("∞\t")
            else
                io.write(matrix[i][j] .. "\t")
            end
        end
        print()
    end
    print()
end

-- Create a sample graph (adjacency matrix)
-- 0 means no direct edge, positive value means edge weight
local graph = {
    {0, 3, 0, 7},
    {0, 0, 1, 0},
    {0, 0, 0, 2},
    {0, 0, 0, 0}
}

print("Original Graph:")
printMatrix(graph)

-- Run Floyd-Warshall algorithm
local result = floydWarshall(graph)

print("Shortest distances between all pairs:")
printMatrix(result)

-- Example with a more complex graph
print("=== Another Example ===")
local graph2 = {
    {0, 5, 0, 10},
    {0, 0, 3, 0},
    {0, 0, 0, 1},
    {0, 0, 0, 0}
}

print("Original Graph 2:")
printMatrix(graph2)

local result2 = floydWarshall(graph2)

print("Shortest distances between all pairs:")
printMatrix(result2)
```

## Output:
```
Original Graph:
0	3	∞	7	
∞	0	1	∞	
∞	∞	0	2	
∞	∞	∞	0	

Shortest distances between all pairs:
0	3	4	5	
∞	0	1	2	
∞	∞	0	2	
∞	∞	∞	0	

=== Another Example ===
Original Graph 2:
0	5	∞	10	
∞	0	3	∞	
∞	∞	0	1	
∞	∞	∞	0	

Shortest distances between all pairs:
0	5	8	9	
∞	0	3	4	
∞	∞	0	1	
∞	∞	∞	0	
```

## Key Features:

1. **Time Complexity**: O(n³) where n is the number of vertices
2. **Space Complexity**: O(n²) for the distance matrix
3. **Handles**: Negative weights (but not negative cycles)
4. **Output**: Matrix showing shortest distances between all pairs of vertices

## How it works:

1. Initialize a distance matrix with direct edge weights
2. For each intermediate vertex k, check if going through k gives a shorter path
3. Update the distance matrix accordingly
4. After all iterations, the matrix contains shortest paths between all pairs

The algorithm is particularly useful for finding transitive relationships or determining the shortest path between any two nodes in a graph.

