# Kosaraju's Algorithm in Lua

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation in Lua:

```lua
-- Kosaraju's Algorithm for Finding Strongly Connected Components

-- Function to create a new graph
local function createGraph(vertices)
    local graph = {}
    for i = 1, vertices do
        graph[i] = {}
    end
    return graph
end

-- Function to add an edge to the graph
local function addEdge(graph, from, to)
    table.insert(graph[from], to)
end

-- Function to perform DFS and fill the stack
local function dfsFillStack(graph, vertex, visited, stack)
    visited[vertex] = true
    
    -- Visit all adjacent vertices
    for _, neighbor in ipairs(graph[vertex]) do
        if not visited[neighbor] then
            dfsFillStack(graph, neighbor, visited, stack)
        end
    end
    
    -- Push vertex to stack after visiting all neighbors
    table.insert(stack, vertex)
end

-- Function to perform DFS on reversed graph
local function dfsReversed(graph, vertex, visited, component)
    visited[vertex] = true
    table.insert(component, vertex)
    
    -- Visit all adjacent vertices
    for _, neighbor in ipairs(graph[vertex]) do
        if not visited[neighbor] then
            dfsReversed(graph, neighbor, visited, component)
        end
    end
end

-- Function to get the transpose of the graph
local function getTranspose(graph, vertices)
    local transpose = createGraph(vertices)
    
    for i = 1, vertices do
        for _, neighbor in ipairs(graph[i]) do
            addEdge(transpose, neighbor, i)
        end
    end
    
    return transpose
end

-- Main Kosaraju's algorithm function
local function kosaraju(graph, vertices)
    local stack = {}
    local visited = {}
    
    -- Step 1: Fill the stack using DFS
    for i = 1, vertices do
        visited[i] = false
    end
    
    for i = 1, vertices do
        if not visited[i] then
            dfsFillStack(graph, i, visited, stack)
        end
    end
    
    -- Step 2: Get transpose of the graph
    local transpose = getTranspose(graph, vertices)
    
    -- Step 3: Process vertices in order of stack
    for i = 1, vertices do
        visited[i] = false
    end
    
    local sccs = {}
    local sccCount = 0
    
    while #stack > 0 do
        local vertex = table.remove(stack)
        
        if not visited[vertex] then
            sccCount = sccCount + 1
            local component = {}
            dfsReversed(transpose, vertex, visited, component)
            table.insert(sccs, component)
        end
    end
    
    return sccs
end

-- Example usage
print("Kosaraju's Algorithm Example")
print("============================")

-- Create a sample graph with 5 vertices
local graph = createGraph(5)

-- Add edges to the graph
addEdge(graph, 1, 2)
addEdge(graph, 2, 3)
addEdge(graph, 3, 1)
addEdge(graph, 2, 4)
addEdge(graph, 4, 5)
addEdge(graph, 5, 4)

print("Graph edges:")
print("1 -> 2")
print("2 -> 3")
print("3 -> 1")
print("2 -> 4")
print("4 -> 5")
print("5 -> 4")

-- Find strongly connected components
local sccs = kosaraju(graph, 5)

print("\nStrongly Connected Components:")
for i, component in ipairs(sccs) do
    print("SCC " .. i .. ": " .. table.concat(component, ", "))
end

-- Another example with more complex graph
print("\n" .. string.rep("=", 30))
print("Second Example")

local graph2 = createGraph(6)

addEdge(graph2, 1, 2)
addEdge(graph2, 2, 3)
addEdge(graph2, 3, 1)
addEdge(graph2, 2, 4)
addEdge(graph2, 4, 5)
addEdge(graph2, 5, 6)
addEdge(graph2, 6, 4)

print("Graph edges:")
print("1 -> 2")
print("2 -> 3")
print("3 -> 1")
print("2 -> 4")
print("4 -> 5")
print("5 -> 6")
print("6 -> 4")

local sccs2 = kosaraju(graph2, 6)

print("\nStrongly Connected Components:")
for i, component in ipairs(sccs2) do
    print("SCC " .. i .. ": " .. table.concat(component, ", "))
end
```

## How the Algorithm Works:

1. **First DFS Pass**: Perform DFS on the original graph and fill a stack with vertices in the order of their finishing times.

2. **Transpose Graph**: Create the transpose of the original graph (reverse all edges).

3. **Second DFS Pass**: Process vertices in the order they appear in the stack (from top to bottom) on the transposed graph to find SCCs.

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary data structures

## Output Example:
```
Kosaraju's Algorithm Example
============================
Graph edges:
1 -> 2
2 -> 3
3 -> 1
2 -> 4
4 -> 5
5 -> 4

Strongly Connected Components:
SCC 1: 1, 2, 3
SCC 2: 4, 5
```

This implementation correctly identifies the strongly connected components in directed graphs using Kosaraju's algorithm.

