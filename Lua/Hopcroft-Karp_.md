# Hopcroft-Karp Algorithm Implementation in Lua

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a complete implementation:

```lua
-- Hopcroft-Karp Algorithm for Maximum Matching in Bipartite Graph
local HopcroftKarp = {}

-- Create a new HopcroftKarp instance
function HopcroftKarp:new()
    local obj = {
        left_vertices = {},    -- Left side vertices
        right_vertices = {},   -- Right side vertices
        adj_list = {},         -- Adjacency list representation
        match_left = {},       -- Matching for left vertices
        match_right = {},      -- Matching for right vertices
        dist = {},             -- Distance array for BFS
        visited = {}           -- Visited array for BFS
    }
    setmetatable(obj, {__index = self})
    return obj
end

-- Add a vertex to the left side
function HopcroftKarp:addLeftVertex(vertex)
    table.insert(self.left_vertices, vertex)
    self.match_left[vertex] = nil
end

-- Add a vertex to the right side
function HopcroftKarp:addRightVertex(vertex)
    table.insert(self.right_vertices, vertex)
    self.match_right[vertex] = nil
end

-- Add an edge between left and right vertices
function HopcroftKarp:addEdge(left_vertex, right_vertex)
    if not self.adj_list[left_vertex] then
        self.adj_list[left_vertex] = {}
    end
    table.insert(self.adj_list[left_vertex], right_vertex)
end

-- Perform BFS to find augmenting paths
function HopcroftKarp:bfs()
    local queue = {}
    local queue_start = 1
    local queue_end = 0
    
    -- Initialize distances
    for _, u in ipairs(self.left_vertices) do
        if not self.match_left[u] then
            self.dist[u] = 0
            queue_end = queue_end + 1
            queue[queue_end] = u
        else
            self.dist[u] = math.huge
        end
    end
    
    -- Set infinite distance for unmatched right vertices
    for _, v in ipairs(self.right_vertices) do
        self.dist[v] = math.huge
    end
    
    local is_found = false
    
    while queue_start <= queue_end do
        local u = queue[queue_start]
        queue_start = queue_start + 1
        
        if self.dist[u] < self.dist[math.huge] then
            for _, v in ipairs(self.adj_list[u] or {}) do
                if self.dist[self.match_right[v]] == math.huge then
                    self.dist[self.match_right[v]] = self.dist[u] + 1
                    if not self.match_right[v] then
                        is_found = true
                    else
                        queue_end = queue_end + 1
                        queue[queue_end] = self.match_right[v]
                    end
                end
            end
        end
    end
    
    return is_found
end

-- Perform DFS to find augmenting path
function HopcroftKarp:dfs(u)
    if u then
        for _, v in ipairs(self.adj_list[u] or {}) do
            if self.dist[self.match_right[v]] == self.dist[u] + 1 then
                if self:dfs(self.match_right[v]) then
                    self.match_right[v] = u
                    self.match_left[u] = v
                    return true
                end
            end
        end
        self.dist[u] = math.huge
        return false
    end
    return true
end

-- Find maximum matching using Hopcroft-Karp algorithm
function HopcroftKarp:findMaximumMatching()
    local matching = 0
    
    -- Initialize matching
    for _, v in ipairs(self.right_vertices) do
        self.match_right[v] = nil
    end
    
    for _, u in ipairs(self.left_vertices) do
        self.match_left[u] = nil
    end
    
    -- Continue until no augmenting path found
    while self:bfs() do
        for _, u in ipairs(self.left_vertices) do
            if not self.match_left[u] then
                if self:dfs(u) then
                    matching = matching + 1
                end
            end
        end
    end
    
    return matching
end

-- Get the actual matching pairs
function HopcroftKarp:getMatching()
    local result = {}
    for u, v in pairs(self.match_left) do
        if v then
            table.insert(result, {left = u, right = v})
        end
    end
    return result
end

-- Example usage
print("=== Hopcroft-Karp Algorithm Example ===")

-- Create a new instance
local hk = HopcroftKarp:new()

-- Add vertices (left side: 1,2,3,4; right side: 5,6,7,8)
hk:addLeftVertex(1)
hk:addLeftVertex(2)
hk:addLeftVertex(3)
hk:addLeftVertex(4)

hk:addRightVertex(5)
hk:addRightVertex(6)
hk:addRightVertex(7)
hk:addRightVertex(8)

-- Add edges (bipartite graph)
hk:addEdge(1, 5)
hk:addEdge(1, 6)
hk:addEdge(2, 5)
hk:addEdge(2, 7)
hk:addEdge(3, 6)
hk:addEdge(3, 8)
hk:addEdge(4, 7)
hk:addEdge(4, 8)

-- Find maximum matching
local max_matching = hk:findMaximumMatching()
print("Maximum matching size: " .. max_matching)

-- Get actual matching pairs
local matching_pairs = hk:getMatching()
print("Matching pairs:")
for i, pair in ipairs(matching_pairs) do
    print("  " .. pair.left .. " -> " .. pair.right)
end

-- Another example with different graph
print("\n=== Another Example ===")

local hk2 = HopcroftKarp:new()

-- Add vertices
hk2:addLeftVertex("A")
hk2:addLeftVertex("B")
hk2:addLeftVertex("C")
hk2:addLeftVertex("D")

hk2:addRightVertex("X")
hk2:addRightVertex("Y")
hk2:addRightVertex("Z")

-- Add edges
hk2:addEdge("A", "X")
hk2:addEdge("A", "Y")
hk2:addEdge("B", "X")
hk2:addEdge("B", "Z")
hk2:addEdge("C", "Y")
hk2:addEdge("C", "Z")
hk2:addEdge("D", "X")

-- Find maximum matching
local max_matching2 = hk2:findMaximumMatching()
print("Maximum matching size: " .. max_matching2)

-- Get actual matching pairs
local matching_pairs2 = hk2:getMatching()
print("Matching pairs:")
for i, pair in ipairs(matching_pairs2) do
    print("  " .. pair.left .. " -> " .. pair.right)
end
```

## Key Features of this Implementation:

1. **Bipartite Graph Support**: Handles graphs with two distinct sets of vertices
2. **Efficient Algorithm**: Implements the full Hopcroft-Karp algorithm with O(E√V) time complexity
3. **Flexible Vertex Types**: Works with any type of vertex labels (numbers, strings, etc.)
4. **Complete Matching Information**: Returns both the size and actual matching pairs
5. **Modular Design**: Clean separation of graph representation and matching algorithm

## How it Works:

1. **Initialization**: Sets up adjacency list and matching arrays
2. **BFS Phase**: Finds augmenting paths using breadth-first search
3. **DFS Phase**: Uses depth-first search to find and augment paths
4. **Iteration**: Continues until no more augmenting paths exist

The algorithm efficiently finds the maximum cardinality matching in bipartite graphs, which is useful in various applications like job assignment, resource allocation, and network flow problems.

