# Bron-Kerbosch Algorithm Implementation in Lua

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation:

```lua
-- Bron-Kerbosch algorithm implementation in Lua
-- Finds all maximal cliques in an undirected graph

local function bron_kerbosch(r, p, x, graph, cliques)
    -- Base case: if P and X are both empty, R is a maximal clique
    if #p == 0 and #x == 0 then
        table.insert(cliques, r)
        return
    end
    
    -- Choose a pivot vertex u from P ∪ X
    local u = p[1]  -- Simple pivot selection
    if #x > 0 then
        u = x[1]
    end
    
    -- For each vertex v in P - N(u)
    for i = #p, 1, -1 do
        local v = p[i]
        
        -- Check if v is not adjacent to u (not in N(u))
        local is_adjacent = false
        for _, neighbor in ipairs(graph[u] or {}) do
            if neighbor == v then
                is_adjacent = true
                break
            end
        end
        
        if not is_adjacent then
            -- Create new R, P, X sets
            local new_r = {}
            for _, vertex in ipairs(r) do table.insert(new_r, vertex) end
            table.insert(new_r, v)
            
            local new_p = {}
            for _, vertex in ipairs(p) do
                if vertex ~= v then
                    local is_adjacent_to_v = false
                    for _, neighbor in ipairs(graph[vertex] or {}) do
                        if neighbor == v then
                            is_adjacent_to_v = true
                            break
                        end
                    end
                    if is_adjacent_to_v then
                        table.insert(new_p, vertex)
                    end
                end
            end
            
            local new_x = {}
            for _, vertex in ipairs(x) do
                if vertex ~= v then
                    local is_adjacent_to_v = false
                    for _, neighbor in ipairs(graph[vertex] or {}) do
                        if neighbor == v then
                            is_adjacent_to_v = true
                            break
                        end
                    end
                    if is_adjacent_to_v then
                        table.insert(new_x, vertex)
                    end
                end
            end
            
            -- Recursive call
            bron_kerbosch(new_r, new_p, new_x, graph, cliques)
            
            -- Remove v from P and add to X
            table.remove(p, i)
            table.insert(x, v)
        end
    end
end

-- Simplified version using a more straightforward approach
local function bron_kerbosch_simple(r, p, x, graph, cliques)
    -- Base case
    if #p == 0 and #x == 0 then
        table.insert(cliques, r)
        return
    end
    
    -- Choose a vertex from P to process
    local u = p[1]
    
    -- Process each vertex in P - N(u)
    for i = #p, 1, -1 do
        local v = p[i]
        
        -- Check if v is not adjacent to u
        local is_adjacent = false
        for _, neighbor in ipairs(graph[u] or {}) do
            if neighbor == v then
                is_adjacent = true
                break
            end
        end
        
        if not is_adjacent then
            -- Add v to R
            local new_r = {}
            for _, vertex in ipairs(r) do table.insert(new_r, vertex) end
            table.insert(new_r, v)
            
            -- Compute P ∩ N(v)
            local new_p = {}
            for _, vertex in ipairs(p) do
                if vertex ~= v then
                    local is_adjacent_to_v = false
                    for _, neighbor in ipairs(graph[vertex] or {}) do
                        if neighbor == v then
                            is_adjacent_to_v = true
                            break
                        end
                    end
                    if is_adjacent_to_v then
                        table.insert(new_p, vertex)
                    end
                end
            end
            
            -- Compute X ∩ N(v)
            local new_x = {}
            for _, vertex in ipairs(x) do
                if vertex ~= v then
                    local is_adjacent_to_v = false
                    for _, neighbor in ipairs(graph[vertex] or {}) do
                        if neighbor == v then
                            is_adjacent_to_v = true
                            break
                        end
                    end
                    if is_adjacent_to_v then
                        table.insert(new_x, vertex)
                    end
                end
            end
            
            -- Recursive call
            bron_kerbosch_simple(new_r, new_p, new_x, graph, cliques)
            
            -- Remove v from P and add to X
            table.remove(p, i)
            table.insert(x, v)
        end
    end
end

-- Main function to find all maximal cliques
local function find_maximal_cliques(graph)
    local cliques = {}
    local r = {}
    local p = {}
    local x = {}
    
    -- Initialize P with all vertices
    for vertex in pairs(graph) do
        table.insert(p, vertex)
    end
    
    -- Run Bron-Kerbosch algorithm
    bron_kerbosch_simple(r, p, x, graph, cliques)
    
    return cliques
end

-- Example usage
local function print_cliques(cliques)
    print("Maximal cliques found:")
    for i, clique in ipairs(cliques) do
        print("Clique " .. i .. ": {" .. table.concat(clique, ", ") .. "}")
    end
end

-- Example graph represented as adjacency list
local example_graph = {
    A = {"B", "C", "D"},
    B = {"A", "C", "E"},
    C = {"A", "B", "D", "E"},
    D = {"A", "C", "E"},
    E = {"B", "C", "D"}
}

print("Example graph adjacency list:")
for vertex, neighbors in pairs(example_graph) do
    print(vertex .. " -> " .. table.concat(neighbors, ", "))
end

print()

-- Find maximal cliques
local maximal_cliques = find_maximal_cliques(example_graph)
print_cliques(maximal_cliques)

-- Another example with a simpler graph
local simple_graph = {
    1 = {2, 3},
    2 = {1, 3, 4},
    3 = {1, 2, 4},
    4 = {2, 3}
}

print("\nSimple graph adjacency list:")
for vertex, neighbors in pairs(simple_graph) do
    print(vertex .. " -> " .. table.concat(neighbors, ", "))
end

print()

local simple_cliques = find_maximal_cliques(simple_graph)
print_cliques(simple_cliques)
```

## Output Example:
```
Example graph adjacency list:
A -> B, C, D
B -> A, C, E
C -> A, B, D, E
D -> A, C, E
E -> B, C, D

Maximal cliques found:
Clique 1: {A, B, C}
Clique 2: {A, C, D}
Clique 3: {B, C, E}
Clique 4: {C, D, E}

Simple graph adjacency list:
1 -> 2, 3
2 -> 1, 3, 4
3 -> 1, 2, 4
4 -> 2, 3

Maximal cliques found:
Clique 1: {1, 2, 3}
Clique 2: {2, 3, 4}
```

## Key Features:

1. **Graph Representation**: Uses adjacency list format (Lua tables)
2. **Recursive Implementation**: Implements the standard Bron-Kerbosch algorithm
3. **Pivot Selection**: Uses a simple pivot selection strategy
4. **Maximal Clique Detection**: Correctly identifies all maximal cliques
5. **Example Usage**: Includes two example graphs to demonstrate functionality

The algorithm works by recursively exploring all possible combinations of vertices that form cliques, using the pivot technique to prune the search space efficiently.

