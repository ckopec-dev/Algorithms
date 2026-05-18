# Disjoint Set Union (Union-Find) Algorithm in Lua

```lua
-- Disjoint Set Union (Union-Find) implementation in Lua

-- Create a new disjoint set with n elements
local function make_set(n)
    local parent = {}
    local rank = {}
    
    -- Initialize each element as its own parent
    for i = 1, n do
        parent[i] = i
        rank[i] = 0
    end
    
    return {
        parent = parent,
        rank = rank,
        find = function(x)
            -- Path compression
            if parent[x] ~= x then
                parent[x] = make_set(n).find(parent[x])
            end
            return parent[x]
        end,
        union = function(x, y)
            local root_x = make_set(n).find(x)
            local root_y = make_set(n).find(y)
            
            if root_x ~= root_y then
                -- Union by rank
                if rank[root_x] < rank[root_y] then
                    parent[root_x] = root_y
                elseif rank[root_x] > rank[root_y] then
                    parent[root_y] = root_x
                else
                    parent[root_y] = root_x
                    rank[root_x] = rank[root_x] + 1
                end
                return true  -- Union performed
            end
            return false  -- Already in same set
        end
    }
end

-- Alternative cleaner implementation
local function create_union_find(n)
    local parent = {}
    local rank = {}
    
    -- Initialize
    for i = 1, n do
        parent[i] = i
        rank[i] = 0
    end
    
    -- Find with path compression
    local function find(x)
        if parent[x] ~= x then
            parent[x] = find(parent[x])  -- Path compression
        end
        return parent[x]
    end
    
    -- Union by rank
    local function union(x, y)
        local root_x = find(x)
        local root_y = find(y)
        
        if root_x ~= root_y then
            if rank[root_x] < rank[root_y] then
                parent[root_x] = root_y
            elseif rank[root_x] > rank[root_y] then
                parent[root_y] = root_x
            else
                parent[root_y] = root_x
                rank[root_x] = rank[root_x] + 1
            end
            return true
        end
        return false
    end
    
    return {
        find = find,
        union = union
    }
end

-- Example usage
print("=== Disjoint Set Union Example ===")

-- Create a union-find structure with 6 elements (1-6)
local uf = create_union_find(6)

-- Perform some unions
print("Initial state:")
for i = 1, 6 do
    print("Element " .. i .. " belongs to set: " .. uf.find(i))
end

print("\nPerforming unions:")
uf.union(1, 2)
print("Union(1, 2): Elements 1 and 2 are now in same set")

uf.union(3, 4)
print("Union(3, 4): Elements 3 and 4 are now in same set")

uf.union(2, 4)
print("Union(2, 4): Elements 2, 3, 4 are now in same set")

uf.union(5, 6)
print("Union(5, 6): Elements 5 and 6 are now in same set")

print("\nFinal state:")
for i = 1, 6 do
    print("Element " .. i .. " belongs to set: " .. uf.find(i))
end

print("\nChecking if elements are in same set:")
print("Are 1 and 3 in same set? " .. tostring(uf.find(1) == uf.find(3)))  -- true
print("Are 1 and 5 in same set? " .. tostring(uf.find(1) == uf.find(5)))  -- false
print("Are 5 and 6 in same set? " .. tostring(uf.find(5) == uf.find(6)))  -- true
```

## Output:
```
=== Disjoint Set Union Example ===
Initial state:
Element 1 belongs to set: 1
Element 2 belongs to set: 2
Element 3 belongs to set: 3
Element 4 belongs to set: 4
Element 5 belongs to set: 5
Element 6 belongs to set: 6

Performing unions:
Union(1, 2): Elements 1 and 2 are now in same set
Union(3, 4): Elements 3 and 4 are now in same set
Union(2, 4): Elements 2, 3, 4 are now in same set
Union(5, 6): Elements 5 and 6 are now in same set

Final state:
Element 1 belongs to set: 1
Element 2 belongs to set: 1
Element 3 belongs to set: 1
Element 4 belongs to set: 1
Element 5 belongs to set: 5
Element 6 belongs to set: 5

Checking if elements are in same set:
Are 1 and 3 in same set? true
Are 1 and 5 in same set? false
Are 5 and 6 in same set? true
```

## Key Features:

1. **Path Compression**: When finding the root of an element, we make all nodes along the path point directly to the root
2. **Union by Rank**: Always attach the smaller tree under the root of the larger tree
3. **Time Complexity**: Nearly constant time for both operations (amortized)
4. **Space Complexity**: O(n) where n is the number of elements

This implementation is commonly used in algorithms like Kruskal's Minimum Spanning Tree and connectivity problems.

