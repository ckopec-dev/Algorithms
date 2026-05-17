# Union-Find Algorithm in Lua

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in Lua:

```lua
-- Union-Find implementation in Lua
local UnionFind = {}

function UnionFind:new(n)
    local obj = {
        parent = {},
        rank = {},
        count = n
    }
    
    -- Initialize each element to be its own parent
    for i = 1, n do
        obj.parent[i] = i
        obj.rank[i] = 0
    end
    
    return setmetatable(obj, {__index = UnionFind})
end

-- Find with path compression
function UnionFind:find(x)
    if self.parent[x] ~= x then
        self.parent[x] = self:find(self.parent[x])  -- Path compression
    end
    return self.parent[x]
end

-- Union with union by rank
function UnionFind:union(x, y)
    local rootX = self:find(x)
    local rootY = self:find(y)
    
    if rootX ~= rootY then
        -- Union by rank
        if self.rank[rootX] < self.rank[rootY] then
            self.parent[rootX] = rootY
        elseif self.rank[rootX] > self.rank[rootY] then
            self.parent[rootY] = rootX
        else
            self.parent[rootY] = rootX
            self.rank[rootX] = self.rank[rootX] + 1
        end
        self.count = self.count - 1
        return true
    end
    return false
end

-- Check if two elements are in the same set
function UnionFind:connected(x, y)
    return self:find(x) == self:find(y)
end

-- Get the number of disjoint sets
function UnionFind:getCount()
    return self.count
end

-- Example usage
print("Union-Find Algorithm Example")
print("============================")

-- Create a Union-Find structure with 6 elements (1-6)
local uf = UnionFind:new(6)

print("Initial state - 6 disjoint sets")
print("Connected components:", uf:getCount())

-- Perform some union operations
print("\nPerforming union operations:")
uf:union(1, 2)
print("Union(1, 2):", uf:getCount(), "components")

uf:union(3, 4)
print("Union(3, 4):", uf:getCount(), "components")

uf:union(2, 4)
print("Union(2, 4):", uf:getCount(), "components")

-- Check connections
print("\nChecking connections:")
print("1 and 3 connected?", uf:connected(1, 3))  -- Should be true
print("1 and 5 connected?", uf:connected(1, 5))  -- Should be false

-- Final state
print("\nFinal state:")
print("Connected components:", uf:getCount())
print("Root of 1:", uf:find(1))
print("Root of 3:", uf:find(3))
print("Root of 5:", uf:find(5))
```

## Output:
```
Union-Find Algorithm Example
============================
Initial state - 6 disjoint sets
Connected components: 6

Performing union operations:
Union(1, 2): 5 components
Union(3, 4): 4 components
Union(2, 4): 3 components

Checking connections:
1 and 3 connected? true
1 and 5 connected? false

Final state:
Connected components: 3
Root of 1: 1
Root of 3: 1
Root of 5: 5
```

## Key Features:

1. **Path Compression**: The `find` operation compresses the path, making future operations faster
2. **Union by Rank**: The `union` operation always attaches the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `find` and `union` operations have nearly constant time complexity
4. **Count Tracking**: Keeps track of the number of disjoint sets

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

This implementation is commonly used for:
- Detecting cycles in graphs
- Finding connected components
- Kruskal's algorithm for minimum spanning tree
- Image processing applications

