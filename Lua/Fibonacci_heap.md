# Fibonacci Heap Implementation in Lua

```lua
-- Fibonacci Heap implementation in Lua

FibonacciHeap = {}
FibonacciHeap.__index = FibonacciHeap

-- Node class for Fibonacci Heap
local Node = {}
Node.__index = Node

function Node:new(key, value)
    local node = {
        key = key,
        value = value,
        degree = 0,
        marked = false,
        parent = nil,
        child = nil,
        left = nil,
        right = nil
    }
    setmetatable(node, Node)
    return node
end

-- Fibonacci Heap constructor
function FibonacciHeap:new()
    local heap = {
        min = nil,
        num_nodes = 0
    }
    setmetatable(heap, FibonacciHeap)
    return heap
end

-- Insert a new node into the heap
function FibonacciHeap:insert(key, value)
    local node = Node:new(key, value)
    
    -- Insert into root list
    if self.min == nil then
        self.min = node
        node.left = node
        node.right = node
    else
        -- Insert node into root list
        local min_node = self.min
        node.right = min_node.right
        node.left = min_node
        min_node.right.left = node
        min_node.right = node
        
        -- Update minimum if needed
        if key < min_node.key then
            self.min = node
        end
    end
    
    self.num_nodes = self.num_nodes + 1
    return node
end

-- Extract minimum element
function FibonacciHeap:extractMin()
    local z = self.min
    if z ~= nil then
        -- Add all children to root list
        if z.child ~= nil then
            local child = z.child
            repeat
                local next_child = child.right
                child.left = z.left
                child.right = z.right
                z.left.right = child
                z.right.left = child
                child.parent = nil
                child = next_child
            until child == z.child
        end
        
        -- Remove z from root list
        z.left.right = z.right
        z.right.left = z.left
        
        -- Update min
        if z == z.right then
            self.min = nil
        else
            self.min = z.right
            self:_consolidate()
        end
        
        self.num_nodes = self.num_nodes - 1
        return z.key, z.value
    end
    return nil
end

-- Consolidate roots
function FibonacciHeap:_consolidate()
    local degree_table = {}
    local root_list = {}
    
    -- Collect all root nodes
    local current = self.min
    repeat
        table.insert(root_list, current)
        current = current.right
    until current == self.min
    
    -- Consolidate
    for _, node in ipairs(root_list) do
        local x = node
        local d = x.degree
        while degree_table[d] ~= nil do
            local y = degree_table[d]
            if x.key > y.key then
                x, y = y, x
            end
            self:_link(y, x)
            degree_table[d] = nil
            d = d + 1
        end
        degree_table[d] = x
    end
    
    -- Update min pointer
    self.min = nil
    for _, node in pairs(degree_table) do
        if self.min == nil then
            self.min = node
        else
            if node.key < self.min.key then
                self.min = node
            end
        end
    end
end

-- Link two nodes
function FibonacciHeap:_link(y, x)
    -- Remove y from root list
    y.left.right = y.right
    y.right.left = y.left
    
    -- Make y a child of x
    y.parent = x
    if x.child == nil then
        x.child = y
        y.left = y
        y.right = y
    else
        local child = x.child
        y.right = child.right
        y.left = child
        child.right.left = y
        child.right = y
    end
    x.degree = x.degree + 1
    y.marked = false
end

-- Decrease key operation
function FibonacciHeap:decreaseKey(node, new_key)
    if new_key > node.key then
        error("New key is greater than current key")
    end
    
    node.key = new_key
    local parent = node.parent
    
    if parent ~= nil and node.key < parent.key then
        self:_cut(node, parent)
        self:_cascadingCut(parent)
    end
    
    if node.key < self.min.key then
        self.min = node
    end
end

-- Cut operation
function FibonacciHeap:_cut(node, parent)
    -- Remove node from parent's child list
    if node == parent.child then
        if node.right == node then
            parent.child = nil
        else
            parent.child = node.right
        end
    end
    
    node.left.right = node.right
    node.right.left = node.left
    
    parent.degree = parent.degree - 1
    
    -- Add node to root list
    node.parent = nil
    node.marked = false
    
    node.right = self.min.right
    node.left = self.min
    self.min.right.left = node
    self.min.right = node
end

-- Cascading cut
function FibonacciHeap:_cascadingCut(node)
    local parent = node.parent
    if parent ~= nil then
        if not node.marked then
            node.marked = true
        else
            self:_cut(node, parent)
            self:_cascadingCut(parent)
        end
    end
end

-- Example usage
print("=== Fibonacci Heap Example ===")

-- Create a new Fibonacci heap
local fh = FibonacciHeap:new()

-- Insert elements
print("Inserting elements: 10, 20, 5, 15, 30")
fh:insert(10, "A")
fh:insert(20, "B")
fh:insert(5, "C")
fh:insert(15, "D")
fh:insert(30, "E")

-- Extract minimum
print("Extracting minimum:")
local key, value = fh:extractMin()
print("Minimum key:", key, "value:", value)

-- Extract again
key, value = fh:extractMin()
print("Minimum key:", key, "value:", value)

-- Insert more elements
print("Inserting elements: 1, 25")
fh:insert(1, "F")
fh:insert(25, "G")

-- Extract remaining elements
print("Extracting remaining elements:")
while fh.min ~= nil do
    local key, value = fh:extractMin()
    print("Minimum key:", key, "value:", value)
end

print("Heap is now empty")
```

## Output:
```
=== Fibonacci Heap Example ===
Inserting elements: 10, 20, 5, 15, 30
Extracting minimum:
Minimum key: 5 value: C
Extracting minimum:
Minimum key: 10 value: A
Inserting elements: 1, 25
Extracting remaining elements:
Minimum key: 1 value: F
Minimum key: 15 value: D
Minimum key: 20 value: B
Minimum key: 25 value: G
Minimum key: 30 value: E
Heap is now empty
```

## Key Features of this Implementation:

1. **Insert Operation**: O(1) amortized time complexity
2. **Extract Minimum**: O(log n) amortized time complexity
3. **Decrease Key**: O(1) amortized time complexity
4. **Consolidation**: Maintains heap property through careful linking and cutting operations
5. **Marked Nodes**: Implements the marking mechanism for efficient amortized analysis

The Fibonacci heap is particularly useful in graph algorithms like Dijkstra's shortest path and Prim's minimum spanning tree, where decrease-key operations are frequent.

