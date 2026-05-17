# Binary Search Tree in Lua

Here's a complete implementation of a Binary Search Tree algorithm in Lua:

```lua
-- Binary Search Tree implementation in Lua

-- Node class
local Node = {}
Node.__index = Node

function Node:new(value)
    local node = {
        value = value,
        left = nil,
        right = nil
    }
    return setmetatable(node, Node)
end

-- Binary Search Tree class
local BST = {}
BST.__index = BST

function BST:new()
    local tree = {
        root = nil
    }
    return setmetatable(tree, BST)
end

-- Insert a value into the BST
function BST:insert(value)
    self.root = self:insertNode(self.root, value)
end

function BST:insertNode(node, value)
    if node == nil then
        return Node:new(value)
    end
    
    if value < node.value then
        node.left = self:insertNode(node.left, value)
    else
        node.right = self:insertNode(node.right, value)
    end
    
    return node
end

-- Search for a value in the BST
function BST:search(value)
    return self:searchNode(self.root, value)
end

function BST:searchNode(node, value)
    if node == nil then
        return false
    end
    
    if value == node.value then
        return true
    end
    
    if value < node.value then
        return self:searchNode(node.left, value)
    else
        return self:searchNode(node.right, value)
    end
end

-- In-order traversal (returns sorted array)
function BST:inOrderTraversal()
    local result = {}
    self:inOrderNode(self.root, result)
    return result
end

function BST:inOrderNode(node, result)
    if node ~= nil then
        self:inOrderNode(node.left, result)
        table.insert(result, node.value)
        self:inOrderNode(node.right, result)
    end
end

-- Pre-order traversal
function BST:preOrderTraversal()
    local result = {}
    self:preOrderNode(self.root, result)
    return result
end

function BST:preOrderNode(node, result)
    if node ~= nil then
        table.insert(result, node.value)
        self:preOrderNode(node.left, result)
        self:preOrderNode(node.right, result)
    end
end

-- Post-order traversal
function BST:postOrderTraversal()
    local result = {}
    self:postOrderNode(self.root, result)
    return result
end

function BST:postOrderNode(node, result)
    if node ~= nil then
        self:postOrderNode(node.left, result)
        self:postOrderNode(node.right, result)
        table.insert(result, node.value)
    end
end

-- Find minimum value
function BST:minValue()
    if self.root == nil then
        return nil
    end
    return self:minValueNode(self.root).value
end

function BST:minValueNode(node)
    while node.left ~= nil do
        node = node.left
    end
    return node
end

-- Find maximum value
function BST:maxValue()
    if self.root == nil then
        return nil
    end
    return self:maxValueNode(self.root).value
end

function BST:maxValueNode(node)
    while node.right ~= nil do
        node = node.right
    end
    return node
end

-- Example usage
local bst = BST:new()

-- Insert values
bst:insert(50)
bst:insert(30)
bst:insert(70)
bst:insert(20)
bst:insert(40)
bst:insert(60)
bst:insert(80)

print("In-order traversal (sorted):")
local inorder = bst:inOrderTraversal()
for i, v in ipairs(inorder) do
    print(v)
end

print("\nPre-order traversal:")
local preorder = bst:preOrderTraversal()
for i, v in ipairs(preorder) do
    print(v)
end

print("\nPost-order traversal:")
local postorder = bst:postOrderTraversal()
for i, v in ipairs(postorder) do
    print(v)
end

print("\nSearch results:")
print("Search 40:", bst:search(40))  -- true
print("Search 25:", bst:search(25))  -- false

print("\nMin value:", bst:minValue())  -- 20
print("Max value:", bst:maxValue())    -- 80
```

## Output:
```
In-order traversal (sorted):
20
30
40
50
60
70
80

Pre-order traversal:
50
30
20
40
70
60
80

Post-order traversal:
20
40
30
60
80
70
50

Search results:
Search 40: true
Search 25: false

Min value: 20
Max value: 80
```

## Key Features:

1. **Node Structure**: Each node contains a value and references to left and right children
2. **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
3. **Search Operation**: Efficient O(log n) search using BST property
4. **Traversals**: 
   - In-order (sorted output)
   - Pre-order (root-first)
   - Post-order (leaf-first)
5. **Utility Functions**: Find min/max values
6. **Recursive Implementation**: Clean and readable recursive approach

The time complexity for search, insert, and delete operations is O(log n) on average for a balanced tree, and O(n) in the worst case for a skewed tree.

