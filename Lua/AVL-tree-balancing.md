# AVL Tree Balancing Algorithm in Lua

Here's a complete implementation of an AVL tree with balancing operations in Lua:

```lua
-- AVL Tree Node Structure
local AVLNode = {}
AVLNode.__index = AVLNode

function AVLNode:new(value)
    local node = {
        value = value,
        left = nil,
        right = nil,
        height = 1
    }
    setmetatable(node, AVLNode)
    return node
end

-- AVL Tree Class
local AVLTree = {}
AVLTree.__index = AVLTree

function AVLTree:new()
    local tree = {
        root = nil
    }
    setmetatable(tree, AVLTree)
    return tree
end

-- Get height of a node
function AVLTree:getHeight(node)
    if not node then return 0 end
    return node.height
end

-- Get balance factor of a node
function AVLTree:getBalance(node)
    if not node then return 0 end
    return self:getHeight(node.left) - self:getHeight(node.right)
end

-- Update height of a node
function AVLTree:updateHeight(node)
    if node then
        node.height = 1 + math.max(self:getHeight(node.left), self:getHeight(node.right))
    end
end

-- Right rotation
function AVLTree:rotateRight(y)
    local x = y.left
    local T2 = x.right
    
    -- Perform rotation
    x.right = y
    y.left = T2
    
    -- Update heights
    self:updateHeight(y)
    self:updateHeight(x)
    
    return x
end

-- Left rotation
function AVLTree:rotateLeft(x)
    local y = x.right
    local T2 = y.left
    
    -- Perform rotation
    y.left = x
    x.right = T2
    
    -- Update heights
    self:updateHeight(x)
    self:updateHeight(y)
    
    return y
end

-- Insert a value into the AVL tree
function AVLTree:insert(value)
    self.root = self:insertNode(self.root, value)
end

function AVLTree:insertNode(node, value)
    -- Step 1: Perform normal BST insertion
    if not node then
        return AVLNode:new(value)
    end
    
    if value < node.value then
        node.left = self:insertNode(node.left, value)
    elseif value > node.value then
        node.right = self:insertNode(node.right, value)
    else
        -- Duplicate values not allowed
        return node
    end
    
    -- Step 2: Update height of current node
    self:updateHeight(node)
    
    -- Step 3: Get balance factor
    local balance = self:getBalance(node)
    
    -- Step 4: Perform rotations if needed
    
    -- Left Left Case
    if balance > 1 and value < node.left.value then
        return self:rotateRight(node)
    end
    
    -- Right Right Case
    if balance < -1 and value > node.right.value then
        return self:rotateLeft(node)
    end
    
    -- Left Right Case
    if balance > 1 and value > node.left.value then
        node.left = self:rotateLeft(node.left)
        return self:rotateRight(node)
    end
    
    -- Right Left Case
    if balance < -1 and value < node.right.value then
        node.right = self:rotateRight(node.right)
        return self:rotateLeft(node)
    end
    
    -- Return the (unchanged) node pointer
    return node
end

-- In-order traversal to display tree
function AVLTree:inorderTraversal(node, result)
    if not node then return end
    
    self:inorderTraversal(node.left, result)
    table.insert(result, node.value)
    self:inorderTraversal(node.right, result)
end

-- Display tree in-order
function AVLTree:display()
    local result = {}
    self:inorderTraversal(self.root, result)
    return result
end

-- Print tree structure (for visualization)
function AVLTree:printTree(node, prefix, isLast)
    if not node then return end
    
    print(prefix .. (isLast and "└── " or "├── ") .. node.value .. 
          " (h:" .. node.height .. " b:" .. self:getBalance(node) .. ")")
    
    local children = {}
    if node.left then table.insert(children, {node = node.left, isLast = false}) end
    if node.right then table.insert(children, {node = node.right, isLast = true}) end
    
    for i, child in ipairs(children) do
        local newPrefix = prefix .. (isLast and "    " or "│   ")
        self:printTree(child.node, newPrefix, child.isLast)
    end
end

-- Example usage
print("=== AVL Tree Balancing Example ===")

local avl = AVLTree:new()

-- Insert values that will require balancing
local values = {10, 20, 30, 40, 50, 25}

print("Inserting values: " .. table.concat(values, ", "))
print()

for _, value in ipairs(values) do
    avl:insert(value)
    print("After inserting " .. value .. ":")
    avl:printTree(avl.root, "", true)
    print()
end

print("Final in-order traversal: " .. table.concat(avl:display(), ", "))
```

## Key Features of This Implementation:

### 1. **Node Structure**
- Each node contains value, left/right pointers, and height
- Uses Lua metatables for object-oriented programming

### 2. **Balance Operations**
- **Height calculation**: `getHeight()` function
- **Balance factor**: `getBalance()` calculates difference between left and right subtree heights
- **Rotation functions**: 
  - `rotateRight()` for right rotation
  - `rotateLeft()` for left rotation

### 3. **Balancing Cases**
The algorithm handles all four cases of imbalance:
- **Left-Left (LL)**: Right rotation
- **Right-Right (RR)**: Left rotation  
- **Left-Right (LR)**: Left rotation on left child, then right rotation
- **Right-Left (RL)**: Right rotation on right child, then left rotation

### 4. **Insertion Process**
1. Normal BST insertion
2. Height update
3. Balance factor calculation
4. Appropriate rotation based on balance factor

### 5. **Visualization**
- `printTree()` function shows tree structure with heights and balance factors
- `display()` function shows in-order traversal

This implementation automatically maintains the AVL tree property (height difference ≤ 1) after each insertion, ensuring O(log n) search, insert, and delete operations.

