# Morris Traversal Algorithm in Lua

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses threading to traverse the tree in-order.

## Implementation

```lua
-- Definition of a binary tree node
local TreeNode = {}
TreeNode.__index = TreeNode

function TreeNode:new(val)
    local node = {
        val = val,
        left = nil,
        right = nil
    }
    return setmetatable(node, TreeNode)
end

-- Morris In-order Traversal
function morrisTraversal(root)
    local result = {}
    local current = root
    
    while current ~= nil do
        if current.left == nil then
            -- Process current node
            table.insert(result, current.val)
            current = current.right
        else
            -- Find the inorder predecessor
            local predecessor = current.left
            while predecessor.right ~= nil and predecessor.right ~= current do
                predecessor = predecessor.right
            end
            
            if predecessor.right == nil then
                -- Make current as right child of predecessor
                predecessor.right = current
                current = current.left
            else
                -- Revert the changes (restore tree structure)
                predecessor.right = nil
                -- Process current node
                table.insert(result, current.val)
                current = current.right
            end
        end
    end
    
    return result
end

-- Morris Pre-order Traversal
function morrisPreorderTraversal(root)
    local result = {}
    local current = root
    
    while current ~= nil do
        if current.left == nil then
            -- Process current node
            table.insert(result, current.val)
            current = current.right
        else
            -- Find the inorder predecessor
            local predecessor = current.left
            while predecessor.right ~= nil and predecessor.right ~= current do
                predecessor = predecessor.right
            end
            
            if predecessor.right == nil then
                -- Process current node (pre-order)
                table.insert(result, current.val)
                -- Make current as right child of predecessor
                predecessor.right = current
                current = current.left
            else
                -- Revert the changes (restore tree structure)
                predecessor.right = nil
                current = current.right
            end
        end
    end
    
    return result
end

-- Example usage
local function createExampleTree()
    -- Create tree:
    --       1
    --      / \
    --     2   3
    --    / \
    --   4   5
    
    local root = TreeNode:new(1)
    root.left = TreeNode:new(2)
    root.right = TreeNode:new(3)
    root.left.left = TreeNode:new(4)
    root.left.right = TreeNode:new(5)
    
    return root
end

-- Test the implementation
local root = createExampleTree()

print("In-order traversal using Morris traversal:")
local inorder = morrisTraversal(root)
for i, val in ipairs(inorder) do
    io.write(val .. " ")
end
print()

print("Pre-order traversal using Morris traversal:")
local preorder = morrisPreorderTraversal(root)
for i, val in ipairs(preorder) do
    io.write(val .. " ")
end
print()
```

## Output
```
In-order traversal using Morris traversal:
4 2 5 1 3 

Pre-order traversal using Morris traversal:
1 2 4 5 3 
```

## How it works:

1. **In-order traversal**: 
   - If current node has no left child, process it and move to right
   - If it has a left child, find the inorder predecessor and create a thread to current node
   - When thread is encountered, process current node and restore tree structure

2. **Pre-order traversal**:
   - Process current node before moving to left subtree
   - Uses same threading technique but processes nodes earlier

## Time and Space Complexity:
- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - no additional space except for variables

The Morris traversal is particularly useful when you want to traverse a tree without using recursion or explicit stack, making it memory-efficient for very deep trees.

