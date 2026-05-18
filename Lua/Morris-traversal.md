# Morris Traversal Algorithm in Lua

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's right pointers to create temporary links for traversal.

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

-- Morris Inorder Traversal
function morrisTraversal(root)
    local current = root
    local result = {}
    
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

-- Morris Preorder Traversal
function morrisPreorderTraversal(root)
    local current = root
    local result = {}
    
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
                -- Process current node (preorder)
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

-- Test the algorithms
local root = createExampleTree()

print("Inorder traversal (Morris):")
local inorder_result = morrisTraversal(root)
for i, val in ipairs(inorder_result) do
    io.write(val .. " ")
end
print()

print("Preorder traversal (Morris):")
local preorder_result = morrisPreorderTraversal(root)
for i, val in ipairs(preorder_result) do
    io.write(val .. " ")
end
print()

-- Expected output:
-- Inorder traversal (Morris): 4 2 5 1 3 
-- Preorder traversal (Morris): 1 2 4 5 3
```

## How Morris Traversal Works:

1. **Inorder Traversal**:
   - If current node has no left child, visit it and move to right
   - If current node has left child, find its inorder predecessor
   - Create a temporary link from predecessor to current node
   - Move to left subtree
   - When returning, remove the temporary link and visit current node

2. **Preorder Traversal**:
   - Similar logic but visits the current node before moving to the left subtree
   - The processing happens at the time of creating the temporary link

## Key Features:

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - no extra space used
- **No recursion or stack needed**
- **Restores original tree structure** after traversal

The algorithm is particularly useful when you want to traverse a tree with minimal memory usage and cannot use recursion or additional data structures.

