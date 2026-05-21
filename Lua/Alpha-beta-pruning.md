# Alpha-Beta Pruning Algorithm in Lua

Here's a complete implementation of the Alpha-Beta pruning algorithm in Lua:

```lua
-- Alpha-Beta Pruning Implementation in Lua

-- Game tree node structure
local Node = {}
Node.__index = Node

function Node:new(value, isMaximizing)
    local obj = {
        value = value,
        isMaximizing = isMaximizing,
        children = {},
        alpha = -math.huge,
        beta = math.huge
    }
    setmetatable(obj, Node)
    return obj
end

-- Add child node
function Node:addChild(child)
    table.insert(self.children, child)
end

-- Alpha-Beta pruning function
function alpha_beta(node, depth, alpha, beta, maximizingPlayer)
    -- Base case: leaf node or maximum depth reached
    if depth == 0 or #node.children == 0 then
        return node.value
    end
    
    if maximizingPlayer then
        local maxEval = -math.huge
        for _, child in ipairs(node.children) do
            local eval = alpha_beta(child, depth - 1, alpha, beta, false)
            maxEval = math.max(maxEval, eval)
            alpha = math.max(alpha, eval)
            
            -- Alpha-beta pruning
            if beta <= alpha then
                break -- Beta cut-off
            end
        end
        return maxEval
    else
        local minEval = math.huge
        for _, child in ipairs(node.children) do
            local eval = alpha_beta(child, depth - 1, alpha, beta, true)
            minEval = math.min(minEval, eval)
            beta = math.min(beta, eval)
            
            -- Alpha-beta pruning
            if beta <= alpha then
                break -- Alpha cut-off
            end
        end
        return minEval
    end
end

-- Example usage with a sample game tree
function create_sample_tree()
    -- Create root node (maximizing player)
    local root = Node:new(nil, true)
    
    -- Level 1 nodes
    local node1 = Node:new(3, false)
    local node2 = Node:new(5, false)
    local node3 = Node:new(2, false)
    
    root:addChild(node1)
    root:addChild(node2)
    root:addChild(node3)
    
    -- Level 2 nodes for node1
    local node1_1 = Node:new(9, true)
    local node1_2 = Node:new(2, true)
    local node1_3 = Node:new(7, true)
    
    node1:addChild(node1_1)
    node1:addChild(node1_2)
    node1:addChild(node1_3)
    
    -- Level 2 nodes for node2
    local node2_1 = Node:new(1, true)
    local node2_2 = Node:new(4, true)
    
    node2:addChild(node2_1)
    node2:addChild(node2_2)
    
    -- Level 2 nodes for node3
    local node3_1 = Node:new(8, true)
    local node3_2 = Node:new(6, true)
    local node3_3 = Node:new(3, true)
    
    node3:addChild(node3_1)
    node3:addChild(node3_2)
    node3:addChild(node3_3)
    
    -- Set leaf node values
    node1_1.value = 9
    node1_2.value = 2
    node1_3.value = 7
    node2_1.value = 1
    node2_2.value = 4
    node3_1.value = 8
    node3_2.value = 6
    node3_3.value = 3
    
    return root
end

-- Print the game tree structure
function print_tree(node, depth)
    if not node then return end
    
    local indent = string.rep("  ", depth)
    if node.value then
        print(indent .. "Leaf: " .. node.value)
    else
        print(indent .. "Node (max: " .. tostring(node.isMaximizing) .. ")")
    end
    
    for _, child in ipairs(node.children) do
        print_tree(child, depth + 1)
    end
end

-- Main execution
print("Alpha-Beta Pruning Algorithm Example")
print("====================================")

-- Create sample game tree
local game_tree = create_sample_tree()

print("\nGame Tree Structure:")
print_tree(game_tree, 0)

print("\nRunning Alpha-Beta Pruning...")
print("Depth: 3")
print("Maximizing player at root")

local result = alpha_beta(game_tree, 3, -math.huge, math.huge, true)
print("\nOptimal value: " .. result)

-- Demonstrate with a simpler example
print("\n" .. string.rep("=", 50))
print("Simple Example with Manual Tree")

-- Simple 3-level tree
local simple_root = Node:new(nil, true)

-- Level 1
local l1_1 = Node:new(5, false)
local l1_2 = Node:new(3, false)
local l1_3 = Node:new(8, false)

simple_root:addChild(l1_1)
simple_root:addChild(l1_2)
simple_root:addChild(l1_3)

-- Level 2 (children of l1_1)
local l2_1 = Node:new(2, true)
local l2_2 = Node:new(9, true)
l1_1:addChild(l2_1)
l1_1:addChild(l2_2)

-- Level 2 (children of l1_2)
local l2_3 = Node:new(6, true)
local l2_4 = Node:new(1, true)
l1_2:addChild(l2_3)
l1_2:addChild(l2_4)

-- Level 2 (children of l1_3)
local l2_5 = Node:new(4, true)
local l2_6 = Node:new(7, true)
l1_3:addChild(l2_5)
l1_3:addChild(l2_6)

-- Set leaf values
l2_1.value = 2
l2_2.value = 9
l2_3.value = 6
l2_4.value = 1
l2_5.value = 4
l2_6.value = 7

print("\nSimple Game Tree:")
print_tree(simple_root, 0)

local simple_result = alpha_beta(simple_root, 3, -math.huge, math.huge, true)
print("\nOptimal value for simple tree: " .. simple_result)
```

## Key Features of this Implementation:

1. **Node Structure**: Each node has value, maximizing player flag, children, and alpha/beta values
2. **Recursive Algorithm**: Implements the core alpha-beta pruning logic
3. **Pruning Mechanism**: 
   - Alpha cut-off when β ≤ α
   - Beta cut-off when α ≥ β
4. **Game Tree Construction**: Demonstrates how to build a sample tree
5. **Visualization**: Includes tree printing functionality

## How Alpha-Beta Pruning Works:

1. **Alpha**: Best value that the maximizing player can guarantee at that level or above
2. **Beta**: Best value that the minimizing player can guarantee at that level or above
3. **Pruning**: When α ≥ β, the remaining branches can be ignored
4. **Optimization**: Reduces the number of nodes evaluated compared to minimax

## Time Complexity:
- **Without pruning**: O(b^d) where b is branching factor and d is depth
- **With pruning**: O(b^(d/2)) in best case, significantly better in practice

This implementation demonstrates the core concepts of alpha-beta pruning while maintaining clean, readable Lua code structure.

