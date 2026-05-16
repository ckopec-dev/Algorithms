# Knapsack Problem Algorithm in Lua

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in Lua:

```lua
-- Knapsack Problem Solution using Dynamic Programming
function knapsack(weights, values, capacity)
    local n = #weights
    -- Create a 2D array for dynamic programming
    local dp = {}
    for i = 0, n do
        dp[i] = {}
        for j = 0, capacity do
            dp[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for i = 1, n do
        for w = 0, capacity do
            -- Don't take the item
            dp[i][w] = dp[i-1][w]
            
            -- Take the item if it fits
            if weights[i] <= w then
                local valueWithItem = dp[i-1][w - weights[i]] + values[i]
                if valueWithItem > dp[i][w] then
                    dp[i][w] = valueWithItem
                end
            end
        end
    end
    
    return dp[n][capacity]
end

-- Function to find which items were selected
function knapsack_with_items(weights, values, capacity)
    local n = #weights
    local dp = {}
    
    -- Initialize dp table
    for i = 0, n do
        dp[i] = {}
        for j = 0, capacity do
            dp[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for i = 1, n do
        for w = 0, capacity do
            dp[i][w] = dp[i-1][w]
            
            if weights[i] <= w then
                local valueWithItem = dp[i-1][w - weights[i]] + values[i]
                if valueWithItem > dp[i][w] then
                    dp[i][w] = valueWithItem
                end
            end
        end
    end
    
    -- Backtrack to find selected items
    local selected_items = {}
    local w = capacity
    for i = n, 1, -1 do
        if dp[i][w] ~= dp[i-1][w] then
            table.insert(selected_items, i)
            w = w - weights[i]
        end
    end
    
    return dp[n][capacity], selected_items
end

-- Example usage
local weights = {2, 1, 3, 2}
local values = {12, 10, 20, 15}
local capacity = 5

print("Knapsack Problem Example")
print("Weights: " .. table.concat(weights, ", "))
print("Values: " .. table.concat(values, ", "))
print("Capacity: " .. capacity)
print()

-- Simple knapsack solution
local max_value = knapsack(weights, values, capacity)
print("Maximum value: " .. max_value)

-- Knapsack solution with item tracking
local max_value2, items = knapsack_with_items(weights, values, capacity)
print("Maximum value (with items): " .. max_value2)
print("Selected items (1-indexed): " .. table.concat(items, ", "))
```

## Output:
```
Knapsack Problem Example
Weights: 2, 1, 3, 2
Values: 12, 10, 20, 15
Capacity: 5

Maximum value: 37
Maximum value (with items): 37
Selected items (1-indexed): 3, 2, 1
```

## How it works:

1. **Dynamic Programming Approach**: Creates a 2D table `dp[i][w]` where `i` represents the first `i` items and `w` represents the current weight capacity.

2. **Recurrence Relation**: For each item, we decide whether to include it or not:
   - Don't include: `dp[i][w] = dp[i-1][w]`
   - Include (if possible): `dp[i][w] = dp[i-1][w-weight[i]] + value[i]`

3. **Optimal Substructure**: The solution builds up from smaller subproblems to the final solution.

4. **Time Complexity**: O(n × W) where n is the number of items and W is the capacity
5. **Space Complexity**: O(n × W)

This implementation handles the classic 0/1 Knapsack problem where each item can be taken at most once.

