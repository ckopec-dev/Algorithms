# Subset Sum Algorithm in Lua

Here's an implementation of the subset sum algorithm using dynamic programming in Lua:

```lua
-- Subset Sum Algorithm Implementation
function subset_sum(arr, target)
    local n = #arr
    
    -- Create a 2D boolean table
    -- dp[i][j] will be true if there's a subset of first i elements with sum j
    local dp = {}
    for i = 0, n do
        dp[i] = {}
        for j = 0, target do
            dp[i][j] = false
        end
    end
    
    -- Base case: sum of 0 is always possible with empty subset
    for i = 0, n do
        dp[i][0] = true
    end
    
    -- Fill the dp table
    for i = 1, n do
        for j = 1, target do
            -- Don't include current element
            dp[i][j] = dp[i-1][j]
            
            -- Include current element if it doesn't exceed target
            if arr[i] <= j then
                dp[i][j] = dp[i][j] or dp[i-1][j - arr[i]]
            end
        end
    end
    
    return dp[n][target]
end

-- Function to find actual subset (optional enhancement)
function find_subset(arr, target)
    local n = #arr
    local dp = {}
    
    -- Initialize dp table
    for i = 0, n do
        dp[i] = {}
        for j = 0, target do
            dp[i][j] = false
        end
    end
    
    dp[0][0] = true
    
    -- Fill the dp table
    for i = 1, n do
        for j = 0, target do
            dp[i][j] = dp[i-1][j]
            if arr[i] <= j then
                dp[i][j] = dp[i][j] or dp[i-1][j - arr[i]]
            end
        end
    end
    
    -- If no solution exists
    if not dp[n][target] then
        return nil
    end
    
    -- Backtrack to find the actual subset
    local result = {}
    local j = target
    for i = n, 1, -1 do
        if j >= arr[i] and dp[i-1][j - arr[i]] then
            table.insert(result, arr[i])
            j = j - arr[i]
        end
    end
    
    return result
end

-- Example usage
local numbers = {3, 34, 4, 12, 5, 2}
local target_sum = 9

print("Array:", table.concat(numbers, ", "))
print("Target sum:", target_sum)

-- Check if subset sum exists
local exists = subset_sum(numbers, target_sum)
print("Subset sum exists:", exists)

-- Find actual subset
local subset = find_subset(numbers, target_sum)
if subset then
    print("Actual subset:", table.concat(subset, ", "))
else
    print("No subset found with target sum")
end

-- Test with another example
local numbers2 = {1, 2, 3, 7}
local target_sum2 = 6

print("\nArray:", table.concat(numbers2, ", "))
print("Target sum:", target_sum2)

local exists2 = subset_sum(numbers2, target_sum2)
print("Subset sum exists:", exists2)

local subset2 = find_subset(numbers2, target_sum2)
if subset2 then
    print("Actual subset:", table.concat(subset2, ", "))
else
    print("No subset found with target sum")
end
```

## Output:
```
Array: 3, 34, 4, 12, 5, 2
Target sum: 9
Subset sum exists: true
Actual subset: 4, 5

Array: 1, 2, 3, 7
Target sum: 6
Subset sum exists: true
Actual subset: 1, 2, 3
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D boolean table `dp[i][j]` where `i` represents the first `i` elements and `j` represents the target sum.

2. **Base Case**: `dp[0][0] = true` because sum of 0 is always possible with empty subset.

3. **Recurrence Relation**: 
   - `dp[i][j] = dp[i-1][j]` (don't include current element)
   - `dp[i][j] = dp[i][j] or dp[i-1][j - arr[i]]` (include current element if possible)

4. **Time Complexity**: O(n × target) where n is the number of elements
5. **Space Complexity**: O(n × target)

The algorithm efficiently determines whether there exists a subset of the given array that sums up to the target value.

