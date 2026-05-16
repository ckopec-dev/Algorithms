# Bead Sort Algorithm in Lua

```lua
function bead_sort(arr)
    if #arr == 0 then return arr end
    
    -- Find maximum value to determine grid size
    local max_val = arr[1]
    for i = 2, #arr do
        if arr[i] > max_val then
            max_val = arr[i]
        end
    end
    
    -- Create grid representation
    local grid = {}
    for i = 1, max_val do
        grid[i] = {}
        for j = 1, #arr do
            grid[i][j] = 0
        end
    end
    
    -- Place beads (1s) in the grid
    for i = 1, #arr do
        for j = 1, arr[i] do
            grid[j][i] = 1
        end
    end
    
    -- Let beads fall down
    for i = 1, max_val do
        local count = 0
        for j = 1, #arr do
            if grid[i][j] == 1 then
                count = count + 1
                grid[i][j] = 0
            end
        end
        -- Place beads at the bottom
        for j = #arr - count + 1, #arr do
            grid[i][j] = 1
        end
    end
    
    -- Read sorted array from grid
    local result = {}
    for i = 1, #arr do
        local count = 0
        for j = 1, max_val do
            if grid[j][i] == 1 then
                count = count + 1
            end
        end
        result[i] = count
    end
    
    return result
end

-- Example usage
local test_array = {5, 3, 8, 1, 9, 2, 7, 4, 6}
print("Original array: " .. table.concat(test_array, ", "))

local sorted_array = bead_sort(test_array)
print("Sorted array: " .. table.concat(sorted_array, ", "))

-- Another example
local test_array2 = {4, 2, 7, 1, 9, 3}
print("\nOriginal array: " .. table.concat(test_array2, ", "))

local sorted_array2 = bead_sort(test_array2)
print("Sorted array: " .. table.concat(sorted_array2, ", "))
```

## How it works:

1. **Grid Creation**: Create a grid where rows represent bead levels and columns represent input elements
2. **Bead Placement**: Place beads (1s) in the grid according to the input values
3. **Gravity Simulation**: Let beads fall down to the bottom of their respective columns
4. **Result Extraction**: Read the final sorted values from the grid

## Output:
```
Original array: 5, 3, 8, 1, 9, 2, 7, 4, 6
Sorted array: 1, 2, 3, 4, 5, 6, 7, 8, 9

Original array: 4, 2, 7, 1, 9, 3
Sorted array: 1, 2, 3, 4, 7, 9
```

**Time Complexity**: O(S) where S is the sum of all input elements
**Space Complexity**: O(max_val × n) where max_val is the maximum input value and n is the array length

