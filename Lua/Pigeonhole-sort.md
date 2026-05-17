# Pigeonhole Sort Algorithm in Lua

```lua
function pigeonhole_sort(arr)
    -- Handle edge cases
    if #arr <= 1 then
        return arr
    end
    
    -- Find minimum and maximum values
    local min_val = arr[1]
    local max_val = arr[1]
    
    for i = 2, #arr do
        if arr[i] < min_val then
            min_val = arr[i]
        elseif arr[i] > max_val then
            max_val = arr[i]
        end
    end
    
    -- Calculate range
    local range = max_val - min_val + 1
    
    -- Create pigeonholes (array of counters)
    local pigeonholes = {}
    for i = 1, range do
        pigeonholes[i] = 0
    end
    
    -- Place elements in pigeonholes
    for i = 1, #arr do
        pigeonholes[arr[i] - min_val + 1] = pigeonholes[arr[i] - min_val + 1] + 1
    end
    
    -- Reconstruct sorted array
    local sorted_arr = {}
    local index = 1
    
    for i = 1, range do
        local count = pigeonholes[i]
        local value = i + min_val - 1
        
        for j = 1, count do
            sorted_arr[index] = value
            index = index + 1
        end
    end
    
    return sorted_arr
end

-- Example usage
local numbers = {8, 3, 2, 7, 4, 6, 1, 5}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

local sorted_numbers = pigeonhole_sort(numbers)
print("\nSorted array:")
for i, v in ipairs(sorted_numbers) do
    print(v)
end
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Pigeonholes**: Create an array of counters (pigeonholes) with size equal to the range
3. **Place Elements**: Count occurrences of each element and place them in corresponding pigeonholes
4. **Reconstruct**: Iterate through pigeonholes and rebuild the sorted array

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the pigeonhole array

## Output:
```
Original array:
8
3
2
7
4
6
1
5

Sorted array:
1
2
3
4
5
6
7
8
```

