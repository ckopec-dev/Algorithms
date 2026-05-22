# Counting Sort Algorithm in Lua

```lua
function counting_sort(arr)
    -- Handle edge cases
    if not arr or #arr == 0 then
        return arr
    end
    
    -- Find the range of values
    local min_val = arr[1]
    local max_val = arr[1]
    
    for i = 1, #arr do
        if arr[i] < min_val then
            min_val = arr[i]
        elseif arr[i] > max_val then
            max_val = arr[i]
        end
    end
    
    -- Create counting array
    local range = max_val - min_val + 1
    local count = {}
    
    -- Initialize count array with zeros
    for i = 1, range do
        count[i] = 0
    end
    
    -- Count occurrences of each element
    for i = 1, #arr do
        count[arr[i] - min_val + 1] = count[arr[i] - min_val + 1] + 1
    end
    
    -- Reconstruct sorted array
    local sorted = {}
    local index = 1
    
    for i = 1, range do
        while count[i] > 0 do
            sorted[index] = i + min_val - 1
            index = index + 1
            count[i] = count[i] - 1
        end
    end
    
    return sorted
end

-- Example usage
local arr = {4, 2, 2, 8, 3, 3, 1}
print("Original array: " .. table.concat(arr, ", "))

local sorted_arr = counting_sort(arr)
print("Sorted array: " .. table.concat(sorted_arr, ", "))

-- Another example with negative numbers
local arr2 = {-5, -1, 3, 0, 2, -3}
print("\nOriginal array: " .. table.concat(arr2, ", "))

local sorted_arr2 = counting_sort(arr2)
print("Sorted array: " .. table.concat(sorted_arr2, ", "))
```

## Output:
```
Original array: 4, 2, 2, 8, 3, 3, 1
Sorted array: 1, 2, 2, 3, 3, 4, 8

Original array: -5, -1, 3, 0, 2, -3
Sorted array: -5, -3, -1, 0, 2, 3
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the array
2. **Create count array**: Initialize an array to count occurrences of each value
3. **Count elements**: Iterate through the input array and count each element
4. **Reconstruct**: Build the sorted array by iterating through the count array

## Time Complexity: O(n + k)
- where n is the number of elements and k is the range of input

## Space Complexity: O(k)
- where k is the range of input values

## Key Features:
- **Stable sorting**: Maintains relative order of equal elements
- **Efficient for small ranges**: Works well when the range of possible values is not significantly larger than the number of elements
- **Non-comparison based**: Doesn't use comparisons between elements

