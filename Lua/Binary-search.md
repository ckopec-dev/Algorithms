# Binary Search Algorithm in Lua

Here's an implementation of the binary search algorithm in Lua:

```lua
function binary_search(arr, target)
    local left = 1
    local right = #arr
    
    while left <= right do
        local mid = math.floor((left + right) / 2)
        
        if arr[mid] == target then
            return mid  -- Return the index if found
        elseif arr[mid] < target then
            left = mid + 1  -- Search in the right half
        else
            right = mid - 1  -- Search in the left half
        end
    end
    
    return -1  -- Return -1 if not found
end

-- Example usage:
local sorted_array = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19}

-- Search for existing element
local result = binary_search(sorted_array, 7)
print("Found 7 at index:", result)  -- Output: Found 7 at index: 4

-- Search for non-existing element
local result2 = binary_search(sorted_array, 6)
print("Found 6 at index:", result2)  -- Output: Found 6 at index: -1

-- Search for first element
local result3 = binary_search(sorted_array, 1)
print("Found 1 at index:", result3)  -- Output: Found 1 at index: 1

-- Search for last element
local result4 = binary_search(sorted_array, 19)
print("Found 19 at index:", result4)  -- Output: Found 19 at index: 10
```

## How it works:

1. **Initialize pointers**: Set `left` to 1 and `right` to the length of the array
2. **Loop while valid range**: Continue while `left ≤ right`
3. **Calculate middle**: Find the middle index using `math.floor((left + right) / 2)`
4. **Compare and narrow search**:
   - If middle element equals target: return the index
   - If middle element is less than target: search right half
   - If middle element is greater than target: search left half
5. **Return -1**: If loop ends without finding target

## Time Complexity:
- **Best case**: O(1) - element found at middle
- **Average/Worst case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

**Note**: The array must be sorted for binary search to work correctly.

