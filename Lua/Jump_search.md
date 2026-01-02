# Jump Search Algorithm in Lua

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified range.

## Implementation

```lua
function jumpSearch(arr, target)
    local n = #arr
    
    -- Handle edge cases
    if n == 0 then
        return -1
    end
    
    -- Calculate optimal jump size
    local jumpSize = math.floor(math.sqrt(n))
    
    -- Find the block where element is present
    local prev = 0
    while arr[math.min(jumpSize, n)] < target do
        prev = jumpSize
        jumpSize = jumpSize + math.floor(math.sqrt(n))
        
        -- If we've gone past the array
        if prev >= n then
            return -1
        end
    end
    
    -- Linear search in the identified block
    while arr[prev + 1] < target do
        prev = prev + 1
        
        -- If we've gone past the array or found the element
        if prev == math.min(jumpSize, n) then
            return -1
        end
    end
    
    -- If element is found
    if arr[prev + 1] == target then
        return prev + 1
    end
    
    return -1
end

-- Example usage
local sortedArray = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19}
local target = 7

local result = jumpSearch(sortedArray, target)

if result ~= -1 then
    print("Element " .. target .. " found at index: " .. result)
else
    print("Element " .. target .. " not found in array")
end

-- Test with another element
target = 15
result = jumpSearch(sortedArray, target)

if result ~= -1 then
    print("Element " .. target .. " found at index: " .. result)
else
    print("Element " .. target .. " not found in array")
end

-- Test with element not in array
target = 8
result = jumpSearch(sortedArray, target)

if result ~= -1 then
    print("Element " .. target .. " found at index: " .. result)
else
    print("Element " .. target .. " not found in array")
end
```

## Output
```
Element 7 found at index: 4
Element 15 found at index: 8
Element 8 not found in array
```

## How it works:

1. **Calculate jump size**: Use square root of array length as jump size
2. **Jump blocks**: Move through the array in jumps of the calculated size
3. **Find range**: When we find an element greater than target, we know the target is in the previous block
4. **Linear search**: Perform linear search within the identified block

## Time Complexity:
- **Best case**: O(1) - element found at first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity:
- O(1) - only uses a constant amount of extra space

Jump search is particularly useful when the array is large and the cost of jumping is relatively low compared to linear search, making it more efficient than linear search for large sorted arrays.

