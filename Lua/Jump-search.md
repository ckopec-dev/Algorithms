# Jump Search Algorithm in Lua

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified block.

## Implementation

```lua
function jump_search(arr, target)
    local n = #arr
    
    -- Handle edge cases
    if n == 0 then
        return -1
    end
    
    -- Calculate optimal jump size
    local jump_size = math.floor(math.sqrt(n))
    
    -- Find the block where target may be present
    local prev = 0
    while arr[math.min(jump_size, n)] < target do
        prev = jump_size
        jump_size = jump_size + math.floor(math.sqrt(n))
        
        -- If we've gone past the array
        if prev >= n then
            return -1
        end
    end
    
    -- Linear search in the identified block
    for i = prev + 1, math.min(jump_size, n) do
        if arr[i] == target then
            return i - 1  -- Return 0-based index
        end
    end
    
    return -1  -- Target not found
end

-- Example usage
local sorted_array = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19}
local target = 7

local result = jump_search(sorted_array, target)

if result ~= -1 then
    print("Element found at index: " .. result)
else
    print("Element not found")
end

-- Test with another example
target = 15
result = jump_search(sorted_array, target)

if result ~= -1 then
    print("Element found at index: " .. result)
else
    print("Element not found")
end

-- Test with element not in array
target = 8
result = jump_search(sorted_array, target)

if result ~= -1 then
    print("Element found at index: " .. result)
else
    print("Element not found")
end
```

## How it works:

1. **Calculate jump size**: Use `floor(sqrt(n))` as the optimal jump size
2. **Jump through array**: Move through the array in jumps of the calculated size
3. **Find block**: When we find an element greater than or equal to the target, we know the target must be in the previous block
4. **Linear search**: Perform linear search in the identified block

## Time Complexity:
- **Best case**: O(1) - when target is found at the first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity: O(1)

## Output:
```
Element found at index: 3
Element found at index: 7
Element not found
```

This implementation demonstrates the jump search algorithm's efficiency for sorted arrays, providing better performance than linear search while being simpler to implement than binary search.

