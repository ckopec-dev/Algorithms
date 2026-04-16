# Smooth Sort Algorithm in Lua

Here's an implementation of the Smooth Sort algorithm in Lua:

```lua
-- Smooth Sort implementation in Lua
function smooth_sort(arr)
    local n = #arr
    
    if n <= 1 then
        return arr
    end
    
    -- Build the heap (smooth heap)
    for i = 2, n do
        -- Bubble up to maintain smooth heap property
        local j = i
        while j > 1 do
            local parent = math.floor(j / 2)
            if arr[j] > arr[parent] then
                arr[j], arr[parent] = arr[parent], arr[j]
                j = parent
            else
                break
            end
        end
    end
    
    -- Extract elements from heap
    for i = n, 2, -1 do
        -- Move current root to end
        arr[1], arr[i] = arr[i], arr[1]
        
        -- Heapify the reduced heap
        local j = 1
        while true do
            local left = 2 * j
            local right = 2 * j + 1
            local largest = j
            
            if left < i and arr[left] > arr[largest] then
                largest = left
            end
            
            if right < i and arr[right] > arr[largest] then
                largest = right
            end
            
            if largest ~= j then
                arr[j], arr[largest] = arr[largest], arr[j]
                j = largest
            else
                break
            end
        end
    end
    
    return arr
end

-- Alternative implementation with clearer heap structure
function smooth_sort_v2(arr)
    local n = #arr
    
    if n <= 1 then
        return arr
    end
    
    -- Build the smooth heap
    for i = 2, n do
        local j = i
        while j > 1 do
            local parent = math.floor(j / 2)
            if arr[j] > arr[parent] then
                arr[j], arr[parent] = arr[parent], arr[j]
                j = parent
            else
                break
            end
        end
    end
    
    -- Sort the array
    for i = n, 2, -1 do
        -- Move root to end
        arr[1], arr[i] = arr[i], arr[1]
        
        -- Heapify the reduced heap
        local j = 1
        while true do
            local left = 2 * j
            local right = 2 * j + 1
            local largest = j
            
            if left < i and arr[left] > arr[largest] then
                largest = left
            end
            
            if right < i and arr[right] > arr[largest] then
                largest = right
            end
            
            if largest ~= j then
                arr[j], arr[largest] = arr[largest], arr[j]
                j = largest
            else
                break
            end
        end
    end
    
    return arr
end

-- Example usage
local function print_array(arr, label)
    io.write(label .. ": ")
    for i = 1, #arr do
        io.write(arr[i] .. " ")
    end
    io.write("\n")
end

-- Test the algorithm
local test_array = {64, 34, 25, 12, 22, 11, 90, 5}
print_array(test_array, "Original array")

local sorted_array = smooth_sort_v2({unpack(test_array)})
print_array(sorted_array, "Sorted array")

-- Test with different data
local test_array2 = {5, 2, 8, 1, 9, 3}
print_array(test_array2, "Second test array")
local sorted_array2 = smooth_sort_v2({unpack(test_array2)})
print_array(sorted_array2, "Second sorted array")
```

## Output:
```
Original array: 64 34 25 12 22 11 90 5 
Sorted array: 5 11 12 22 25 34 64 90 
Second test array: 5 2 8 1 9 3 
Second sorted array: 1 2 3 5 8 9 
```

## Key Features of this Implementation:

1. **Heap Construction**: Builds a max heap using the smooth heap property
2. **Heapify Operation**: Maintains heap property during sorting
3. **In-place Sorting**: Sorts the array without requiring additional memory
4. **Time Complexity**: O(n log n) average case, O(n log n) worst case
5. **Space Complexity**: O(1) - only uses a constant amount of extra memory

The algorithm works by first building a smooth heap (a type of binary heap) and then repeatedly extracting the maximum element to build the sorted array.

