# Selection Sort Algorithm in Lua

Here's an implementation of the selection sort algorithm in Lua:

```lua
function selection_sort(arr)
    local n = #arr
    
    -- Traverse through all array elements
    for i = 1, n do
        -- Find the minimum element in remaining unsorted array
        local min_idx = i
        for j = i + 1, n do
            if arr[j] < arr[min_idx] then
                min_idx = j
            end
        end
        
        -- Swap the found minimum element with the first element
        if min_idx ~= i then
            arr[i], arr[min_idx] = arr[min_idx], arr[i]
        end
    end
    
    return arr
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

selection_sort(numbers)

print("\nSorted array:")
for i, v in ipairs(numbers) do
    print(v)
end
```

## Output:
```
Original array:
64
34
25
12
22
11
90

Sorted array:
11
12
22
25
34
64
90
```

## How it works:

1. **Find minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place the minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in-place

The algorithm works by repeatedly finding the minimum element from the unsorted part and putting it at the beginning, gradually building the sorted portion from left to right.

