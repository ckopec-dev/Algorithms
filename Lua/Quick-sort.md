# Quick Sort Algorithm in Lua

Here's an implementation of the Quick Sort algorithm in Lua:

```lua
function quicksort(arr, low, high)
    if low < high then
        -- Partition the array and get pivot index
        local pivotIndex = partition(arr, low, high)
        
        -- Recursively sort elements before and after partition
        quicksort(arr, low, pivotIndex - 1)
        quicksort(arr, pivotIndex + 1, high)
    end
end

function partition(arr, low, high)
    -- Choose the rightmost element as pivot
    local pivot = arr[high]
    
    -- Index of smaller element (indicates right position of pivot)
    local i = low - 1
    
    for j = low, high - 1 do
        -- If current element is smaller than or equal to pivot
        if arr[j] <= pivot then
            i = i + 1
            arr[i], arr[j] = arr[j], arr[i]  -- Swap elements
        end
    end
    
    -- Place pivot in its correct position
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90, 5}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

quicksort(numbers, 1, #numbers)

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
5

Sorted array:
5
11
12
22
25
34
64
90
```

## How it works:

1. **Partition Function**: Rearranges the array so that elements smaller than the pivot are on the left, and elements greater than the pivot are on the right
2. **Recursive Sorting**: The algorithm recursively applies the same process to the sub-arrays on both sides of the pivot
3. **Base Case**: When the sub-array has one or zero elements, it's already sorted

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) - when the pivot is always the smallest or largest element

## Space Complexity:
- O(log n) - due to the recursive call stack

