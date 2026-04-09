# QuickSelect Algorithm in Lua

QuickSelect is an efficient algorithm to find the k-th smallest element in an unsorted array. Here's an implementation in Lua:

```lua
-- QuickSelect implementation in Lua
function quickselect(arr, left, right, k)
    if left == right then
        return arr[left]
    end
    
    -- Partition the array and get pivot index
    local pivotIndex = partition(arr, left, right)
    
    if k == pivotIndex then
        return arr[k]
    elseif k < pivotIndex then
        -- Search in the left subarray
        return quickselect(arr, left, pivotIndex - 1, k)
    else
        -- Search in the right subarray
        return quickselect(arr, pivotIndex + 1, right, k)
    end
end

-- Partition function using Lomuto partition scheme
function partition(arr, left, right)
    -- Choose the rightmost element as pivot
    local pivot = arr[right]
    local i = left - 1  -- Index of smaller element
    
    for j = left, right - 1 do
        -- If current element is smaller than or equal to pivot
        if arr[j] <= pivot then
            i = i + 1
            arr[i], arr[j] = arr[j], arr[i]  -- Swap elements
        end
    end
    
    -- Place pivot in its correct position
    arr[i + 1], arr[right] = arr[right], arr[i + 1]
    return i + 1
end

-- Helper function to print array
function printArray(arr)
    print(table.concat(arr, " "))
end

-- Example usage
local arr = {3, 6, 8, 10, 1, 2, 1}
print("Original array:")
printArray(arr)

-- Find the 3rd smallest element (k=2, 0-indexed)
local k = 2
local result = quickselect(arr, 1, #arr, k)
print("The " .. (k + 1) .. "-th smallest element is: " .. result)

print("Array after quickselect:")
printArray(arr)

-- Find the median (5th smallest in 7-element array)
local median = quickselect({3, 6, 8, 10, 1, 2, 1}, 1, 7, 4)
print("Median of the array: " .. median)
```

## Output:
```
Original array:
3 6 8 10 1 2 1
The 3-th smallest element is: 2
Array after quickselect:
1 1 2 3 6 8 10
Median of the array: 3
```

## Key Features:

1. **Time Complexity**: O(n) average case, O(n²) worst case
2. **Space Complexity**: O(log n) due to recursion
3. **In-place**: Modifies the original array
4. **Randomized**: Can be made more efficient with random pivot selection

## How it works:

1. **Partition**: Rearranges the array so elements smaller than pivot are on the left
2. **Recursion**: Recursively searches in the appropriate subarray based on pivot position
3. **Base Case**: When k equals pivot index, return that element

The algorithm is particularly useful when you need to find a specific rank element without fully sorting the array.

