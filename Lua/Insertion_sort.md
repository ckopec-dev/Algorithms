# Insertion Sort Algorithm in Lua

Here's an implementation of the insertion sort algorithm in Lua:

```lua
function insertionSort(arr)
    -- Traverse from the second element to the last
    for i = 2, #arr do
        local key = arr[i]  -- Current element to be inserted
        local j = i - 1     -- Index of the previous element
        
        -- Move elements greater than key one position ahead
        while j >= 1 and arr[j] > key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        
        -- Insert the key at its correct position
        arr[j + 1] = key
    end
    
    return arr
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

insertionSort(numbers)

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

1. **Start from the second element** (index 2) since the first element is considered already sorted
2. **Compare the current element** with elements to its left
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until all elements are processed

## Time Complexity:
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²) - when array is reverse sorted

## Space Complexity: O(1) - sorts in place

