# Insertion Sort in Lua

Here's an implementation of the insertion sort algorithm in Lua:

```lua
function insertion_sort(arr)
    -- Iterate through the array starting from the second element
    for i = 2, #arr do
        local key = arr[i]  -- Current element to be inserted
        local j = i - 1     -- Index of the last element in sorted portion
        
        -- Move elements greater than key one position ahead
        while j >= 1 and arr[j] > key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        
        -- Insert key at its correct position
        arr[j + 1] = key
    end
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

insertion_sort(numbers)

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

1. Start from the second element (index 2) since a single element is already "sorted"
2. Take the current element as `key`
3. Compare `key` with elements in the sorted portion (to its left)
4. Shift larger elements one position to the right
5. Insert `key` at its correct position
6. Repeat until the entire array is sorted

The time complexity is O(n²) in the worst case and O(n) in the best case (already sorted array).

