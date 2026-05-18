# Cocktail Shaker Sort in Lua

```lua
function cocktail_shaker_sort(arr)
    local n = #arr
    local start = 1
    local end_idx = n
    local swapped = true
    
    while swapped do
        swapped = false
        
        -- Forward pass (left to right)
        for i = start, end_idx - 1 do
            if arr[i] > arr[i + 1] then
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                swapped = true
            end
        end
        
        -- If no swapping occurred, array is sorted
        if not swapped then
            break
        end
        
        -- Move the end pointer one position to the left
        end_idx = end_idx - 1
        
        -- Backward pass (right to left)
        for i = end_idx, start + 1, -1 do
            if arr[i] < arr[i - 1] then
                arr[i], arr[i - 1] = arr[i - 1], arr[i]
                swapped = true
            end
        end
        
        -- Move the start pointer one position to the right
        start = start + 1
    end
    
    return arr
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

cocktail_shaker_sort(numbers)

print("\nSorted array:")
for i, v in ipairs(numbers) do
    print(v)
end
```

## How it works:

1. **Bidirectional Sorting**: Unlike regular bubble sort, cocktail shaker sort alternates between forward and backward passes
2. **Two Passes**: 
   - Forward pass: Bubbles largest element to the right
   - Backward pass: Bubbles smallest element to the left
3. **Boundary Optimization**: After each pass, the sorted boundaries (start and end) are adjusted
4. **Early Termination**: If no swaps occur during a complete pass, the array is sorted

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

