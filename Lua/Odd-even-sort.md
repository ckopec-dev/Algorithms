# Odd-Even Sort Algorithm in Lua

Here's an implementation of the Odd-Even sort (also known as Brick Sort) algorithm in Lua:

```lua
function odd_even_sort(arr)
    local n = #arr
    local is_sorted = false
    
    while not is_sorted do
        is_sorted = true
        
        -- Odd phase: compare odd indexed elements
        for i = 1, n - 1, 2 do
            if arr[i] > arr[i + 1] then
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                is_sorted = false
            end
        end
        
        -- Even phase: compare even indexed elements
        for i = 2, n - 1, 2 do
            if arr[i] > arr[i + 1] then
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                is_sorted = false
            end
        end
    end
    
    return arr
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
print("Original array:")
for i, v in ipairs(numbers) do
    io.write(v .. " ")
end
print()

-- Sort the array
odd_even_sort(numbers)

print("Sorted array:")
for i, v in ipairs(numbers) do
    io.write(v .. " ")
end
print()
```

## How it works:

1. **Odd Phase**: Compare elements at odd indices (1, 3, 5, ...) with their next neighbors
2. **Even Phase**: Compare elements at even indices (2, 4, 6, ...) with their next neighbors
3. Repeat until no more swaps are needed

## Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 42 
Sorted array:
11 12 22 25 34 42 50 64 76 88 90 
```

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in place

The algorithm is stable and works well for small datasets or when the array is partially sorted.

