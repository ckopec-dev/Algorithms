# Comb Sort Algorithm in Lua

```lua
function comb_sort(arr)
    local gap = #arr
    local shrink = 1.3
    local swapped = true
    
    while swapped do
        -- Calculate new gap
        gap = math.floor(gap / shrink)
        
        -- If gap becomes less than 1, set it to 1
        if gap < 1 then
            gap = 1
            swapped = false
        end
        
        -- Compare elements with current gap
        for i = 1, #arr - gap do
            if arr[i] > arr[i + gap] then
                -- Swap elements
                arr[i], arr[i + gap] = arr[i + gap], arr[i]
                swapped = true
            end
        end
    end
    
    return arr
end

-- Example usage
local numbers = {64, 34, 25, 12, 22, 11, 90, 5}
print("Original array:")
for i, v in ipairs(numbers) do
    io.write(v .. " ")
end
print()

-- Sort the array
local sorted = comb_sort(numbers)
print("Sorted array:")
for i, v in ipairs(sorted) do
    io.write(v .. " ")
end
print()
```

**Output:**
```
Original array:
64 34 25 12 22 11 90 5 
Sorted array:
5 11 12 22 25 34 64 90 
```

## How it works:

1. **Initialize**: Start with a gap equal to the array size
2. **Shrink**: Reduce the gap by a shrink factor (typically 1.3)
3. **Compare**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until no more swaps are needed and gap becomes 1

The comb sort algorithm is an improvement over bubble sort, as it eliminates "turtles" (small values near the end) more quickly by using gaps larger than 1.

