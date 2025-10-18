# Radix Sort Algorithm in Lua

Here's an implementation of the Radix Sort algorithm in Lua:

```lua
-- Radix Sort implementation in Lua
function radixSort(arr)
    if #arr <= 1 then
        return arr
    end
    
    -- Find the maximum number to know number of digits
    local max = arr[1]
    for i = 2, #arr do
        if arr[i] > max then
            max = arr[i]
        end
    end
    
    -- Do counting sort for every digit
    local exp = 1
    while math.floor(max / exp) > 0 do
        countingSortByDigit(arr, exp)
        exp = exp * 10
    end
    
    return arr
end

-- Counting sort for a specific digit
function countingSortByDigit(arr, exp)
    local n = #arr
    local output = {}
    local count = {}
    
    -- Initialize count array
    for i = 1, 10 do
        count[i] = 0
    end
    
    -- Store count of occurrences of each digit
    for i = 1, n do
        local digit = math.floor(arr[i] / exp) % 10
        count[digit + 1] = count[digit + 1] + 1
    end
    
    -- Change count[i] to actual position
    for i = 2, 10 do
        count[i] = count[i] + count[i - 1]
    end
    
    -- Build the output array
    for i = n, 1, -1 do
        local digit = math.floor(arr[i] / exp) % 10
        output[count[digit + 1]] = arr[i]
        count[digit + 1] = count[digit + 1] - 1
    end
    
    -- Copy the output array to arr
    for i = 1, n do
        arr[i] = output[i]
    end
end

-- Example usage
local numbers = {170, 45, 75, 90, 2, 802, 24, 66}
print("Original array:")
for i, v in ipairs(numbers) do
    io.write(v .. " ")
end
print()

-- Sort the array
radixSort(numbers)

print("Sorted array:")
for i, v in ipairs(numbers) do
    io.write(v .. " ")
end
print()
```

**Output:**
```
Original array:
170 45 75 90 2 802 24 66 
Sorted array:
2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit (units place), we perform counting sort
3. **Counting Sort**: For each digit position, we use counting sort to arrange elements
4. **Repeat**: Continue with the next digit position until we've processed all digits

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits and `n` is the number of elements.

## Space Complexity:
- O(n + k) where `k` is the range of input (typically 10 for decimal digits)

This implementation sorts integers in ascending order and handles edge cases like empty arrays or single-element arrays.

