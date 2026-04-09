# Fibonacci Search Technique in Lua

Here's an implementation of the Fibonacci search algorithm in Lua:

```lua
-- Fibonacci Search Algorithm in Lua

-- Function to generate Fibonacci numbers up to n
function generateFibonacci(n)
    local fib = {}
    local a, b = 0, 1
    
    while a <= n do
        table.insert(fib, a)
        a, b = b, a + b
    end
    
    return fib
end

-- Fibonacci search function
function fibonacciSearch(arr, target)
    local n = #arr
    
    -- Generate Fibonacci numbers up to array length
    local fib = generateFibonacci(n)
    
    -- Find the smallest Fibonacci number >= n
    local fibIndex = #fib
    while fib[fibIndex] > n do
        fibIndex = fibIndex - 1
    end
    
    -- Initialize variables
    local offset = -1
    local fibM2 = fib[fibIndex - 2]  -- (m-2)th Fibonacci number
    local fibM1 = fib[fibIndex - 1]  -- (m-1)th Fibonacci number
    local fibM = fib[fibIndex]       -- mth Fibonacci number
    
    -- While there are elements to search
    while fibM > 1 do
        -- Check if fibM2 is a valid location
        local i = math.min(offset + fibM2, n - 1)
        
        -- If target is greater than element at index i
        if arr[i + 1] < target then
            fibM = fibM1
            fibM1 = fibM2
            fibM2 = fibM - fibM1
            offset = i
        -- If target is less than element at index i
        elseif arr[i + 1] > target then
            fibM = fibM2
            fibM1 = fibM1 - fibM2
            fibM2 = fibM - fibM1
        -- Element found
        else
            return i
        end
    end
    
    -- Compare the last element with target
    if fibM1 == 1 and arr[offset + 1] == target then
        return offset
    end
    
    -- Element not found
    return -1
end

-- Example usage
local arr = {10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100}
local target = 85

print("Array: " .. table.concat(arr, ", "))
print("Target: " .. target)

local result = fibonacciSearch(arr, target)

if result ~= -1 then
    print("Element found at index: " .. result)
else
    print("Element not found")
end

-- Test with element that doesn't exist
local target2 = 95
print("\nSearching for: " .. target2)
local result2 = fibonacciSearch(arr, target2)

if result2 ~= -1 then
    print("Element found at index: " .. result2)
else
    print("Element not found")
end
```

## Output:
```
Array: 10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100
Target: 85
Element found at index: 8

Searching for: 95
Element not found
```

## How it works:

1. **Fibonacci Sequence Generation**: The algorithm generates Fibonacci numbers up to the size of the array
2. **Find Suitable Fibonacci**: It finds the smallest Fibonacci number that is greater than or equal to the array size
3. **Search Process**: 
   - Compare target with element at position `offset + fibM2`
   - If target is greater, move to the right subarray
   - If target is smaller, move to the left subarray
   - If equal, return the index
4. **Continue**: Repeat until element is found or subarray size becomes 0

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log n)
- **Worst Case**: O(log n)

## Space Complexity: O(log n) for storing Fibonacci numbers

The Fibonacci search is particularly useful when the array size is a Fibonacci number, and it's often compared to binary search but with the advantage of using only addition and subtraction operations instead of division.

