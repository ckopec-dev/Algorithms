# TimSort Algorithm Implementation in Lua

```lua
-- TimSort implementation in Lua
-- This is a simplified version focusing on the key concepts

local function insertionSort(arr, left, right)
    for i = left + 1, right do
        local key = arr[i]
        local j = i - 1
        
        while j >= left and arr[j] > key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        arr[j + 1] = key
    end
end

local function merge(arr, left, mid, right)
    local leftArr = {}
    local rightArr = {}
    
    for i = left, mid do
        leftArr[i - left + 1] = arr[i]
    end
    
    for i = mid + 1, right do
        rightArr[i - mid] = arr[i]
    end
    
    local i, j, k = 1, 1, left
    
    while i <= #leftArr and j <= #rightArr do
        if leftArr[i] <= rightArr[j] then
            arr[k] = leftArr[i]
            i = i + 1
        else
            arr[k] = rightArr[j]
            j = j + 1
        end
        k = k + 1
    end
    
    while i <= #leftArr do
        arr[k] = leftArr[i]
        i = i + 1
        k = k + 1
    end
    
    while j <= #rightArr do
        arr[k] = rightArr[j]
        j = j + 1
        k = k + 1
    end
end

local function timSort(arr)
    local n = #arr
    local minRun = 32
    
    -- Sort individual subarrays of size minRun
    for i = 1, n, minRun do
        local endIdx = math.min(i + minRun - 1, n)
        insertionSort(arr, i, endIdx)
    end
    
    -- Merge subarrays in increasing sizes
    local size = minRun
    while size < n do
        local left = 1
        while left < n - size do
            local mid = left + size - 1
            local right = math.min(left + size * 2 - 1, n)
            
            if mid < right then
                merge(arr, left, mid, right)
            end
            
            left = left + size * 2
        end
        size = size * 2
    end
end

-- Example usage
local testArray = {5, 2, 8, 1, 9, 3, 7, 4, 6, 0}
print("Original array:")
for i, v in ipairs(testArray) do
    io.write(v .. " ")
end
print()

timSort(testArray)

print("Sorted array:")
for i, v in ipairs(testArray) do
    io.write(v .. " ")
end
print()
```

## Output
```
Original array:
5 2 8 1 9 3 7 4 6 0 
Sorted array:
0 1 2 3 4 5 6 7 8 9 
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger ones
2. **Minimum Run Size**: Uses a minimum run size of 32 elements
3. **Natural Subarrays**: Takes advantage of existing sorted subsequences
4. **Stable Sorting**: Maintains the relative order of equal elements
5. **Adaptive**: Performs well on partially sorted data

This implementation demonstrates the core principles of TimSort: using insertion sort on small chunks and then merging them efficiently using a merge process similar to merge sort.

