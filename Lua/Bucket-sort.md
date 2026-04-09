# Bucket Sort Algorithm in Lua

Here's an implementation of the Bucket Sort algorithm in Lua:

```lua
function bucketSort(arr, bucketCount)
    if #arr <= 1 then
        return arr
    end
    
    -- Find minimum and maximum values
    local min = arr[1]
    local max = arr[1]
    
    for i = 2, #arr do
        if arr[i] < min then
            min = arr[i]
        elseif arr[i] > max then
            max = arr[i]
        end
    end
    
    -- Create buckets
    local buckets = {}
    for i = 1, bucketCount do
        buckets[i] = {}
    end
    
    -- Determine bucket range
    local range = (max - min) / bucketCount
    
    -- Distribute elements into buckets
    for i = 1, #arr do
        local bucketIndex
        if arr[i] == max then
            bucketIndex = bucketCount
        else
            bucketIndex = math.floor((arr[i] - min) / range) + 1
        end
        table.insert(buckets[bucketIndex], arr[i])
    end
    
    -- Sort each bucket and concatenate results
    local result = {}
    for i = 1, bucketCount do
        -- Sort the bucket (using built-in sort)
        table.sort(buckets[i])
        for j = 1, #buckets[i] do
            table.insert(result, buckets[i][j])
        end
    end
    
    return result
end

-- Example usage
local numbers = {4, 2, 2, 8, 3, 3, 1}
print("Original array:")
for i, v in ipairs(numbers) do
    print(v)
end

local sorted = bucketSort(numbers, 4)
print("\nSorted array:")
for i, v in ipairs(sorted) do
    print(v)
end
```

## Output:
```
Original array:
4
2
2
8
3
3
1

Sorted array:
1
2
2
3
3
4
8
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Buckets**: Initialize empty buckets (arrays)
3. **Distribute Elements**: Place each element into the appropriate bucket based on its value
4. **Sort Buckets**: Sort each individual bucket (using Lua's built-in sort)
5. **Concatenate**: Combine all sorted buckets into the final result

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements fall into the same bucket

## Space Complexity: O(n + k)

The algorithm is particularly efficient when the input is uniformly distributed over a range.

