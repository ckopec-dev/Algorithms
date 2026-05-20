# Longest Increasing Subsequence (LIS) Algorithm in Lua

Here's an implementation of the LIS algorithm using dynamic programming with O(n²) time complexity:

```lua
function longest_increasing_subsequence(arr)
    if not arr or #arr == 0 then
        return 0
    end
    
    local n = #arr
    local dp = {}  -- dp[i] stores the length of LIS ending at index i
    
    -- Initialize all positions with 1 (each element forms a subsequence of length 1)
    for i = 1, n do
        dp[i] = 1
    end
    
    -- Fill dp array using dynamic programming
    for i = 2, n do
        for j = 1, i - 1 do
            if arr[i] > arr[j] and dp[i] < dp[j] + 1 then
                dp[i] = dp[j] + 1
            end
        end
    end
    
    -- Find the maximum length
    local max_length = 0
    for i = 1, n do
        if dp[i] > max_length then
            max_length = dp[i]
        end
    end
    
    return max_length
end

-- Function to reconstruct the actual LIS (optional)
function get_lis_sequence(arr)
    if not arr or #arr == 0 then
        return {}
    end
    
    local n = #arr
    local dp = {}
    local parent = {}
    
    for i = 1, n do
        dp[i] = 1
        parent[i] = 0
    end
    
    for i = 2, n do
        for j = 1, i - 1 do
            if arr[i] > arr[j] and dp[i] < dp[j] + 1 then
                dp[i] = dp[j] + 1
                parent[i] = j
            end
        end
    end
    
    -- Find the index with maximum LIS length
    local max_length = 0
    local max_index = 1
    for i = 1, n do
        if dp[i] > max_length then
            max_length = dp[i]
            max_index = i
        end
    end
    
    -- Reconstruct the sequence
    local lis = {}
    local current = max_index
    while current > 0 do
        table.insert(lis, 1, arr[current])
        current = parent[current]
    end
    
    return lis
end

-- Example usage
local test_array = {10, 22, 9, 33, 21, 50, 41, 60, 80}
print("Input array: " .. table.concat(test_array, ", "))

local lis_length = longest_increasing_subsequence(test_array)
print("Length of LIS: " .. lis_length)

local lis_sequence = get_lis_sequence(test_array)
print("Actual LIS sequence: " .. table.concat(lis_sequence, ", "))

-- Additional test cases
print("\n--- Additional Test Cases ---")

local test1 = {3, 4, -1, 0, 6, 2, 3}
print("Array: " .. table.concat(test1, ", "))
print("LIS length: " .. longest_increasing_subsequence(test1))

local test2 = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1}
print("Array: " .. table.concat(test2, ", "))
print("LIS length: " .. longest_increasing_subsequence(test2))

local test3 = {1, 2, 3, 4, 5, 6, 7, 8, 9}
print("Array: " .. table.concat(test3, ", "))
print("LIS length: " .. longest_increasing_subsequence(test3))
```

## Output:
```
Input array: 10, 22, 9, 33, 21, 50, 41, 60, 80
Length of LIS: 6
Actual LIS sequence: 10, 22, 33, 50, 60, 80

--- Additional Test Cases ---
Array: 3, 4, -1, 0, 6, 2, 3
LIS length: 4
Array: 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
LIS length: 1
Array: 1, 2, 3, 4, 5, 6, 7, 8, 9
LIS length: 9
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses `dp[i]` to store the length of the longest increasing subsequence ending at index `i`.

2. **Time Complexity**: O(n²) where n is the length of the array.

3. **Space Complexity**: O(n) for the dp array.

4. **How it works**:
   - Initialize all positions with 1 (each element forms a subsequence of length 1)
   - For each element, check all previous elements that are smaller
   - Update dp[i] if we can extend a longer subsequence
   - Return the maximum value in the dp array

The second function `get_lis_sequence` reconstructs the actual subsequence by tracking parent indices, which is useful when you need to know which elements form the LIS rather than just its length.

