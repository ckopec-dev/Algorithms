# Boyer-Moore Majority Vote Algorithm in Lua

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times).

```lua
function boyer_moore_majority_vote(arr)
    -- Check for empty array
    if not arr or #arr == 0 then
        return nil
    end
    
    -- Phase 1: Find candidate
    local candidate = arr[1]
    local count = 1
    
    for i = 2, #arr do
        if arr[i] == candidate then
            count = count + 1
        else
            count = count - 1
            if count == 0 then
                candidate = arr[i]
                count = 1
            end
        end
    end
    
    -- Phase 2: Verify candidate is actually majority
    local verify_count = 0
    for i = 1, #arr do
        if arr[i] == candidate then
            verify_count = verify_count + 1
        end
    end
    
    -- Return candidate if it's actually majority, otherwise return nil
    if verify_count > #arr / 2 then
        return candidate
    else
        return nil
    end
end

-- Example usage
print("Example 1:")
local arr1 = {3, 3, 4, 2, 4, 4, 2, 4, 4}
local result1 = boyer_moore_majority_vote(arr1)
print("Array: " .. table.concat(arr1, ", "))
print("Majority element: " .. tostring(result1))

print("\nExample 2:")
local arr2 = {1, 1, 1, 2, 2}
local result2 = boyer_moore_majority_vote(arr2)
print("Array: " .. table.concat(arr2, ", "))
print("Majority element: " .. tostring(result2))

print("\nExample 3:")
local arr3 = {1, 2, 3, 4, 5}
local result3 = boyer_moore_majority_vote(arr3)
print("Array: " .. table.concat(arr3, ", "))
print("Majority element: " .. tostring(result3))

print("\nExample 4:")
local arr4 = {2, 2, 2, 2, 1, 1, 1}
local result4 = boyer_moore_majority_vote(arr4)
print("Array: " .. table.concat(arr4, ", "))
print("Majority element: " .. tostring(result4))
```

## Output:
```
Example 1:
Array: 3, 3, 4, 2, 4, 4, 2, 4, 4
Majority element: 4

Example 2:
Array: 1, 1, 1, 2, 2
Majority element: 1

Example 3:
Array: 1, 2, 3, 4, 5
Majority element: nil

Example 4:
Array: 2, 2, 2, 2, 1, 1, 1
Majority element: 2
```

## How it works:

1. **Phase 1 (Voting)**: 
   - Start with first element as candidate and count = 1
   - For each subsequent element:
     - If same as candidate: increment count
     - If different: decrement count
     - If count reaches 0: new candidate becomes current element

2. **Phase 2 (Verification)**:
   - Count occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

The algorithm is efficient and works in linear time with constant space, making it optimal for this problem.

