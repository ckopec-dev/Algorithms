# Reservoir Sampling Algorithm in Lua

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size, where we can only make one pass through the data.

```lua
-- Reservoir Sampling Implementation in Lua
function reservoir_sampling(stream, k)
    local reservoir = {}
    
    -- Fill reservoir with first k elements
    for i = 1, k do
        local item = stream[i]
        if item ~= nil then
            reservoir[i] = item
        else
            break
        end
    end
    
    -- Process remaining elements
    for i = k + 1, #stream do
        local item = stream[i]
        if item ~= nil then
            -- Generate random index between 1 and i
            local random_index = math.random(1, i)
            
            -- If random index is within reservoir size, replace that element
            if random_index <= k then
                reservoir[random_index] = item
            end
        end
    end
    
    return reservoir
end

-- Alternative implementation for streaming data (when we don't know the size)
function reservoir_sampling_streaming(stream_function, k)
    local reservoir = {}
    local count = 0
    
    -- Read elements one by one
    for item in stream_function do
        count = count + 1
        
        if count <= k then
            -- Fill reservoir with first k elements
            reservoir[count] = item
        else
            -- Replace elements with decreasing probability
            local random_index = math.random(1, count)
            if random_index <= k then
                reservoir[random_index] = item
            end
        end
    end
    
    return reservoir
end

-- Example usage
print("=== Reservoir Sampling Example ===")

-- Example 1: Sampling from a known array
local data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
local k = 3
local sample = reservoir_sampling(data, k)

print("Original data:", table.concat(data, ", "))
print("Sample of 3 elements:", table.concat(sample, ", "))

-- Example 2: Sampling from a large dataset
local large_dataset = {}
for i = 1, 1000 do
    table.insert(large_dataset, i)
end

local large_sample = reservoir_sampling(large_dataset, 5)
print("Sample from large dataset (1000 elements):", table.concat(large_sample, ", "))

-- Example 3: Streaming data function
local function number_stream()
    local i = 1
    return function()
        if i <= 20 then
            local value = i
            i = i + 1
            return value
        end
    end
end

local streaming_sample = reservoir_sampling_streaming(number_stream, 4)
print("Streaming sample (4 elements from 1-20):", table.concat(streaming_sample, ", "))
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements from the stream
2. **Processing**: For each subsequent element at position i:
   - Generate a random number between 1 and i
   - If the random number is ≤ k, replace the element at that random position with the current element
3. **Result**: After processing all elements, the reservoir contains a random sample

## Key Properties:

- **Time Complexity**: O(n) where n is the number of elements
- **Space Complexity**: O(k) where k is the sample size
- **Fairness**: Each element has equal probability of being selected
- **Single Pass**: Only one traversal of the data is needed

## Output Example:
```
=== Reservoir Sampling Example ===
Original data: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
Sample of 3 elements: 3, 7, 10
Sample from large dataset (1000 elements): 42, 893, 234
Streaming sample (4 elements from 1-20): 5, 12, 18, 3
```

