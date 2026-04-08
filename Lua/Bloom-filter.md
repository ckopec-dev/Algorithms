# Bloom Filter Implementation in Lua

```lua
-- Bloom Filter implementation in Lua
local BloomFilter = {}

function BloomFilter:new(capacity, error_rate)
    local obj = {
        capacity = capacity,
        error_rate = error_rate,
        bit_array = {},
        hash_functions = {}
    }
    
    -- Calculate optimal number of hash functions
    local k = math.ceil(-math.log(error_rate) / math.log(2))
    obj.k = k
    
    -- Calculate optimal bit array size
    local m = math.ceil(-capacity * math.log(error_rate) / (math.log(2)^2))
    obj.m = m
    
    -- Initialize bit array
    for i = 1, m do
        obj.bit_array[i] = false
    end
    
    -- Generate hash functions
    for i = 1, k do
        table.insert(obj.hash_functions, function(item)
            return obj:hash(item, i)
        end)
    end
    
    return setmetatable(obj, {__index = BloomFilter})
end

-- Simple hash function (djb2 algorithm)
function BloomFilter:hash(item, seed)
    local hash = 5381 + seed
    for i = 1, #item do
        hash = (hash * 33 + string.byte(item, i)) % self.m
    end
    return (hash % self.m) + 1  -- 1-based indexing
end

-- Add an item to the Bloom filter
function BloomFilter:add(item)
    for i = 1, #self.hash_functions do
        local index = self.hash_functions[i](item)
        self.bit_array[index] = true
    end
end

-- Check if an item might be in the Bloom filter
function BloomFilter:contains(item)
    for i = 1, #self.hash_functions do
        local index = self.hash_functions[i](item)
        if not self.bit_array[index] then
            return false  -- Definitely not in the set
        end
    end
    return true  -- Might be in the set (false positive possible)
end

-- Get the false positive rate
function BloomFilter:get_false_positive_rate()
    local k = self.k
    local m = self.m
    local n = self.capacity
    -- Formula: (1 - e^(-kn/m))^k
    local rate = math.pow(1 - math.exp(-k * n / m), k)
    return rate
end

-- Example usage
print("=== Bloom Filter Example ===")

-- Create a Bloom filter for 1000 items with 1% error rate
local bf = BloomFilter:new(1000, 0.01)

-- Add some items
local items = {"apple", "banana", "cherry", "date", "elderberry"}
for _, item in ipairs(items) do
    bf:add(item)
    print("Added: " .. item)
end

print("\n--- Testing items ---")

-- Test existing items
for _, item in ipairs(items) do
    if bf:contains(item) then
        print(item .. " - FOUND (might be false positive)")
    else
        print(item .. " - NOT FOUND")
    end
end

-- Test non-existing items
local non_existing = {"grape", "kiwi", "lemon", "mango"}
for _, item in ipairs(non_existing) do
    if bf:contains(item) then
        print(item .. " - FOUND (FALSE POSITIVE!)")
    else
        print(item .. " - NOT FOUND")
    end
end

print("\n--- Bloom Filter Statistics ---")
print("Capacity: " .. bf.capacity)
print("Error rate: " .. bf.error_rate)
print("Bit array size: " .. bf.m)
print("Number of hash functions: " .. bf.k)
print("Estimated false positive rate: " .. string.format("%.4f", bf:get_false_positive_rate()))
```

## How it works:

1. **Initialization**: Creates a bit array of size `m` and generates `k` hash functions
2. **Adding items**: Each item is hashed by all `k` hash functions, and the corresponding bits in the bit array are set to 1
3. **Checking membership**: For a query item, all `k` hash functions are applied, and if any bit is 0, the item is definitely not in the set. If all bits are 1, the item might be in the set (with some probability of false positive)

## Key characteristics:

- **Space efficient**: Uses only bits to represent elements
- **Fast operations**: O(k) for both add and lookup
- **False positives**: Can incorrectly report that an item exists
- **No false negatives**: If an item is not in the set, it will never be reported as present

The Bloom filter is particularly useful in scenarios like web crawlers, databases, and network applications where memory efficiency and fast lookups are important, and a small probability of false positives is acceptable.

