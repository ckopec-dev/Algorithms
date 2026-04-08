# Cuckoo Hashing Algorithm in Lua

```lua
-- Cuckoo Hashing Implementation in Lua

local CuckooHash = {}
CuckooHash.__index = CuckooHash

function CuckooHash.new(size)
    local self = setmetatable({}, CuckooHash)
    self.size = size
    self.table1 = {}
    self.table2 = {}
    self.max_iterations = 1000  -- Prevent infinite loops
    return self
end

-- Simple hash functions
function CuckooHash:hash1(key)
    local hash = 0
    for i = 1, #key do
        hash = hash + string.byte(key, i) * i
    end
    return (hash % self.size) + 1
end

function CuckooHash:hash2(key)
    local hash = 0
    for i = 1, #key do
        hash = hash + string.byte(key, i) * (i * 31)
    end
    return (hash % self.size) + 1
end

-- Insert a key-value pair
function CuckooHash:insert(key, value)
    local current_key = key
    local current_value = value
    local iteration = 0
    
    -- Try to insert into table1 first
    local table1_index = self:hash1(current_key)
    local table2_index = self:hash2(current_key)
    
    -- Check if key already exists
    if self.table1[table1_index] and self.table1[table1_index].key == current_key then
        self.table1[table1_index].value = current_value
        return true
    end
    
    if self.table2[table2_index] and self.table2[table2_index].key == current_key then
        self.table2[table2_index].value = current_value
        return true
    end
    
    -- Start cuckoo insertion process
    while iteration < self.max_iterations do
        -- Try table1
        if not self.table1[table1_index] then
            self.table1[table1_index] = {key = current_key, value = current_value}
            return true
        else
            -- Evict existing element and try to place it in table2
            local old_key = self.table1[table1_index].key
            local old_value = self.table1[table1_index].value
            self.table1[table1_index] = {key = current_key, value = current_value}
            
            current_key = old_key
            current_value = old_value
            
            -- Try to place the evicted element in table2
            table2_index = self:hash2(current_key)
            
            if not self.table2[table2_index] then
                self.table2[table2_index] = {key = current_key, value = current_value}
                return true
            else
                -- Evict from table2 and continue
                local old_key2 = self.table2[table2_index].key
                local old_value2 = self.table2[table2_index].value
                self.table2[table2_index] = {key = current_key, value = current_value}
                
                current_key = old_key2
                current_value = old_value2
                
                -- Try to place in table1 again
                table1_index = self:hash1(current_key)
                iteration = iteration + 1
            end
        end
    end
    
    -- If we get here, we couldn't insert (likely due to cycle)
    error("Cuckoo hashing failed - possible cycle detected")
end

-- Find a value by key
function CuckooHash:find(key)
    local index1 = self:hash1(key)
    local index2 = self:hash2(key)
    
    if self.table1[index1] and self.table1[index1].key == key then
        return self.table1[index1].value
    end
    
    if self.table2[index2] and self.table2[index2].key == key then
        return self.table2[index2].value
    end
    
    return nil
end

-- Remove a key-value pair
function CuckooHash:remove(key)
    local index1 = self:hash1(key)
    local index2 = self:hash2(key)
    
    if self.table1[index1] and self.table1[index1].key == key then
        self.table1[index1] = nil
        return true
    end
    
    if self.table2[index2] and self.table2[index2].key == key then
        self.table2[index2] = nil
        return true
    end
    
    return false
end

-- Print the hash tables
function CuckooHash:print()
    print("Table 1:")
    for i, v in pairs(self.table1) do
        print("  [" .. i .. "] = " .. v.key .. " => " .. v.value)
    end
    
    print("Table 2:")
    for i, v in pairs(self.table2) do
        print("  [" .. i .. "] = " .. v.key .. " => " .. v.value)
    end
end

-- Example usage
print("=== Cuckoo Hashing Example ===")

local cuckoo = CuckooHash.new(10)

-- Insert some values
cuckoo:insert("apple", 5)
cuckoo:insert("banana", 3)
cuckoo:insert("cherry", 8)
cuckoo:insert("date", 2)
cuckoo:insert("elderberry", 7)

print("After inserting 5 items:")
cuckoo:print()

-- Find values
print("\nFind operations:")
print("apple: " .. tostring(cuckoo:find("apple")))
print("banana: " .. tostring(cuckoo:find("banana")))
print("grape: " .. tostring(cuckoo:find("grape")))

-- Update a value
cuckoo:insert("apple", 10)
print("\nAfter updating apple:")
print("apple: " .. tostring(cuckoo:find("apple")))

-- Remove an item
cuckoo:remove("banana")
print("\nAfter removing banana:")
cuckoo:print()

-- Insert more items to demonstrate cuckoo process
cuckoo:insert("fig", 4)
cuckoo:insert("grape", 6)
cuckoo:insert("honeydew", 9)

print("\nAfter inserting more items:")
cuckoo:print()
```

## Key Features of this Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store elements
2. **Two Hash Functions**: Implements two different hash functions for better distribution
3. **Cuckoo Eviction**: When a collision occurs, it evicts the existing element and tries to place it elsewhere
4. **Cycle Detection**: Includes a maximum iteration limit to prevent infinite loops
5. **Basic Operations**: Supports insert, find, and remove operations

## How Cuckoo Hashing Works:

1. **Insertion**: Hash a key to both tables using two different hash functions
2. **Collision Resolution**: If a slot is empty, insert the element there; otherwise, evict the existing element and try to place it in the other table
3. **Cuckoo Process**: Continue the eviction process until either the element is placed or a cycle is detected
4. **Lookup**: Search in both tables using the same hash functions
5. **Deletion**: Remove elements from the appropriate table

This implementation demonstrates the core principles of cuckoo hashing with a simple, readable Lua syntax.

