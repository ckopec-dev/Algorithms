# Bitap Algorithm Implementation in Lua

The Bitap algorithm is a fast string searching algorithm that uses bit manipulation to find occurrences of a pattern in a text.

```lua
-- Bitap Algorithm Implementation in Lua

function bitap_search(text, pattern)
    local m = #pattern
    local n = #text
    
    -- If pattern is empty, return 0 (found at position 0)
    if m == 0 then
        return 0
    end
    
    -- If text is empty or pattern is longer than text, return -1 (not found)
    if n == 0 or m > n then
        return -1
    end
    
    -- Create a lookup table for pattern characters
    local lookup = {}
    for i = 1, m do
        local char = pattern:sub(i, i)
        lookup[char] = bit.bor(lookup[char] or 0, bit.lshift(1, m - i))
    end
    
    -- Initialize the bit array (0x0000...0001)
    local R = bit.lshift(1, m) - 1
    
    -- Process each character in the text
    for i = 1, n do
        local char = text:sub(i, i)
        local mask = lookup[char] or 0
        
        -- Update the bit array
        R = bit.band(bit.bor(R, mask), bit.lshift(1, m) - 1)
        
        -- Check if we found a complete match
        if bit.band(R, bit.lshift(1, m - 1)) == 0 then
            return i - m  -- Return 0-based index of match
        end
    end
    
    return -1  -- Not found
end

-- Enhanced version that allows for error tolerance (differences)
function bitap_search_with_errors(text, pattern, max_errors)
    local m = #pattern
    local n = #text
    
    if m == 0 then
        return 0
    end
    
    if n == 0 or m > n then
        return -1
    end
    
    -- Create lookup table
    local lookup = {}
    for i = 1, m do
        local char = pattern:sub(i, i)
        lookup[char] = bit.bor(lookup[char] or 0, bit.lshift(1, m - i))
    end
    
    -- Initialize arrays for error tracking
    local R = {}
    for i = 0, max_errors do
        R[i] = bit.lshift(1, m) - 1
    end
    
    -- Process each character in text
    for i = 1, n do
        local char = text:sub(i, i)
        local mask = lookup[char] or 0
        
        -- Update the bit arrays for each error level
        for error_count = 0, max_errors do
            local old_R = R[error_count]
            R[error_count] = bit.band(bit.bor(old_R, mask), bit.lshift(1, m) - 1)
            
            if error_count > 0 then
                -- For error tracking, we need to consider previous states
                R[error_count] = bit.bor(R[error_count], bit.rshift(old_R, 1))
            end
        end
        
        -- Check if we found a match with at most max_errors errors
        if bit.band(R[max_errors], bit.lshift(1, m - 1)) == 0 then
            return i - m  -- Return 0-based index
        end
    end
    
    return -1
end

-- Example usage
print("=== Bitap Algorithm Examples ===")

-- Basic search
local text1 = "Hello, this is a sample text for testing"
local pattern1 = "sample"
local result1 = bitap_search(text1, pattern1)
print("Text: " .. text1)
print("Pattern: " .. pattern1)
print("Result: Position " .. (result1 == -1 and "Not found" or result1))

-- Another example
local text2 = "The quick brown fox jumps over the lazy dog"
local pattern2 = "fox"
local result2 = bitap_search(text2, pattern2)
print("\nText: " .. text2)
print("Pattern: " .. pattern2)
print("Result: Position " .. (result2 == -1 and "Not found" or result2))

-- Test with non-existent pattern
local text3 = "abcdefg"
local pattern3 = "xyz"
local result3 = bitap_search(text3, pattern3)
print("\nText: " .. text3)
print("Pattern: " .. pattern3)
print("Result: " .. (result3 == -1 and "Not found" or "Position " .. result3))

-- Test with empty pattern
local text4 = "hello world"
local pattern4 = ""
local result4 = bitap_search(text4, pattern4)
print("\nText: " .. text4)
print("Pattern: " .. pattern4)
print("Result: Position " .. (result4 == -1 and "Not found" or result4))

-- Test with single character pattern
local text5 = "programming"
local pattern5 = "g"
local result5 = bitap_search(text5, pattern5)
print("\nText: " .. text5)
print("Pattern: " .. pattern5)
print("Result: Position " .. (result5 == -1 and "Not found" or result5))
```

## How it works:

1. **Bit Manipulation**: The algorithm uses bitwise operations to represent character positions in the pattern
2. **Lookup Table**: Creates a mapping of characters to their bit positions in the pattern
3. **Bit Array**: Maintains a bit array that represents possible matches
4. **Character Processing**: For each character in the text, it updates the bit array
5. **Match Detection**: When a complete match is found (bit at position m-1 is 0), it returns the position

## Key Features:

- **Time Complexity**: O(n × m) where n is text length and m is pattern length
- **Space Complexity**: O(m) for the lookup table and bit array
- **Efficient**: Uses bit operations for fast comparisons
- **Flexible**: Can be extended to handle approximate matching with errors

The algorithm is particularly useful for searching in large texts where performance is critical, as it can be faster than traditional string matching algorithms in many cases.

