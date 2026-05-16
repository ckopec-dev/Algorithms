# Lempel-Ziv Algorithm Implementation in Lua

Here's a complete implementation of the Lempel-Ziv compression algorithm in Lua:

```lua
-- Lempel-Ziv Compression Algorithm Implementation
-- This is a simplified version of LZ77 algorithm

local LZ = {}

-- Compress function using LZ77 algorithm
function LZ.compress(text)
    local dict = {}
    local result = {}
    local pos = 1
    local dict_size = 256  -- Starting dictionary size
    
    -- Initialize dictionary with ASCII characters
    for i = 0, 255 do
        dict[string.char(i)] = i
    end
    
    while pos <= #text do
        local match_length = 0
        local match_pos = 0
        local search_pos = 1
        local search_len = math.min(1000, pos - 1)  -- Limit search length
        
        -- Find longest match in dictionary
        for i = 1, search_len do
            local substr = text:sub(pos - i, pos - 1)
            if dict[substr] then
                if #substr > match_length then
                    match_length = #substr
                    match_pos = dict[substr]
                end
            end
        end
        
        -- Add to dictionary if we found a match
        if match_length > 0 then
            table.insert(result, {match_pos, match_length, text:sub(pos + match_length, pos + match_length)})
            pos = pos + match_length + 1
        else
            -- No match found, output literal character
            table.insert(result, {0, 0, text:sub(pos, pos)})
            pos = pos + 1
        end
    end
    
    return result
end

-- Decompress function
function LZ.decompress(compressed_data)
    local dict = {}
    local result = {}
    
    -- Initialize dictionary with ASCII characters
    for i = 0, 255 do
        dict[i] = string.char(i)
    end
    
    local dict_size = 256
    
    for _, entry in ipairs(compressed_data) do
        local pos, length, char = entry[1], entry[2], entry[3]
        
        if pos == 0 then
            -- Literal character
            table.insert(result, char)
            dict[dict_size] = char
            dict_size = dict_size + 1
        else
            -- Match from dictionary
            local match = dict[pos]
            if match then
                local new_string = match .. char
                table.insert(result, new_string)
                dict[dict_size] = new_string
                dict_size = dict_size + 1
            else
                -- Handle edge case
                table.insert(result, char)
            end
        end
    end
    
    return table.concat(result)
end

-- Simple LZ77 implementation with fixed dictionary
function LZ.compress_simple(text)
    local result = {}
    local dict_size = 256
    local dict = {}
    
    -- Initialize dictionary
    for i = 0, 255 do
        dict[string.char(i)] = i
    end
    
    local i = 1
    while i <= #text do
        local max_len = 0
        local max_pos = 0
        local found = false
        
        -- Search for longest match
        for j = 1, math.min(i - 1, 1000) do  -- Limit search window
            local substr = text:sub(i - j, i - 1)
            if dict[substr] and #substr > max_len then
                max_len = #substr
                max_pos = dict[substr]
                found = true
            end
        end
        
        if found and max_len > 0 then
            -- Add match tuple (position, length, next_char)
            local next_char = text:sub(i + max_len, i + max_len)
            table.insert(result, {max_pos, max_len, next_char})
            i = i + max_len + 1
        else
            -- Add literal
            table.insert(result, {0, 0, text:sub(i, i)})
            i = i + 1
        end
    end
    
    return result
end

-- Example usage
print("=== Lempel-Ziv Compression Example ===")

-- Test string
local test_string = "ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB

