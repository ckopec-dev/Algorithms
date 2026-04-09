# Boyer-Moore Algorithm Implementation in Lua

Here's a complete implementation of the Boyer-Moore string searching algorithm in Lua:

```lua
-- Boyer-Moore String Search Algorithm
function boyer_moore_search(text, pattern)
    if not text or not pattern or #pattern == 0 then
        return -1
    end
    
    local text_len = #text
    local pattern_len = #pattern
    
    -- Bad character heuristic table
    local bad_char_table = {}
    
    -- Initialize bad character table with -1
    for i = 1, 256 do
        bad_char_table[i] = -1
    end
    
    -- Fill bad character table
    for i = 1, pattern_len do
        bad_char_table[string.byte(pattern, i)] = i
    end
    
    local shift = 0
    
    while shift <= text_len - pattern_len do
        local j = pattern_len
        
        -- Match from right to left
        while j > 0 and text:sub(shift + j, shift + j) == pattern:sub(j, j) do
            j = j - 1
        end
        
        -- If pattern is found
        if j == 0 then
            return shift
        end
        
        -- Calculate shift using bad character heuristic
        local bad_char_shift = j - bad_char_table[string.byte(text, shift + j)]
        shift = shift + math.max(1, bad_char_shift)
    end
    
    return -1
end

-- Enhanced version with multiple matches
function boyer_moore_all_matches(text, pattern)
    local matches = {}
    local start = 0
    
    repeat
        local pos = boyer_moore_search(text:sub(start + 1), pattern)
        if pos >= 0 then
            pos = pos + start
            table.insert(matches, pos + 1)  -- Convert to 1-based indexing
            start = pos + 1
        else
            break
        end
    until false
    
    return matches
end

-- Example usage
print("Boyer-Moore String Search Algorithm")
print("===================================")

-- Example 1: Basic search
local text1 = "ABABDABACDABABCABCABCABCABC"
local pattern1 = "ABABCABCABCABC"

print("Text: " .. text1)
print("Pattern: " .. pattern1)
local result1 = boyer_moore_search(text1, pattern1)
if result1 >= 0 then
    print("Pattern found at position: " .. result1)
else
    print("Pattern not found")
end

print()

-- Example 2: Multiple matches
local text2 = "AAAAAA"
local pattern2 = "AA"

print("Text: " .. text2)
print("Pattern: " .. pattern2)
local matches = boyer_moore_all_matches(text2, pattern2)
print("All matches at positions: " .. table.concat(matches, ", "))

print()

-- Example 3: No match
local text3 = "HELLO WORLD"
local pattern3 = "WORLD!"

print("Text: " .. text3)
print("Pattern: " .. pattern3)
local result3 = boyer_moore_search(text3, pattern3)
if result3 >= 0 then
    print("Pattern found at position: " .. result3)
else
    print("Pattern not found")
end

print()

-- Example 4: Single character pattern
local text4 = "This is a test string"
local pattern4 = "t"

print("Text: " .. text4)
print("Pattern: " .. pattern4)
local result4 = boyer_moore_search(text4, pattern4)
if result4 >= 0 then
    print("Pattern found at position: " .. result4)
else
    print("Pattern not found")
end
```

## Output:
```
Boyer-Moore String Search Algorithm
===================================
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at position: 10

Text: AAAAAA
Pattern: AA
All matches at positions: 1, 2, 3, 4, 5

Text: HELLO WORLD
Pattern: WORLD!
Pattern not found

Text: This is a test string
Pattern: t
Pattern found at position: 10
```

## Key Features:

1. **Bad Character Heuristic**: Creates a lookup table for quick character comparisons
2. **Right-to-Left Matching**: Compares characters from right to left for efficiency
3. **Multiple Match Support**: Can find all occurrences of a pattern
4. **Edge Case Handling**: Properly handles empty strings and no matches
5. **1-based Indexing**: Returns positions using 1-based indexing (like Lua strings)

## Time Complexity:
- **Best Case**: O(n/m) where n is text length and m is pattern length
- **Average Case**: O(n/m)
- **Worst Case**: O(n×m) but typically much better in practice

## Space Complexity:
- O(1) for the algorithm itself (excluding the input strings)
- O(256) for the bad character table (constant space)

The Boyer-Moore algorithm is particularly efficient for large texts and patterns, making it a popular choice for text search operations.

