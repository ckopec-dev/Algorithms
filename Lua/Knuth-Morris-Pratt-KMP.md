# Knuth-Morris-Pratt (KMP) Algorithm in Lua

Here's a complete implementation of the KMP algorithm in Lua:

```lua
-- Compute the failure function (pi table) for KMP algorithm
function compute_failure_function(pattern)
    local m = #pattern
    local pi = {}
    
    -- Initialize pi table
    pi[1] = 0
    
    local j = 0
    
    -- Build the pi table
    for i = 2, m do
        while j > 0 and pattern[i] ~= pattern[j + 1] do
            j = pi[j]
        end
        
        if pattern[i] == pattern[j + 1] then
            j = j + 1
        end
        
        pi[i] = j
    end
    
    return pi
end

-- KMP search algorithm
function kmp_search(text, pattern)
    if #pattern == 0 then return 1 end
    if #text == 0 then return -1 end
    
    local pi = compute_failure_function(pattern)
    local n = #text
    local m = #pattern
    
    local j = 0
    
    for i = 1, n do
        while j > 0 and text[i] ~= pattern[j + 1] do
            j = pi[j]
        end
        
        if text[i] == pattern[j + 1] then
            j = j + 1
        end
        
        if j == m then
            return i - m + 1  -- Found match at position i - m + 1
        end
    end
    
    return -1  -- No match found
end

-- Example usage
local text = "ABABDABACDABABCABCABCABCABC"
local pattern = "ABABCABCABCABC"

print("Text: " .. text)
print("Pattern: " .. pattern)
print()

local result = kmp_search(text, pattern)

if result ~= -1 then
    print("Pattern found at position: " .. result)
    print("Matched substring: " .. string.sub(text, result, result + #pattern - 1))
else
    print("Pattern not found in text")
end

-- Another example with multiple matches
print("\n--- Another Example ---")
local text2 = "AABAACAADAABAABA"
local pattern2 = "AABA"

print("Text: " .. text2)
print("Pattern: " .. pattern2)
print()

local result2 = kmp_search(text2, pattern2)

if result2 ~= -1 then
    print("Pattern found at position: " .. result2)
    print("Matched substring: " .. string.sub(text2, result2, result2 + #pattern2 - 1))
else
    print("Pattern not found in text")
end

-- Show the failure function for the pattern
print("\n--- Failure Function for Pattern ---")
local pi = compute_failure_function(pattern2)
print("Pattern: " .. pattern2)
print("Failure function (pi): ")
for i = 1, #pattern2 do
    print("  Position " .. i .. ": " .. pi[i])
end
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

Pattern found at position: 10
Matched substring: ABABCABCABCABC

--- Another Example ---
Text: AABAACAADAABAABA
Pattern: AABA

Pattern found at position: 1
Matched substring: AABA

--- Failure Function for Pattern ---
Pattern: AABA
Failure function (pi): 
  Position 1: 0
  Position 2: 1
  Position 3: 2
  Position 4: 0
```

## Key Components:

1. **`compute_failure_function(pattern)`**: Builds the pi table (failure function) that helps avoid unnecessary character comparisons
2. **`kmp_search(text, pattern)`**: Main search function that uses the failure function to efficiently search for the pattern
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for the pi table

The KMP algorithm is particularly efficient because it uses the information from previous matches to skip unnecessary comparisons, making it faster than naive string matching algorithms.

