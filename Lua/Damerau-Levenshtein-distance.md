# Damerau-Levenshtein Distance in Lua

Here's an implementation of the Damerau-Levenshtein distance algorithm in Lua:

```lua
function damerau_levenshtein_distance(str1, str2)
    local len1 = #str1
    local len2 = #str2
    
    -- Create a matrix to store distances
    local matrix = {}
    for i = 0, len1 do
        matrix[i] = {}
        for j = 0, len2 do
            matrix[i][j] = 0
        end
    end
    
    -- Initialize base cases
    for i = 0, len1 do
        matrix[i][0] = i
    end
    
    for j = 0, len2 do
        matrix[0][j] = j
    end
    
    -- Fill the matrix
    for i = 1, len1 do
        for j = 1, len2 do
            local cost = (str1:sub(i, i) == str2:sub(j, j)) and 0 or 1
            
            matrix[i][j] = math.min(
                matrix[i-1][j] + 1,        -- deletion
                matrix[i][j-1] + 1,        -- insertion
                matrix[i-1][j-1] + cost    -- substitution
            )
            
            -- Check for transposition (Damerau-Levenshtein specific)
            if i > 1 and j > 1 and 
               str1:sub(i, i) == str2:sub(j-1, j-1) and 
               str1:sub(i-1, i-1) == str2:sub(j, j) then
                matrix[i][j] = math.min(matrix[i][j], matrix[i-2][j-2] + cost)
            end
        end
    end
    
    return matrix[len1][len2]
end

-- Example usage
print("Damerau-Levenshtein Distance Examples:")
print("=====================================")

-- Example 1: Basic difference
local str1 = "kitten"
local str2 = "sitting"
print(string.format("'%s' -> '%s': %d", str1, str2, damerau_levenshtein_distance(str1, str2)))

-- Example 2: With transposition
local str3 = "cat"
local str4 = "cta"
print(string.format("'%s' -> '%s': %d", str3, str4, damerau_levenshtein_distance(str3, str4)))

-- Example 3: With multiple operations
local str5 = "hello"
local str6 = "hallo"
print(string.format("'%s' -> '%s': %d", str5, str6, damerau_levenshtein_distance(str5, str6)))

-- Example 4: Identical strings
local str7 = "test"
local str8 = "test"
print(string.format("'%s' -> '%s': %d", str7, str8, damerau_levenshtein_distance(str7, str8)))

-- Example 5: Empty strings
local str9 = ""
local str10 = "abc"
print(string.format("'%s' -> '%s': %d", str9, str10, damerau_levenshtein_distance(str9, str10)))
```

## Output:
```
Damerau-Levenshtein Distance Examples:
=====================================
'kitten' -> 'sitting': 3
'cat' -> 'cta': 1
'hello' -> 'hallo': 1
'test' -> 'test': 0
'' -> 'abc': 3
```

## How it works:

1. **Matrix Initialization**: Creates a 2D matrix where `matrix[i][j]` represents the distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base Cases**: 
   - First row: cost of inserting characters
   - First column: cost of deleting characters

3. **Dynamic Programming**: For each cell, calculates the minimum cost of:
   - Deletion: `matrix[i-1][j] + 1`
   - Insertion: `matrix[i][j-1] + 1`
   - Substitution: `matrix[i-1][j-1] + cost`
   - Transposition: `matrix[i-2][j-2] + cost` (Damerau-Levenshtein specific)

4. **Transposition Check**: Special case where two adjacent characters are swapped in one operation.

The algorithm handles all four operations: insertion, deletion, substitution, and transposition, making it more suitable for spell checking and fuzzy string matching than the standard Levenshtein distance.

