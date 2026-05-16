# Levenshtein Distance Algorithm in Lua

Here's an implementation of the Levenshtein distance algorithm in Lua:

```lua
function levenshtein_distance(str1, str2)
    local len1 = #str1
    local len2 = #str2
    
    -- Create a matrix to store distances
    local matrix = {}
    
    -- Initialize the matrix
    for i = 0, len1 do
        matrix[i] = {}
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
        end
    end
    
    return matrix[len1][len2]
end

-- Example usage
print("Levenshtein Distance Examples:")
print("Distance between 'kitten' and 'sitting': " .. levenshtein_distance("kitten", "sitting"))
print("Distance between 'hello' and 'world': " .. levenshtein_distance("hello", "world"))
print("Distance between 'lua' and 'lua': " .. levenshtein_distance("lua", "lua"))
print("Distance between 'algorithm' and 'logarithm': " .. levenshtein_distance("algorithm", "logarithm"))
```

## Output:
```
Levenshtein Distance Examples:
Distance between 'kitten' and 'sitting': 3
Distance between 'hello' and 'world': 4
Distance between 'lua' and 'lua': 0
Distance between 'algorithm' and 'logarithm': 3
```

## How it works:

1. **Initialization**: Create a matrix where `matrix[i][j]` represents the edit distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base cases**: 
   - The distance from an empty string to a string of length `j` is `j` (all insertions)
   - The distance from a string of length `i` to an empty string is `i` (all deletions)

3. **Recurrence relation**: For each cell `matrix[i][j]`, calculate the minimum of:
   - Deletion: `matrix[i-1][j] + 1`
   - Insertion: `matrix[i][j-1] + 1`  
   - Substitution: `matrix[i-1][j-1] + cost` (where cost is 0 if characters match, 1 otherwise)

4. **Result**: The value at `matrix[len1][len2]` is the Levenshtein distance.

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n), where m and n are the lengths of the input strings.

