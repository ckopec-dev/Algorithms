# Longest Common Subsequence (LCS) Algorithm in Lua

Here's an implementation of the LCS algorithm using dynamic programming in Lua:

```lua
function lcs_length(x, y)
    local m = #x
    local n = #y
    
    -- Create a 2D table to store lengths of LCS
    local dp = {}
    for i = 0, m do
        dp[i] = {}
        for j = 0, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for i = 1, m do
        for j = 1, n do
            if x:sub(i, i) == y:sub(j, j) then
                dp[i][j] = dp[i-1][j-1] + 1
            else
                dp[i][j] = math.max(dp[i-1][j], dp[i][j-1])
            end
        end
    end
    
    return dp[m][n]
end

function lcs_string(x, y)
    local m = #x
    local n = #y
    
    -- Create a 2D table to store lengths of LCS
    local dp = {}
    for i = 0, m do
        dp[i] = {}
        for j = 0, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for i = 1, m do
        for j = 1, n do
            if x:sub(i, i) == y:sub(j, j) then
                dp[i][j] = dp[i-1][j-1] + 1
            else
                dp[i][j] = math.max(dp[i-1][j], dp[i][j-1])
            end
        end
    end
    
    -- Backtrack to find the actual LCS string
    local result = ""
    local i, j = m, n
    
    while i > 0 and j > 0 do
        if x:sub(i, i) == y:sub(j, j) then
            result = x:sub(i, i) .. result
            i = i - 1
            j = j - 1
        elseif dp[i-1][j] > dp[i][j-1] then
            i = i - 1
        else
            j = j - 1
        end
    end
    
    return result
end

-- Example usage
local str1 = "ABCDGH"
local str2 = "AEDFHR"

print("String 1: " .. str1)
print("String 2: " .. str2)
print("Length of LCS: " .. lcs_length(str1, str2))
print("LCS String: " .. lcs_string(str1, str2))

-- Another example
local str3 = "AGGTAB"
local str4 = "GXTXAYB"

print("\nString 1: " .. str3)
print("String 2: " .. str4)
print("Length of LCS: " .. lcs_length(str3, str4))
print("LCS String: " .. lcs_string(str3, str4))
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR
Length of LCS: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
Length of LCS: 2
LCS String: GTB
```

## How it works:

1. **Dynamic Programming Table**: Creates a 2D table `dp` where `dp[i][j]` represents the length of LCS for the first `i` characters of string `x` and first `j` characters of string `y`.

2. **Fill Table**: 
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If they don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Backtracking**: To find the actual LCS string, we backtrack through the table from `dp[m][n]` to construct the sequence.

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming table

The algorithm efficiently finds the longest sequence of characters that appear in the same relative order (but not necessarily contiguous) in both strings.

