# Longest Common Subsequence (LCS) Algorithm

Here's a Python implementation of the LCS algorithm using dynamic programming:

```python
def lcs_length(x, y):
    """
    Calculate the length of the longest common subsequence between two strings.
    
    Args:
        x (str): First string
        y (str): Second string
    
    Returns:
        int: Length of the longest common subsequence
    """
    m = len(x)
    n = len(y)
    
    # Create a 2D DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if x[i-1] == y[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
            else:
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])
    
    return dp[m][n]

def lcs_string(x, y):
    """
    Find the actual longest common subsequence string.
    
    Args:
        x (str): First string
        y (str): Second string
    
    Returns:
        str: The longest common subsequence
    """
    m = len(x)
    n = len(y)
    
    # Create a 2D DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if x[i-1] == y[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
            else:
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])
    
    # Reconstruct the LCS string
    lcs = []
    i, j = m, n
    
    while i > 0 and j > 0:
        if x[i-1] == y[j-1]:
            lcs.append(x[i-1])
            i -= 1
            j -= 1
        elif dp[i-1][j] > dp[i][j-1]:
            i -= 1
        else:
            j -= 1
    
    return ''.join(reversed(lcs))

# Example usage
if __name__ == "__main__":
    # Test case 1
    str1 = "ABCDGH"
    str2 = "AEDFHR"
    
    print(f"String 1: {str1}")
    print(f"String 2: {str2}")
    print(f"LCS Length: {lcs_length(str1, str2)}")
    print(f"LCS String: {lcs_string(str1, str2)}")
    print()
    
    # Test case 2
    str3 = "AGGTAB"
    str4 = "GXTXAYB"
    
    print(f"String 1: {str3}")
    print(f"String 2: {str4}")
    print(f"LCS Length: {lcs_length(str3, str4)}")
    print(f"LCS String: {lcs_string(str3, str4)}")
    print()
    
    # Test case 3 - Empty strings
    str5 = ""
    str6 = "ABC"
    
    print(f"String 1: '{str5}'")
    print(f"String 2: '{str6}'")
    print(f"LCS Length: {lcs_length(str5, str6)}")
    print(f"LCS String: '{lcs_string(str5, str6)}'")
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR
LCS Length: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
LCS Length: 4
LCS String: GTAB

String 1: ''
String 2: 'ABC'
LCS Length: 0
LCS String: ''
```

## How it works:

1. **Dynamic Programming Approach**: Creates a 2D table `dp` where `dp[i][j]` represents the length of LCS for the first `i` characters of string `x` and first `j` characters of string `y`.

2. **Recurrence Relation**:
   - If `x[i-1] == y[j-1]`: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Reconstruction**: Backtracks through the DP table to reconstruct the actual LCS string.

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

This implementation handles edge cases like empty strings and provides both the length of the LCS and the actual subsequence string.

