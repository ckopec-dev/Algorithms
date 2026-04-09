# Levenshtein Distance Algorithm in Python

The Levenshtein distance is a string metric that measures the difference between two sequences. It represents the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

## Implementation

```python
def levenshtein_distance(str1, str2):
    """
    Calculate the Levenshtein distance between two strings.
    
    Args:
        str1 (str): First string
        str2 (str): Second string
    
    Returns:
        int: The Levenshtein distance between the two strings
    """
    # Get the lengths of both strings
    len1, len2 = len(str1), len(str2)
    
    # Create a matrix to store the distances
    # Initialize with zeros
    dp = [[0 for _ in range(len2 + 1)] for _ in range(len1 + 1)]
    
    # Fill the first row and column with sequential numbers
    for i in range(len1 + 1):
        dp[i][0] = i
    
    for j in range(len2 + 1):
        dp[0][j] = j
    
    # Fill the matrix using dynamic programming
    for i in range(1, len1 + 1):
        for j in range(1, len2 + 1):
            if str1[i-1] == str2[j-1]:
                # Characters match, no operation needed
                dp[i][j] = dp[i-1][j-1]
            else:
                # Take minimum of three operations: insert, delete, substitute
                dp[i][j] = 1 + min(
                    dp[i-1][j],    # deletion
                    dp[i][j-1],    # insertion
                    dp[i-1][j-1]   # substitution
                )
    
    return dp[len1][len2]

# Example usage
if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("kitten", "sitting"),
        ("saturday", "sunday"),
        ("hello", "world"),
        ("", "abc"),
        ("abc", ""),
        ("same", "same")
    ]
    
    print("Levenshtein Distance Examples:")
    print("=" * 40)
    
    for str1, str2 in test_cases:
        distance = levenshtein_distance(str1, str2)
        print(f"'{str1}' -> '{str2}': {distance}")
```

## Output
```
Levenshtein Distance Examples:
========================================
'kitten' -> 'sitting': 3
'saturday' -> 'sunday': 3
'hello' -> 'world': 4
'' -> 'abc': 3
'abc' -> '': 3
'same' -> 'same': 0
```

## How it works

1. **Initialization**: Create a matrix where `dp[i][j]` represents the distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base cases**: Fill the first row and column with sequential numbers (0, 1, 2, ...).

3. **Dynamic programming**: For each cell `dp[i][j]`:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

4. **Result**: The bottom-right cell contains the Levenshtein distance.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

This implementation efficiently calculates the minimum edit distance between any two strings using dynamic programming principles.

