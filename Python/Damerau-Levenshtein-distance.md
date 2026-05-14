# Damerau-Levenshtein Distance Algorithm in Python

The Damerau-Levenshtein distance is a string metric that measures the minimum number of single-character edits (insertions, deletions, substitutions, or transpositions) required to change one word into another.

## Implementation

```python
def damerau_levenshtein_distance(s1, s2):
    """
    Calculate the Damerau-Levenshtein distance between two strings.
    
    Args:
        s1 (str): First string
        s2 (str): Second string
    
    Returns:
        int: The Damerau-Levenshtein distance
    """
    # Get the length of both strings
    len1, len2 = len(s1), len(s2)
    
    # Create a matrix to store the distances
    # Initialize with zeros
    dp = [[0 for _ in range(len2 + 1)] for _ in range(len1 + 1)]
    
    # Initialize the first row and column
    for i in range(len1 + 1):
        dp[i][0] = i
    
    for j in range(len2 + 1):
        dp[0][j] = j
    
    # Fill the matrix
    for i in range(1, len1 + 1):
        for j in range(1, len2 + 1):
            # If characters are the same, no operation needed
            if s1[i-1] == s2[j-1]:
                cost = 0
            else:
                cost = 1
            
            # Calculate the minimum cost of three operations
            dp[i][j] = min(
                dp[i-1][j] + 1,        # deletion
                dp[i][j-1] + 1,        # insertion
                dp[i-1][j-1] + cost    # substitution
            )
            
            # Check for transposition (if possible)
            if (i > 1 and j > 1 and 
                s1[i-1] == s2[j-2] and 
                s1[i-2] == s2[j-1]):
                dp[i][j] = min(dp[i][j], dp[i-2][j-2] + 1)
    
    return dp[len1][len2]

# Example usage
if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("kitten", "sitting"),
        ("saturday", "sunday"),
        ("hello", "world"),
        ("python", "python"),
        ("abc", "acb"),
        ("", "abc"),
        ("abc", "")
    ]
    
    print("Damerau-Levenshtein Distance Examples:")
    print("=" * 40)
    
    for s1, s2 in test_cases:
        distance = damerau_levenshtein_distance(s1, s2)
        print(f"'{s1}' -> '{s2}': {distance}")
```

## Output

```
Damerau-Levenshtein Distance Examples:
========================================
'kitten' -> 'sitting': 3
'saturday' -> 'sunday': 3
'hello' -> 'world': 4
'python' -> 'python': 0
'abc' -> 'acb': 1
'' -> 'abc': 3
'abc' -> '': 3
```

## Explanation of Examples

1. **"kitten" → "sitting"**: 3 operations
   - kitten → sitten (substitute 'k' with 's')
   - sitten → sittin (substitute 'e' with 'i')
   - sittin → sitting (insert 'g')

2. **"abc" → "acb"**: 1 operation
   - Transpose 'b' and 'c'

3. **"python" → "python"**: 0 operations
   - Strings are identical

## Key Features

- **Transposition support**: Unlike standard Levenshtein distance, this algorithm includes transposition as a valid operation
- **Time complexity**: O(m×n) where m and n are the lengths of the strings
- **Space complexity**: O(m×n) for the dynamic programming matrix
- **Use cases**: Spell checkers, DNA sequence analysis, fuzzy string matching

This implementation correctly handles all four basic operations: insertion, deletion, substitution, and transposition.

