# Damerau-Levenshtein Distance Algorithm

The Damerau-Levenshtein distance is a string metric for measuring the difference between two sequences. It is similar to the Levenshtein distance, but also considers transposition of adjacent characters as a single operation.

## Python Implementation

```python
def damerau_levenshtein_distance(str1, str2):
    """
    Calculate the Damerau-Levenshtein distance between two strings.
    
    Args:
        str1 (str): First string
        str2 (str): Second string
    
    Returns:
        int: The Damerau-Levenshtein distance
    """
    # Get the length of both strings
    len1, len2 = len(str1), len(str2)
    
    # Create a matrix to store distances
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
            if str1[i-1] == str2[j-1]:
                cost = 0
            else:
                cost = 1
            
            # Calculate the minimum cost of three operations:
            # 1. Deletion: dp[i-1][j] + 1
            # 2. Insertion: dp[i][j-1] + 1
            # 3. Substitution: dp[i-1][j-1] + cost
            
            dp[i][j] = min(
                dp[i-1][j] + 1,        # deletion
                dp[i][j-1] + 1,        # insertion
                dp[i-1][j-1] + cost    # substitution
            )
            
            # Check for transposition (adjacent characters)
            if (i > 1 and j > 1 and 
                str1[i-1] == str2[j-2] and 
                str1[i-2] == str2[j-1]):
                
                dp[i][j] = min(dp[i][j], dp[i-2][j-2] + cost)
    
    return dp[len1][len2]

# Example usage
def main():
    # Test cases
    test_cases = [
        ("kitten", "sitting"),
        ("saturday", "sunday"),
        ("hello", "world"),
        ("abc", "acb"),
        ("", "abc"),
        ("same", "same")
    ]
    
    print("Damerau-Levenshtein Distance Examples:")
    print("=" * 40)
    
    for str1, str2 in test_cases:
        distance = damerau_levenshtein_distance(str1, str2)
        print(f"'{str1}' -> '{str2}': {distance}")

# Run the examples
if __name__ == "__main__":
    main()
```

## Output

```
Damerau-Levenshtein Distance Examples:
========================================
'kitten' -> 'sitting': 3
'saturday' -> 'sunday': 3
'hello' -> 'world': 4
'abc' -> 'acb': 1
'' -> 'abc': 3
'same' -> 'same': 0
```

## Explanation of Key Concepts

### How it works:
1. **Base operations**: Deletion, insertion, and substitution (like Levenshtein)
2. **Additional operation**: Transposition (swapping adjacent characters)
3. **Matrix approach**: Uses dynamic programming to build up the solution
4. **Transposition check**: When characters at positions (i-1,j-2) and (i-2,j-1) are swapped

### Time Complexity: O(m×n)
### Space Complexity: O(m×n)

Where m and n are the lengths of the two input strings.

The algorithm correctly identifies that "kitten" to "sitting" requires 3 operations, including one transposition (the 'k' and 's' can be swapped in the final result), making it more efficient than standard Levenshtein distance for certain types of string comparison.

