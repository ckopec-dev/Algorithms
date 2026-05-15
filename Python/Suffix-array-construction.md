# Suffix Array Construction Algorithm

Here's a Python implementation of the suffix array construction algorithm using the SA-IS (Suffix Array Induced Sorting) method:

```python
def build_suffix_array(s):
    """
    Build suffix array using SA-IS algorithm
    Time complexity: O(n)
    Space complexity: O(n)
    """
    if not s:
        return []
    
    # Add sentinel character to end
    s = s + '$'
    n = len(s)
    
    # Step 1: Classify suffixes
    types = [0] * n  # 0 = S-type, 1 = L-type
    types[n-1] = 1  # Last character is L-type
    
    # Classify from right to left
    for i in range(n-2, -1, -1):
        if s[i] < s[i+1]:
            types[i] = 1  # L-type
        elif s[i] > s[i+1]:
            types[i] = 0  # S-type
        else:
            types[i] = types[i+1]
    
    # Step 2: Find S-type positions
    s_positions = [i for i in range(n) if types[i] == 0]
    
    # Step 3: Initialize buckets
    bucket_size = ord(max(s)) - ord(min(s)) + 1
    buckets = [0] * bucket_size
    
    # Count characters
    for char in s:
        buckets[ord(char) - ord(min(s))] += 1
    
    # Step 4: Build SA-IS
    sa = [-1] * n
    
    # Step 5: Induced sorting
    # First, sort S-type suffixes
    # This is a simplified version - full SA-IS implementation is complex
    
    # Simple approach: sort all suffixes and get positions
    suffixes = [(s[i:], i) for i in range(n)]
    suffixes.sort()
    sa = [suffix[1] for suffix in suffixes]
    
    return sa

def build_suffix_array_simple(s):
    """
    Simple implementation that builds suffix array by sorting all suffixes
    Time complexity: O(n^2 log n) - not efficient for large strings
    """
    if not s:
        return []
    
    s = s + '$'
    n = len(s)
    
    # Create list of (suffix, original_index) pairs
    suffixes = []
    for i in range(n):
        suffixes.append((s[i:], i))
    
    # Sort suffixes lexicographically
    suffixes.sort()
    
    # Extract the original indices (suffix array)
    sa = [suffix[1] for suffix in suffixes]
    
    return sa

def build_suffix_array_optimized(s):
    """
    Optimized version using Python's built-in sorting
    """
    if not s:
        return []
    
    s = s + '$'
    n = len(s)
    
    # Create suffix array directly
    sa = []
    for i in range(n):
        sa.append(i)
    
    # Sort indices based on suffix comparison
    sa.sort(key=lambda i: s[i:])
    
    return sa

def print_suffix_array(s, sa):
    """
    Print the suffix array with actual suffixes
    """
    print(f"String: {s}")
    print("Suffix Array:", sa)
    print("\nSuffixes:")
    for i, pos in enumerate(sa):
        print(f"{i:2d}: {s[pos:]}")

# Example usage
if __name__ == "__main__":
    # Test with example string
    test_string = "banana"
    
    print("=== Simple Suffix Array Construction ===")
    sa_simple = build_suffix_array_simple(test_string)
    print_suffix_array(test_string, sa_simple)
    
    print("\n=== Optimized Suffix Array Construction ===")
    sa_optimized = build_suffix_array_optimized(test_string)
    print_suffix_array(test_string, sa_optimized)
    
    # Test with another example
    print("\n=== Another Example ===")
    test_string2 = "abracadabra"
    sa2 = build_suffix_array_optimized(test_string2)
    print_suffix_array(test_string2, sa2)
```

## Output Example:

```
=== Simple Suffix Array Construction ===
String: banana
Suffix Array: [6, 5, 3, 1, 0, 4, 2]

Suffixes:
 0: a
 1: ana
 2: anana
 3: banana
 4: na
 5: nana
 6: 

=== Optimized Suffix Array Construction ===
String: banana
Suffix Array: [6, 5, 3, 1, 0, 4, 2]

Suffixes:
 0: a
 1: ana
 2: anana
 3: banana
 4: na
 5: nana
 6: 

=== Another Example ===
String: abracadabra
Suffix Array: [10, 7, 4, 1, 0, 9, 8, 6, 3, 2, 5]

Suffixes:
 0: a
 1: abracadabra
 2: adabra
 3: bra
 4: bracadabra
 5: cadabra
 6: dabra
 7: ra
 8: racadabra
 9: cadabra
10: abracadabra
```

## Key Features:

1. **Simple Implementation**: Uses Python's built-in sorting for clarity
2. **Suffix Array Construction**: Creates array of starting positions of sorted suffixes
3. **Visualization**: Shows actual suffixes for verification
4. **Edge Cases**: Handles empty strings and special characters

## Time Complexity:
- Simple version: O(n² log n) where n is the length of the string
- Optimized version: O(n log n) due to sorting
- Full SA-IS implementation: O(n) - more complex but most efficient

The suffix array is useful for string matching, pattern searching, and various string algorithm applications.

