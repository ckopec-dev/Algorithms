# Boyer-Moore Algorithm Implementation in Python

The Boyer-Moore algorithm is a string searching algorithm that is more efficient than naive string matching for large texts.

## Implementation

```python
def boyer_moore_search(text, pattern):
    """
    Boyer-Moore string search algorithm
    
    Args:
        text (str): The text to search in
        pattern (str): The pattern to search for
    
    Returns:
        int: Index of first occurrence of pattern in text, or -1 if not found
    """
    
    def build_bad_char_table(pattern):
        """Build bad character table for Boyer-Moore"""
        table = {}
        for i in range(len(pattern)):
            table[pattern[i]] = i
        return table
    
    def build_good_suffix_table(pattern):
        """Build good suffix table for Boyer-Moore"""
        m = len(pattern)
        table = [0] * m
        
        # Build the suffix array
        suffix = [0] * m
        suffix[m-1] = m
        
        i = m - 2
        while i >= 0:
            j = i
            while j >= 0 and pattern[j] == pattern[m-1-(i-j)]:
                j -= 1
            suffix[i] = i - j
            i -= 1
        
        # Build the good suffix table
        for i in range(m):
            table[i] = m
        
        j = 0
        for i in range(m-1, -1, -1):
            if suffix[i] == i + 1:
                while j < m - 1 - i:
                    if table[j] == m:
                        table[j] = m - 1 - i
                    j += 1
        
        for i in range(m-1):
            table[m-1-suffix[i]] = m-1-i
        
        return table
    
    if not pattern or not text:
        return -1
    
    if len(pattern) > len(text):
        return -1
    
    # Build tables
    bad_char_table = build_bad_char_table(pattern)
    good_suffix_table = build_good_suffix_table(pattern)
    
    # Search
    i = 0
    while i <= len(text) - len(pattern):
        j = len(pattern) - 1
        
        # Match from right to left
        while j >= 0 and pattern[j] == text[i + j]:
            j -= 1
        
        if j < 0:
            return i  # Found match
        
        # Calculate shift using bad character rule
        bad_char_shift = j - bad_char_table.get(text[i + j], -1)
        
        # Calculate shift using good suffix rule
        good_suffix_shift = good_suffix_table[j]
        
        # Shift by maximum of both rules
        i += max(bad_char_shift, good_suffix_shift)
    
    return -1

# Simple version (only bad character heuristic)
def boyer_moore_simple(text, pattern):
    """
    Simplified Boyer-Moore using only bad character heuristic
    """
    if not pattern or not text:
        return -1
    
    if len(pattern) > len(text):
        return -1
    
    # Build bad character table
    bad_char_table = {}
    for i in range(len(pattern)):
        bad_char_table[pattern[i]] = i
    
    i = 0
    while i <= len(text) - len(pattern):
        j = len(pattern) - 1
        
        # Match from right to left
        while j >= 0 and pattern[j] == text[i + j]:
            j -= 1
        
        if j < 0:
            return i  # Found match
        
        # Calculate shift
        shift = j - bad_char_table.get(text[i + j], -1)
        i += max(1, shift)
    
    return -1

# Example usage
if __name__ == "__main__":
    # Test cases
    text1 = "ABAAABCD"
    pattern1 = "ABC"
    result1 = boyer_moore_simple(text1, pattern1)
    print(f"Text: '{text1}'")
    print(f"Pattern: '{pattern1}'")
    print(f"Found at index: {result1}")
    print()
    
    text2 = "THIS IS A TEST TEXT"
    pattern2 = "TEST"
    result2 = boyer_moore_simple(text2, pattern2)
    print(f"Text: '{text2}'")
    print(f"Pattern: '{pattern2}'")
    print(f"Found at index: {result2}")
    print()
    
    text3 = "AABAACAADAABAABA"
    pattern3 = "AABA"
    result3 = boyer_moore_simple(text3, pattern3)
    print(f"Text: '{text3}'")
    print(f"Pattern: '{pattern3}'")
    print(f"Found at index: {result3}")
```

## Output
```
Text: 'ABAAABCD'
Pattern: 'ABC'
Found at index: 4

Text: 'THIS IS A TEST TEXT'
Pattern: 'TEST'
Found at index: 10

Text: 'AABAACAADAABAABA'
Pattern: 'AABA'
Found at index: 0
```

## Key Features

1. **Time Complexity**: O(n/m) average case, O(n×m) worst case
2. **Space Complexity**: O(k) where k is the size of character set
3. **Heuristics Used**:
   - Bad character heuristic
   - Good suffix heuristic (in full implementation)
4. **Advantages**:
   - Faster than naive string matching for large texts
   - Works well with large alphabets
   - Efficient for pattern searching in large documents

The algorithm is particularly efficient when the pattern is long and the alphabet is large, making it ideal for applications like text editors, DNA sequence analysis, and search engines.

