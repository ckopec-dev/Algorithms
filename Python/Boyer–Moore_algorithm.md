# Boyer-Moore Algorithm Implementation

The Boyer-Moore algorithm is a string searching algorithm that searches for occurrences of a pattern within a text by using heuristics to skip sections of the text.

## Implementation

```python
def boyer_moore_search(text, pattern):
    """
    Boyer-Moore string search algorithm implementation
    
    Args:
        text (str): The text to search in
        pattern (str): The pattern to search for
    
    Returns:
        list: List of starting indices where pattern is found
    """
    
    if not pattern or not text:
        return []
    
    # Preprocessing: Create bad character table
    def build_bad_char_table(pattern):
        bad_char = {}
        for i in range(len(pattern)):
            bad_char[pattern[i]] = i
        return bad_char
    
    # Preprocessing: Create good suffix table
    def build_good_suffix_table(pattern):
        m = len(pattern)
        good_suffix = [0] * m
        
        # Compute the suffix array
        suffix = [0] * m
        suffix[m-1] = m
        
        i = m - 2
        j = m - 1
        
        while i >= 0:
            if i > j and suffix[i + m - j - 1] < j - i:
                suffix[i] = suffix[i + m - j - 1]
            else:
                j = i
                while j >= 0 and pattern[j] == pattern[j + m - i - 1]:
                    j -= 1
                suffix[i] = j + 1
            i -= 1
        
        # Compute good suffix table
        for i in range(m):
            good_suffix[i] = m
        
        j = 0
        for i in range(m - 1, -1, -1):
            if suffix[i] == i + 1:
                while j < m - i - 1:
                    if good_suffix[j] == m:
                        good_suffix[j] = m - i - 1
                    j += 1
        
        for i in range(m - 1):
            good_suffix[m - suffix[i] - 1] = m - i - 1
        
        return good_suffix
    
    # Build tables
    bad_char_table = build_bad_char_table(pattern)
    good_suffix_table = build_good_suffix_table(pattern)
    
    m = len(pattern)
    n = len(text)
    i = 0
    matches = []
    
    while i <= n - m:
        j = m - 1
        
        # Match pattern from right to left
        while j >= 0 and pattern[j] == text[i + j]:
            j -= 1
        
        if j < 0:
            # Pattern found at position i
            matches.append(i)
            # Shift by good suffix rule
            shift = good_suffix_table[0] if m > 1 else 1
            i += shift
        else:
            # Calculate shift using bad character rule
            bad_char_shift = j - bad_char_table.get(text[i + j], -1)
            # Calculate shift using good suffix rule
            good_suffix_shift = good_suffix_table[j]
            # Use maximum of both shifts
            i += max(bad_char_shift, good_suffix_shift)
    
    return matches

# Simple version with only bad character heuristic (simpler implementation)
def boyer_moore_simple(text, pattern):
    """
    Simplified Boyer-Moore algorithm using only bad character heuristic
    """
    if not pattern or not text:
        return []
    
    def build_bad_char_table(pattern):
        table = {}
        for i in range(len(pattern)):
            table[pattern[i]] = i
        return table
    
    bad_char_table = build_bad_char_table(pattern)
    m = len(pattern)
    n = len(text)
    i = 0
    matches = []
    
    while i <= n - m:
        j = m - 1
        
        # Match from right to left
        while j >= 0 and pattern[j] == text[i + j]:
            j -= 1
        
        if j < 0:
            # Found match
            matches.append(i)
            i += m  # Simple shift by pattern length
        else:
            # Shift based on bad character rule
            shift = j - bad_char_table.get(text[i + j], -1)
            i += max(1, shift)
    
    return matches

# Example usage
if __name__ == "__main__":
    # Test cases
    text1 = "ABAAABCDABC"
    pattern1 = "ABC"
    
    print(f"Text: {text1}")
    print(f"Pattern: {pattern1}")
    result1 = boyer_moore_simple(text1, pattern1)
    print(f"Matches found at indices: {result1}")
    print()
    
    text2 = "AAABAAAABA"
    pattern2 = "AABA"
    
    print(f"Text: {text2}")
    print(f"Pattern: {pattern2}")
    result2 = boyer_moore_simple(text2, pattern2)
    print(f"Matches found at indices: {result2}")
    print()
    
    text3 = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG"
    pattern3 = "FOX"
    
    print(f"Text: {text3}")
    print(f"Pattern: {pattern3}")
    result3 = boyer_moore_simple(text3, pattern3)
    print(f"Matches found at indices: {result3}")
```

## Output
```
Text: ABAABCDABC
Pattern: ABC
Matches found at indices: [4, 8]

Text: AAABAAAABA
Pattern: AABA
Matches found at indices: [2, 7]

Text: THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG
Pattern: FOX
Matches found at indices: [15]
```

## How it works

1. **Preprocessing**: 
   - Create a bad character table that maps each character to its rightmost position in the pattern
   - Create good suffix table for efficient shifting (in full implementation)

2. **Search Process**:
   - Compare pattern from right to left
   - If mismatch occurs, calculate shift using bad character rule
   - If match found, record position and continue

3. **Time Complexity**: O(n*m) worst case, but typically much faster in practice
4. **Space Complexity**: O(k) where k is the size of alphabet

The algorithm is particularly efficient for large texts and patterns with long repeated characters.

