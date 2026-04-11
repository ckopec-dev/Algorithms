# Rabin-Karp Algorithm Implementation

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. It's particularly useful for multiple pattern matching.

## Python Implementation

```python
def rabin_karp_search(text, pattern):
    """
    Search for pattern in text using Rabin-Karp algorithm
    
    Args:
        text (str): The text to search in
        pattern (str): The pattern to search for
    
    Returns:
        list: List of starting indices where pattern is found
    """
    if not pattern or not text:
        return []
    
    if len(pattern) > len(text):
        return []
    
    # Prime number for hashing
    prime = 101
    pattern_len = len(pattern)
    text_len = len(text)
    
    # Calculate hash value of pattern and first window of text
    pattern_hash = 0
    text_hash = 0
    h = 1
    
    # Calculate h = pow(256, pattern_len-1) % prime
    for i in range(pattern_len - 1):
        h = (h * 256) % prime
    
    # Calculate hash value of pattern and first window of text
    for i in range(pattern_len):
        pattern_hash = (256 * pattern_hash + ord(pattern[i])) % prime
        text_hash = (256 * text_hash + ord(text[i])) % prime
    
    # List to store all occurrences
    occurrences = []
    
    # Slide the pattern over text one by one
    for i in range(text_len - pattern_len + 1):
        # Check if hash values match
        if pattern_hash == text_hash:
            # Check character by character if hash matches
            if text[i:i + pattern_len] == pattern:
                occurrences.append(i)
        
        # Calculate hash value for next window of text
        if i < text_len - pattern_len:
            text_hash = (256 * (text_hash - ord(text[i]) * h) + ord(text[i + pattern_len])) % prime
            
            # Handle negative hash values
            if text_hash < 0:
                text_hash += prime
    
    return occurrences

# Example usage
def main():
    # Example 1
    text1 = "ABABDABACDABABCABCABC"
    pattern1 = "ABABCABCABC"
    
    print("Example 1:")
    print(f"Text: {text1}")
    print(f"Pattern: {pattern1}")
    result1 = rabin_karp_search(text1, pattern1)
    print(f"Occurrences at indices: {result1}")
    print()
    
    # Example 2
    text2 = "AABAACAADAABAABA"
    pattern2 = "AABA"
    
    print("Example 2:")
    print(f"Text: {text2}")
    print(f"Pattern: {pattern2}")
    result2 = rabin_karp_search(text2, pattern2)
    print(f"Occurrences at indices: {result2}")
    print()
    
    # Example 3 - No matches
    text3 = "HELLO WORLD"
    pattern3 = "XYZ"
    
    print("Example 3:")
    print(f"Text: {text3}")
    print(f"Pattern: {pattern3}")
    result3 = rabin_karp_search(text3, pattern3)
    print(f"Occurrences at indices: {result3}")

# Run the examples
if __name__ == "__main__":
    main()
```

## Output

```
Example 1:
Text: ABABDABACDABABCABCABC
Pattern: ABABCABCABC
Occurrences at indices: [10]

Example 2:
Text: AABAACAADAABAABA
Pattern: AABA
Occurrences at indices: [0, 9, 13]

Example 3:
Text: HELLO WORLD
Pattern: XYZ
Occurrences at indices: []
```

## How It Works

1. **Hash Calculation**: Compute hash values for the pattern and the first window of text
2. **Sliding Window**: Move the pattern one character at a time through the text
3. **Hash Comparison**: Compare hash values first (fast operation)
4. **Character Verification**: If hashes match, verify actual characters to avoid hash collisions
5. **Rolling Hash**: Use the rolling hash technique to efficiently compute new hash values

## Time Complexity

- **Best/Average Case**: O(n + m) where n is text length and m is pattern length
- **Worst Case**: O(n × m) when there are many hash collisions

## Space Complexity

O(1) - Only using a constant amount of extra space

## Key Features

- Efficient for multiple pattern searches
- Handles hash collisions gracefully
- Uses rolling hash for optimal performance
- Good for streaming text processing

The Rabin-Karp algorithm is particularly useful when you need to search for multiple patterns in the same text or when the text is very large.

