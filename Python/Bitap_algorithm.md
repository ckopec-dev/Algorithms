# Bitap Algorithm Implementation

The Bitap algorithm (also known as the Shift-Or algorithm) is a string searching algorithm that uses bit manipulation to find occurrences of a pattern within a text.

```python
def bitap_search(pattern, text):
    """
    Bitap algorithm implementation for string searching
    
    Args:
        pattern (str): The pattern to search for
        text (str): The text to search in
    
    Returns:
        list: List of starting positions where pattern is found
    """
    
    if not pattern or not text:
        return []
    
    # Create a bit vector for each character in the pattern
    # This represents which positions in the pattern each character appears
    char_table = {}
    for i, char in enumerate(pattern):
        if char not in char_table:
            char_table[char] = 0
        char_table[char] |= (1 << i)
    
    # Initialize bit vector
    match_mask = (1 << len(pattern)) - 1
    
    # Initialize current state
    current_state = 0
    
    # Positions where pattern is found
    positions = []
    
    # Process each character in the text
    for i, char in enumerate(text):
        # Update the current state using bit operations
        current_state = ((current_state << 1) | 1) & char_table.get(char, 0)
        
        # Check if we have a complete match
        if current_state & match_mask:
            positions.append(i - len(pattern) + 1)
    
    return positions

def bitap_search_with_error(pattern, text, max_errors=1):
    """
    Bitap algorithm with error tolerance
    
    Args:
        pattern (str): The pattern to search for
        text (str): The text to search in
        max_errors (int): Maximum number of errors allowed
    
    Returns:
        list: List of tuples (position, errors) where pattern is found
    """
    
    if not pattern or not text:
        return []
    
    # Create character table
    char_table = {}
    for i, char in enumerate(pattern):
        if char not in char_table:
            char_table[char] = 0
        char_table[char] |= (1 << i)
    
    # Initialize bit vectors
    match_mask = (1 << len(pattern)) - 1
    
    # For error tolerance, we need multiple states
    current_states = [0] * (max_errors + 1)
    
    positions = []
    
    for i, char in enumerate(text):
        # Update all states
        for j in range(max_errors, -1, -1):
            if j == 0:
                # No errors case
                new_state = ((current_states[0] << 1) | 1) & char_table.get(char, 0)
            else:
                # Error case - we can have one error at position j
                new_state = ((current_states[j] << 1) | 1) & char_table.get(char, 0)
                if j > 0:
                    new_state |= current_states[j-1]
            
            current_states[j] = new_state
            
            # Check for match with errors
            if current_states[j] & match_mask:
                positions.append((i - len(pattern) + 1, j))
    
    return positions

# Example usage and test cases
if __name__ == "__main__":
    # Test case 1: Basic search
    text1 = "This is a simple example text"
    pattern1 = "simple"
    
    print("Text:", text1)
    print("Pattern:", pattern1)
    result1 = bitap_search(pattern1, text1)
    print("Found at positions:", result1)
    print()
    
    # Test case 2: Multiple occurrences
    text2 = "abababab"
    pattern2 = "abab"
    
    print("Text:", text2)
    print("Pattern:", pattern2)
    result2 = bitap_search(pattern2, text2)
    print("Found at positions:", result2)
    print()
    
    # Test case 3: No match
    text3 = "hello world"
    pattern3 = "xyz"
    
    print("Text:", text3)
    print("Pattern:", pattern3)
    result3 = bitap_search(pattern3, text3)
    print("Found at positions:", result3)
    print()
    
    # Test case 4: With error tolerance
    text4 = "kitten"
    pattern4 = "sitting"
    
    print("Text:", text4)
    print("Pattern:", pattern4)
    result4 = bitap_search_with_error(pattern4, text4, max_errors=2)
    print("Found with errors:", result4)
```

## How it works:

1. **Character Table Creation**: For each character in the pattern, we create a bitmask representing all positions where that character appears
2. **Bit Manipulation**: We use bit shifting and bitwise operations to efficiently track potential matches
3. **State Tracking**: The algorithm maintains a current state that represents possible match positions
4. **Match Detection**: When a complete match is found, the algorithm records the position

## Time Complexity:
- Best case: O(n) where n is the length of text
- Worst case: O(n*m) where m is the length of pattern

## Space Complexity:
- O(k) where k is the size of the character set

The Bitap algorithm is particularly efficient for short patterns and can be faster than traditional string search algorithms in many practical scenarios.

