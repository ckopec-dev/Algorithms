```python
def arithmetic_encode(data, freq_table):
    """
    Arithmetic encoding algorithm implementation
    
    Args:
        data: string to encode
        freq_table: dictionary with character frequencies
    
    Returns:
        encoded value as tuple (lower_bound, upper_bound)
    """
    
    # Initialize bounds
    low = 0.0
    high = 1.0
    
    # Calculate total frequency
    total_freq = sum(freq_table.values())
    
    # Process each character in the data
    for char in data:
        # Get frequency of current character
        char_freq = freq_table[char]
        
        # Calculate range for this character
        range_size = high - low
        char_low = low + (range_size * (sum(freq_table[c] for c in freq_table if c < char) / total_freq))
        char_high = low + (range_size * (sum(freq_table[c] for c in freq_table if c <= char) / total_freq))
        
        # Update bounds
        low = char_low
        high = char_high
    
    return (low, high)

def arithmetic_decode(encoded_bounds, freq_table, length):
    """
    Arithmetic decoding algorithm implementation
    
    Args:
        encoded_bounds: tuple (lower_bound, upper_bound) from encoding
        freq_table: dictionary with character frequencies
        length: original string length
    
    Returns:
        decoded string
    """
    
    low, high = encoded_bounds
    total_freq = sum(freq_table.values())
    
    # Initialize result
    result = []
    
    # Decode each character
    for _ in range(length):
        # Calculate current range size
        range_size = high - low
        
        # Find which character this value belongs to
        target = (low + high) / 2  # Midpoint of current range
        
        cumulative_freq = 0
        decoded_char = None
        
        for char in sorted(freq_table.keys()):
            char_freq = freq_table[char]
            cumulative_freq += char_freq
            
            # Check if target falls within this character's range
            char_low = low + (range_size * ((cumulative_freq - char_freq) / total_freq))
            char_high = low + (range_size * (cumulative_freq / total_freq))
            
            if char_low <= target < char_high:
                decoded_char = char
                # Update bounds for next iteration
                low = char_low
                high = char_high
                break
        
        result.append(decoded_char)
    
    return ''.join(result)

# Example usage
if __name__ == "__main__":
    # Sample data and frequency table
    text = "hello"
    freq_table = {'h': 1, 'e': 1, 'l': 2, 'o': 1}
    
    print("Original text:", text)
    print("Frequency table:", freq_table)
    
    # Encode the text
    encoded_result = arithmetic_encode(text, freq_table)
    print(f"\nEncoded bounds: ({encoded_result[0]:.6f}, {encoded_result[1]:.6f})")
    
    # Decode back
    decoded_text = arithmetic_decode(encoded_result, freq_table, len(text))
    print("Decoded text:", decoded_text)
    
    # Another example with different data
    print("\n" + "="*50)
    text2 = "aab"
    freq_table2 = {'a': 2, 'b': 1}
    
    print("Original text:", text2)
    print("Frequency table:", freq_table2)
    
    encoded_result2 = arithmetic_encode(text2, freq_table2)
    print(f"\nEncoded bounds: ({encoded_result2[0]:.6f}, {encoded_result2[1]:.6f})")
    
    decoded_text2 = arithmetic_decode(encoded_result2, freq_table2, len(text2))
    print("Decoded text:", decoded_text2)
```

This implementation demonstrates:

1. **Arithmetic Encoding**: Converts a string into a range of floating-point numbers based on character frequencies
2. **Arithmetic Decoding**: Recovers the original string from the encoded bounds
3. **Key Concepts**:
   - Frequency-based probability distribution
   - Interval narrowing during encoding
   - Midpoint selection for decoding
   - Proper handling of cumulative frequencies

The algorithm works by maintaining a range [low, high] that gets progressively narrowed as each character is processed, with the final range representing the encoded value.

