# Run-Length Encoding (RLE) Algorithm in Python

## Implementation

```python
def run_length_encode(data):
    """
    Encode a string using Run-Length Encoding
    
    Args:
        data (str): Input string to encode
    
    Returns:
        str: Run-length encoded string
    """
    if not data:
        return ""
    
    encoded = []
    current_char = data[0]
    count = 1
    
    # Iterate through the string starting from the second character
    for i in range(1, len(data)):
        if data[i] == current_char:
            count += 1
        else:
            # Append the count and character to result
            encoded.append(str(count) + current_char)
            current_char = data[i]
            count = 1
    
    # Don't forget the last sequence
    encoded.append(str(count) + current_char)
    
    return ''.join(encoded)

def run_length_decode(encoded_data):
    """
    Decode a Run-Length Encoded string
    
    Args:
        encoded_data (str): Run-length encoded string
    
    Returns:
        str: Decoded original string
    """
    if not encoded_data:
        return ""
    
    decoded = []
    i = 0
    
    while i < len(encoded_data):
        # Extract the count (may be multiple digits)
        count = ""
        while i < len(encoded_data) and encoded_data[i].isdigit():
            count += encoded_data[i]
            i += 1
        
        # Get the character to repeat
        if i < len(encoded_data):
            char = encoded_data[i]
            decoded.append(char * int(count))
            i += 1
    
    return ''.join(decoded)

# Example usage
if __name__ == "__main__":
    # Test cases
    test_strings = [
        "AAABBBCCD",
        "AAAAAAAAAA",
        "AABBCC",
        "A",
        "",
        "AABBAA"
    ]
    
    print("Run-Length Encoding Examples:")
    print("=" * 40)
    
    for test_string in test_strings:
        encoded = run_length_encode(test_string)
        decoded = run_length_decode(encoded)
        
        print(f"Original:   '{test_string}'")
        print(f"Encoded:    '{encoded}'")
        print(f"Decoded:    '{decoded}'")
        print(f"Match:      {test_string == decoded}")
        print("-" * 30)
```

## Output

```
Run-Length Encoding Examples:
========================================
Original:   'AAABBBCCD'
Encoded:    '3A3B2C1D'
Decoded:    'AAABBBCCD'
Match:      True
------------------------------
Original:   'AAAAAAAAAA'
Encoded:    '10A'
Decoded:    'AAAAAAAAAA'
Match:      True
------------------------------
Original:   'AABBCC'
Encoded:    '2A2B2C'
Decoded:    'AABBCC'
Match:      True
------------------------------
Original:   'A'
Encoded:    '1A'
Decoded:    'A'
Match:      True
------------------------------
Original:   ''
Encoded:    ''
Decoded:    ''
Match:      True
------------------------------
Original:   'AABBAA'
Encoded:    '2A2B2A'
Decoded:    'AABBAA'
Match:      True
------------------------------
```

## How It Works

1. **Encoding Process**:
   - Traverse the input string character by character
   - Count consecutive occurrences of the same character
   - Store the count followed by the character
   - Repeat for each unique sequence

2. **Decoding Process**:
   - Parse the encoded string to extract counts and characters
   - Repeat each character according to its count
   - Reconstruct the original string

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the encoded/decoded result

## Use Cases

- Data compression for strings with many repeated characters
- Image compression (especially for images with large uniform areas)
- Network data transmission optimization
- File format compression algorithms

