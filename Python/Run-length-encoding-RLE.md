# Run-Length Encoding (RLE) Algorithm in Python

## What is Run-Length Encoding?
Run-Length Encoding is a simple form of data compression where consecutive identical elements are stored as a single element and a count.

## Python Implementation

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
            # Store the character and its count
            encoded.append(f"{count}{current_char}")
            current_char = data[i]
            count = 1
    
    # Don't forget the last group
    encoded.append(f"{count}{current_char}")
    
    return "".join(encoded)

def run_length_decode(encoded_data):
    """
    Decode a Run-Length Encoded string
    
    Args:
        encoded_data (str): RLE encoded string
    
    Returns:
        str: Decoded original string
    """
    if not encoded_data:
        return ""
    
    decoded = []
    
    # Process the encoded data in pairs of count and character
    i = 0
    while i < len(encoded_data):
        # Extract the count (could be multiple digits)
        count = ""
        while i < len(encoded_data) and encoded_data[i].isdigit():
            count += encoded_data[i]
            i += 1
        
        # Extract the character
        if i < len(encoded_data):
            char = encoded_data[i]
            decoded.append(char * int(count))
            i += 1
    
    return "".join(decoded)

# Example usage
if __name__ == "__main__":
    # Test cases
    test_strings = [
        "AAAABBBCCDAA",
        "AAAAAAAAAA",
        "abcdef",
        "AABBCC",
        ""
    ]
    
    print("Run-Length Encoding Examples:")
    print("=" * 40)
    
    for original in test_strings:
        encoded = run_length_encode(original)
        decoded = run_length_decode(encoded)
        
        print(f"Original:   '{original}'")
        print(f"Encoded:    '{encoded}'")
        print(f"Decoded:    '{decoded}'")
        print(f"Match:      {original == decoded}")
        print("-" * 40)
```

## Output Example

```
Run-Length Encoding Examples:
========================================
Original:   'AAAABBBCCDAA'
Encoded:    '4A3B2C1D2A'
Decoded:    'AAAABBBCCDAA'
Match:      True
----------------------------------------
Original:   'AAAAAAAAAA'
Encoded:    '10A'
Decoded:    'AAAAAAAAAA'
Match:      True
----------------------------------------
Original:   'abcdef'
Encoded:    '1a1b1c1d1e1f'
Decoded:    'abcdef'
Match:      True
----------------------------------------
Original:   'AABBCC'
Encoded:    '2A2B2C'
Decoded:    'AABBCC'
Match:      True
----------------------------------------
Original:   ''
Encoded:    ''
Decoded:    ''
Match:      True
----------------------------------------
```

## How it works:

1. **Encoding**: 
   - Traverse the input string
   - Count consecutive identical characters
   - Store count followed by the character
   - Example: "AAAABBBCCDAA" → "4A3B2C1D2A"

2. **Decoding**:
   - Parse the encoded string
   - Extract count and character pairs
   - Repeat each character according to its count
   - Reconstruct the original string

## Time and Space Complexity:
- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for the encoded output

This implementation handles strings with any characters and properly manages multi-digit counts.

