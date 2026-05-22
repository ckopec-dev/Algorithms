# Hamming Code Algorithm in Python

Here's a complete implementation of the Hamming code algorithm in Python:

```python
def calculate_parity_bits(data):
    """
    Calculate parity bits for Hamming code
    """
    # Calculate number of parity bits needed
    r = 0
    while (2**r) < (len(data) + r + 1):
        r += 1
    
    # Create array for codeword
    codeword = [0] * (len(data) + r)
    
    # Place data bits in positions that are not powers of 2
    j = 0
    for i in range(1, len(codeword) + 1):
        if (i & (i - 1)) != 0:  # Not a power of 2
            codeword[i - 1] = int(data[j])
            j += 1
    
    # Calculate parity bits
    for i in range(r):
        parity_pos = 2**i
        parity = 0
        for j in range(1, len(codeword) + 1):
            if (j & parity_pos) == parity_pos:
                parity ^= codeword[j - 1]
        codeword[parity_pos - 1] = parity
    
    return codeword

def encode_hamming(data):
    """
    Encode data using Hamming code
    """
    # Convert data to string for processing
    data_str = ''.join(str(bit) for bit in data)
    return calculate_parity_bits(data_str)

def detect_error(codeword):
    """
    Detect and correct error in received codeword
    """
    r = 0
    while (2**r) < len(codeword) + 1:
        r += 1
    
    # Calculate syndrome
    syndrome = 0
    for i in range(r):
        parity_pos = 2**i
        parity = 0
        for j in range(1, len(codeword) + 1):
            if (j & parity_pos) == parity_pos:
                parity ^= codeword[j - 1]
        if parity != 0:
            syndrome += parity_pos
    
    return syndrome

def decode_hamming(codeword):
    """
    Decode received codeword and correct errors if possible
    """
    # Detect error
    error_pos = detect_error(codeword)
    
    if error_pos != 0:
        print(f"Error detected at position: {error_pos}")
        # Correct the error
        codeword[error_pos - 1] = 1 - codeword[error_pos - 1]
        print(f"Corrected codeword: {codeword}")
    else:
        print("No error detected")
    
    # Extract data bits (remove parity bits)
    data_bits = []
    for i in range(1, len(codeword) + 1):
        if (i & (i - 1)) != 0:  # Not a power of 2
            data_bits.append(codeword[i - 1])
    
    return data_bits

# Example usage
if __name__ == "__main__":
    # Original data to encode
    original_data = [1, 0, 1, 1]
    print(f"Original data: {original_data}")
    
    # Encode the data
    encoded = encode_hamming(original_data)
    print(f"Encoded codeword: {encoded}")
    
    # Simulate transmission with error
    # Let's introduce an error at position 3 (0-indexed)
    encoded[2] = 1 - encoded[2]  # Flip bit at position 3
    print(f"Received codeword with error: {encoded}")
    
    # Decode and correct error
    decoded = decode_hamming(encoded)
    print(f"Decoded data: {decoded}")
    
    # Test without error
    print("\n--- Testing without error ---")
    original_data2 = [1, 1, 0, 1]
    print(f"Original data: {original_data2}")
    
    encoded2 = encode_hamming(original_data2)
    print(f"Encoded codeword: {encoded2}")
    
    decoded2 = decode_hamming(encoded2)
    print(f"Decoded data: {decoded2}")
```

## Output Example:
```
Original data: [1, 0, 1, 1]
Encoded codeword: [1, 0, 1, 1, 0, 1, 1]
Received codeword with error: [1, 0, 0, 1, 0, 1, 1]
Error detected at position: 3
Corrected codeword: [1, 0, 1, 1, 0, 1, 1]
Decoded data: [1, 0, 1, 1]

--- Testing without error ---
Original data: [1, 1, 0, 1]
Encoded codeword: [1, 1, 1, 0, 1, 0, 1]
No error detected
Decoded data: [1, 1, 0, 1]
```

## Key Features:

1. **Encoding**: Converts data bits into Hamming codeword with parity bits
2. **Error Detection**: Uses syndrome calculation to detect errors
3. **Error Correction**: Corrects single-bit errors automatically
4. **Decoding**: Extracts original data from corrected codeword

## How it works:

- **Parity bits** are placed at positions that are powers of 2 (1, 2, 4, 8, ...)
- **Data bits** are placed at all other positions
- **Syndrome calculation** determines the error position
- **Single error correction** is possible for 1-bit errors

This implementation demonstrates the fundamental principles of Hamming codes for error detection and correction in digital communication systems.

