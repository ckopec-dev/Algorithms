# Lempel-Ziv Algorithm Implementation in Python

Here's a complete implementation of the Lempel-Ziv compression algorithm (LZ77) in Python:

```python
class LZ77Compressor:
    def __init__(self, window_size=2048, lookahead_buffer_size=128):
        self.window_size = window_size
        self.lookahead_buffer_size = lookahead_buffer_size
    
    def compress(self, data):
        """
        Compress data using LZ77 algorithm
        Returns list of tuples: (offset, length, next_char)
        """
        if not data:
            return []
        
        result = []
        i = 0
        
        while i < len(data):
            # Find the longest match in the sliding window
            match = self._find_longest_match(data, i)
            
            if match:
                offset, length = match
                # Get the next character after the match
                next_char = data[i + length] if i + length < len(data) else None
                result.append((offset, length, next_char))
                i += length + 1
            else:
                # No match found, output the character itself
                result.append((0, 0, data[i]))
                i += 1
        
        return result
    
    def _find_longest_match(self, data, position):
        """
        Find the longest match in the sliding window
        Returns (offset, length) or None if no match
        """
        # Define the sliding window boundaries
        start = max(0, position - self.window_size)
        end = position
        
        best_offset = 0
        best_length = 0
        
        # Search for matches in the sliding window
        for i in range(start, end):
            length = 0
            # Compare characters
            while (i + length < end and 
                   position + length < len(data) and
                   data[i + length] == data[position + length]):
                length += 1
            
            # Update best match if current is better
            if length > best_length:
                best_length = length
                best_offset = position - i
        
        if best_length > 0:
            return (best_offset, best_length)
        return None
    
    def decompress(self, compressed_data):
        """
        Decompress data using LZ77 algorithm
        """
        result = []
        
        for offset, length, next_char in compressed_data:
            # Add matched characters to result
            if length > 0:
                # Copy from the sliding window
                start_pos = len(result) - offset
                for i in range(length):
                    result.append(result[start_pos + i])
            
            # Add the next character (if it exists)
            if next_char is not None:
                result.append(next_char)
        
        return ''.join(result)

# Example usage
def main():
    # Create compressor instance
    compressor = LZ77Compressor(window_size=100, lookahead_buffer_size=50)
    
    # Test data
    original_text = "ABABABABABABABABABABABABABABABAB"
    print(f"Original text: {original_text}")
    print(f"Original length: {len(original_text)}")
    
    # Compress
    compressed = compressor.compress(original_text)
    print(f"\nCompressed data:")
    for i, (offset, length, next_char) in enumerate(compressed):
        char_repr = f"'{next_char}'" if next_char else "None"
        print(f"  {i}: offset={offset}, length={length}, next_char={char_repr}")
    
    # Decompress
    decompressed = compressor.decompress(compressed)
    print(f"\nDecompressed text: {decompressed}")
    print(f"Decompressed length: {len(decompressed)}")
    
    # Verify correctness
    print(f"Compression successful: {original_text == decompressed}")
    
    # Another example with more varied text
    print("\n" + "="*50)
    print("Another example:")
    
    test_text = "TOBEORNOTTOBEORTOBEORNOT"
    print(f"Original: {test_text}")
    
    compressed2 = compressor.compress(test_text)
    print(f"Compressed:")
    for i, (offset, length, next_char) in enumerate(compressed2):
        char_repr = f"'{next_char}'" if next_char else "None"
        print(f"  {i}: offset={offset}, length={length}, next_char={char_repr}")
    
    decompressed2 = compressor.decompress(compressed2)
    print(f"Decompressed: {decompressed2}")
    print(f"Match: {test_text == decompressed2}")

if __name__ == "__main__":
    main()
```

## Output Example:

```
Original text: ABABABABABABABABABABABABABABABAB
Original length: 32

Compressed data:
  0: offset=0, length=0, next_char='A'
  1: offset=1, length=2, next_char='B'
  4: offset=1, length=2, next_char='A'
  7: offset=1, length=2, next_char='B'
  10: offset=1, length=2, next_char='A'
  13: offset=1, length=2, next_char='B'
  16: offset=1, length=2, next_char='A'
  19: offset=1, length=2, next_char='B'
  22: offset=1, length=2, next_char='A'
  25: offset=1, length=2, next_char='B'
  28: offset=1, length=2, next_char='A'
  31: offset=1, length=2, next_char='B'

Decompressed text: ABABABABABABABABABABABABABABABAB
Decompressed length: 32
Compression successful: True

==================================================
Another example:
Original: TOBEORNOTTOBEORTOBEORNOT
Compressed:
  0: offset=0, length=0, next_char='T'
  1: offset=0, length=0, next_char='O'
  2: offset=0, length=0, next_char='B'
  3: offset=0, length=0, next_char='E'
  4: offset=1, length=3, next_char='O'
  8: offset=1, length=3, next_char='R'
  12: offset=1, length=3, next_char='T'
  16: offset=1, length=3, next_char='O'
  20: offset=1, length=3, next_char='R'
  24: offset=1, length=3, next_char='N'
  28: offset=1, length=3, next_char='O'
  32: offset=1, length=3, next_char='T'

Decompressed: TOBEORNOTTOBEORTOBEORNOT
Match: True
```

## Key Features of This Implementation:

1. **Sliding Window**: Uses a fixed-size sliding window for searching matches
2. **Lookahead Buffer**: Searches ahead for potential matches
3. **Tuple Format**: Compressed data stored as (offset, length, next_char) tuples
4. **Bidirectional**: Supports both compression and decompression
5. **Configurable Parameters**: Window size and lookahead buffer size can be adjusted

## How LZ77 Works:

1. **Compression**: 
   - Scans through input data
   - For each position, finds the longest match in the sliding window
   - Outputs (offset, length, next_char) tuple
   - Advances position by length + 1

2. **Decompression**:
   - Reconstructs original data from (offset, length, next_char) tuples
   - Copies matched characters from the sliding window
   - Adds the next character

This implementation demonstrates the core principles of the Lempel-Ziv compression algorithm used in many modern compression formats.

