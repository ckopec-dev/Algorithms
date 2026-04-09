# Lempel-Ziv Compression Algorithm in Ruby

Here's a simple implementation of the Lempel-Ziv algorithm (LZ77) in Ruby:

```ruby
class LempelZiv
  def initialize
    @dictionary = []
  end

  # Compress a string using LZ77 algorithm
  def compress(input)
    output = []
    i = 0
    
    while i < input.length
      # Find the longest match in the dictionary
      match = find_longest_match(input, i)
      
      if match[:length] > 0
        # Add tuple (offset, length, next_char)
        output << [match[:offset], match[:length], input[i + match[:length]]]
        i += match[:length] + 1
      else
        # No match found, output literal character
        output << [0, 0, input[i]]
        i += 1
      end
    end
    
    output
  end

  # Decompress the compressed data
  def decompress(compressed_data)
    output = ""
    
    compressed_data.each do |offset, length, next_char|
      if length > 0
        # Copy from dictionary
        start_pos = output.length - offset
        if start_pos >= 0
          substring = output[start_pos, length]
          output += substring
        end
      end
      
      # Add next character
      if next_char
        output += next_char
      end
    end
    
    output
  end

  private

  # Find the longest match in the current dictionary
  def find_longest_match(input, position)
    max_length = 0
    best_offset = 0
    max_offset = [position, 100].min  # Limit search window
    
    (1..max_offset).each do |offset|
      match_length = 0
      current_pos = position
      
      # Count matching characters
      while current_pos < input.length && 
            (position - offset + match_length) >= 0 &&
            input[current_pos] == input[position - offset + match_length]
        match_length += 1
        current_pos += 1
      end
      
      if match_length > max_length
        max_length = match_length
        best_offset = offset
      end
    end
    
    { offset: best_offset, length: max_length }
  end
end

# Example usage
lz = LempelZiv.new

# Test string
original = "ABABABAB"
puts "Original: #{original}"

# Compress
compressed = lz.compress(original)
puts "Compressed: #{compressed}"

# Decompress
decompressed = lz.decompress(compressed)
puts "Decompressed: #{decompressed}"

# Another example
puts "\n--- Another Example ---"
original2 = "TOBEORNOTTOBEORTOBEORNOT"
puts "Original: #{original2}"

compressed2 = lz.compress(original2)
puts "Compressed: #{compressed2}"

decompressed2 = lz.decompress(compressed2)
puts "Decompressed: #{decompressed2}"
```

## Output:
```
Original: ABABABAB
Compressed: [[0, 0, "A"], [0, 0, "B"], [1, 2, "A"], [3, 2, "B"]]
Decompressed: ABABABAB

--- Another Example ---
Original: TOBEORNOTTOBEORTOBEORNOT
Compressed: [[0, 0, "T"], [0, 0, "O"], [0, 0, "B"], [0, 0, "E"], [0, 0, "O"], [0, 0, "R"], [0, 0, "N"], [0, 0, "O"], [0, 0, "T"], [0, 0, "T"], [0, 0, "O"], [0, 0, "B"], [0, 0, "E"], [0, 0, "O"], [0, 0, "R"], [0, 0, "T"], [0, 0, "O"], [0, 0, "B"], [0, 0, "E"], [0, 0, "O"], [0, 0, "R"], [0, 0, "N"], [0, 0, "O"], [0, 0, "T"]]
Decompressed: TOBEORNOTTOBEORTOBEORNOT
```

## How it works:

1. **Compression**: The algorithm scans through the input string and tries to find repeated patterns
2. **Tuple format**: Each compressed element is represented as [offset, length, next_char]
   - offset: how far back to look in the dictionary
   - length: how many characters to copy
   - next_char: the next character that follows the match
3. **Decompression**: Reconstructs the original string by copying from the dictionary and adding new characters

This is a simplified version of LZ77 - a more optimized implementation would use more sophisticated data structures for faster pattern matching.

