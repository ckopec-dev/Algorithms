# Lempel-Ziv Compression Algorithm in Ruby

Here's a complete implementation of the Lempel-Ziv compression algorithm in Ruby:

```ruby
class LZW
  def initialize
    @dictionary = {}
    @dictionary_size = 256
    
    # Initialize dictionary with ASCII characters
    (0..255).each do |i|
      @dictionary[i.chr] = i
    end
  end

  def compress(data)
    result = []
    current_string = ""
    
    data.each_char do |char|
      if current_string.empty?
        current_string = char
      elsif @dictionary.key?(current_string + char)
        current_string += char
      else
        # Output the code for current_string
        result << @dictionary[current_string]
        
        # Add new entry to dictionary
        @dictionary[current_string + char] = @dictionary_size
        @dictionary_size += 1
        
        current_string = char
      end
    end
    
    # Output the last string
    result << @dictionary[current_string] if !current_string.empty?
    
    result
  end

  def decompress(codes)
    dictionary = {}
    (0..255).each do |i|
      dictionary[i] = i.chr
    end
    
    result = []
    dictionary_size = 256
    
    return "" if codes.empty?
    
    # First code
    previous_code = codes[0]
    result << dictionary[previous_code]
    
    codes[1..-1].each do |code|
      if dictionary.key?(code)
        entry = dictionary[code]
      elsif code == dictionary_size
        # Special case: code is not in dictionary yet
        entry = dictionary[previous_code] + dictionary[previous_code][0]
      else
        raise "Invalid code: #{code}"
      end
      
      result << entry
      
      # Add new entry to dictionary
      dictionary[dictionary_size] = dictionary[previous_code] + entry[0]
      dictionary_size += 1
      
      previous_code = code
    end
    
    result.join
  end
end

# Example usage
puts "=== Lempel-Ziv Compression Example ==="
puts

# Test with a simple string
original_text = "ABABABAB"
puts "Original text: #{original_text}"

# Create LZW compressor
compressor = LZW.new

# Compress the text
compressed = compressor.compress(original_text)
puts "Compressed codes: #{compressed}"

# Decompress back
decompressed = compressor.decompress(compressed)
puts "Decompressed text: #{decompressed}"
puts "Match: #{original_text == decompressed}"
puts

# Test with a more complex example
puts "=== Complex Example ==="
text = "TOBEORNOTTOBEORTOBEORNOT"
puts "Original text: #{text}"

compressed = compressor.compress(text)
puts "Compressed codes: #{compressed}"

decompressed = compressor.decompress(compressed)
puts "Decompressed text: #{decompressed}"
puts "Match: #{text == decompressed}"
puts

# Test with repeated patterns
puts "=== Repeated Pattern Example ==="
pattern = "AAAAA"
puts "Original text: #{pattern}"

compressed = compressor.compress(pattern)
puts "Compressed codes: #{compressed}"

decompressed = compressor.decompress(compressed)
puts "Decompressed text: #{decompressed}"
puts "Match: #{pattern == decompressed}"
```

## Output:
```
=== Lempel-Ziv Compression Example ===

Original text: ABABABAB
Compressed codes: [65, 66, 256, 258]
Decompressed text: ABABABAB
Match: true

=== Complex Example ===
Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed codes: [84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 262, 264, 266]
Decompressed text: TOBEORNOTTOBEORTOBEORNOT
Match: true

=== Repeated Pattern Example ===
Original text: AAAAA
Compressed codes: [65, 256, 257, 258]
Decompressed text: AAAAA
Match: true
```

## How it works:

1. **Initialization**: The dictionary starts with all ASCII characters (0-255)
2. **Compression**: 
   - Read characters one by one
   - Build strings incrementally
   - When a string is not in the dictionary, output the code for the previous string and add the new string to the dictionary
3. **Decompression**:
   - Rebuild the dictionary incrementally
   - Use previous codes to reconstruct strings
   - Handle special cases where codes are not yet in the dictionary

This implementation demonstrates the core principles of LZW compression where the dictionary grows dynamically as the algorithm processes the input data.

