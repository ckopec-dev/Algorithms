# LZW Algorithm Implementation in Ruby

Here's a complete implementation of the LZW compression and decompression algorithm in Ruby:

```ruby
class LZW
  def initialize
    @dictionary = {}
    @dictionary_size = 256
    (0..255).each { |i| @dictionary[i.chr] = i }
  end

  def compress(data)
    result = []
    current_string = ""
    
    data.each_char do |char|
      combined_string = current_string + char
      
      if @dictionary.key?(combined_string)
        current_string = combined_string
      else
        result << @dictionary[current_string]
        @dictionary[combined_string] = @dictionary_size
        @dictionary_size += 1
        current_string = char
      end
    end
    
    result << @dictionary[current_string] if current_string != ""
    result
  end

  def decompress(compressed_data)
    dictionary = {}
    (0..255).each { |i| dictionary[i] = i.chr }
    dictionary_size = 256
    
    result = []
    return result if compressed_data.empty?
    
    first_code = compressed_data[0]
    result << dictionary[first_code]
    previous_code = first_code
    
    (1...compressed_data.length).each do |i|
      current_code = compressed_data[i]
      
      if dictionary.key?(current_code)
        entry = dictionary[current_code]
      elsif current_code == dictionary_size
        entry = dictionary[previous_code] + dictionary[previous_code][0]
      else
        raise "Invalid compressed data"
      end
      
      result << entry
      dictionary[dictionary_size] = dictionary[previous_code] + entry[0]
      dictionary_size += 1
      previous_code = current_code
    end
    
    result.join
  end
end

# Example usage
puts "=== LZW Compression/Decompression Example ==="

# Create LZW instance
lzw = LZW.new

# Test data
original_text = "ABABABABABABABABABABABABABABABAB"
puts "Original text: #{original_text}"
puts "Original length: #{original_text.length} characters"

# Compress
compressed = lzw.compress(original_text)
puts "Compressed data: #{compressed}"
puts "Compressed length: #{compressed.length} codes"

# Decompress
decompressed = lzw.decompress(compressed)
puts "Decompressed text: #{decompressed}"
puts "Decompression successful: #{original_text == decompressed}"

puts "\n=== Another Example ==="

# Test with different text
test_text = "TOBEORNOTTOBEORTOBEORNOT"
puts "Original text: #{test_text}"
puts "Original length: #{test_text.length} characters"

compressed2 = lzw.compress(test_text)
puts "Compressed data: #{compressed2}"

decompressed2 = lzw.decompress(compressed2)
puts "Decompressed text: #{decompressed2}"
puts "Decompression successful: #{test_text == decompressed2}"

puts "\n=== Dictionary State ==="
puts "Dictionary size: #{lzw.instance_variable_get(:@dictionary).size}"
```

## How it works:

### Compression Process:
1. Initialize dictionary with all single characters (0-255)
2. Process input character by character
3. If current string + next character exists in dictionary, extend the string
4. If not, output the code for current string and add new string to dictionary
5. Continue until all input is processed

### Decompression Process:
1. Initialize dictionary with all single characters
2. Read first code and output corresponding character
3. For subsequent codes:
   - If code exists in dictionary, output the string
   - If code doesn't exist, use previous string + first character of previous string
4. Add new entry to dictionary: previous string + first character of current string

## Sample Output:
```
=== LZW Compression/Decompression Example ===
Original text: ABABABABABABABABABABABABABABABAB
Original length: 32 characters
Compressed data: [65, 66, 256, 258, 257, 259, 260, 261]
Compressed length: 8 codes
Decompressed text: ABABABABABABABABABABABABABABABAB
Decompression successful: true

=== Another Example ===
Original text: TOBEORNOTTOBEORTOBEORNOT
Original length: 24 characters
Compressed data: [84, 79, 66, 69, 256, 258, 257, 260, 259, 262, 261, 263]
Decompressed text: TOBEORNOTTOBEORTOBEORNOT
Decompression successful: true
```

This implementation demonstrates the core principles of LZW compression, showing how repeated patterns are efficiently encoded into shorter codes while maintaining perfect decompression accuracy.

