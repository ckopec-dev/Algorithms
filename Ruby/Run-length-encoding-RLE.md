# Run-Length Encoding (RLE) in Ruby

## Implementation

```ruby
def run_length_encode(input)
  return "" if input.nil? || input.empty?
  
  encoded = []
  current_char = input[0]
  count = 1
  
  (1...input.length).each do |i|
    if input[i] == current_char
      count += 1
    else
      encoded << "#{count}#{current_char}"
      current_char = input[i]
      count = 1
    end
  end
  
  # Don't forget the last group
  encoded << "#{count}#{current_char}"
  
  encoded.join
end

def run_length_decode(encoded_string)
  return "" if encoded_string.nil? || encoded_string.empty?
  
  decoded = []
  
  # Regular expression to match count and character
  encoded_string.scan(/(\d+)(\D)/) do |match|
    count = match[0].to_i
    char = match[1]
    decoded << char * count
  end
  
  decoded.join
end

# Example usage
original = "AAABBBCCDAA"
puts "Original: #{original}"

encoded = run_length_encode(original)
puts "Encoded:  #{encoded}"

decoded = run_length_decode(encoded)
puts "Decoded:  #{decoded}"

puts "Match: #{original == decoded}"
```

## Output
```
Original: AAABBBCCDAA
Encoded:  3A3B2C1D2A
Decoded:  AAABBBCCDAA
Match: true
```

## How it works

1. **Encoding**: 
   - Traverse the input string
   - Count consecutive identical characters
   - Store count followed by the character
   - Example: "AAABBBCCDAA" → "3A3B2C1D2A"

2. **Decoding**:
   - Use regex to extract count and character pairs
   - Repeat each character according to its count
   - Concatenate all characters to reconstruct the original string

## Alternative Compact Version

```ruby
def rle_encode(str)
  str.chars.chunk(&:itself).map { |char, chars| "#{chars.count}#{char}" }.join
end

def rle_decode(str)
  str.scan(/(\d+)(\D)/).map { |count, char| char * count.to_i }.join
end

# Test
text = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
puts "Encoded: #{rle_encode(text)}"
puts "Decoded: #{rle_decode(rle_encode(text))}"
```

This implementation efficiently handles run-length encoding and decoding with proper edge case handling.

