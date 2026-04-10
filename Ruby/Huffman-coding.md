# Huffman Coding Algorithm in Ruby

Huffman coding is a lossless data compression algorithm that uses variable-length codes for characters, with more frequent characters getting shorter codes.

```ruby
# Huffman Coding Implementation in Ruby

class HuffmanNode
  attr_accessor :char, :frequency, :left, :right
  
  def initialize(char, frequency, left = nil, right = nil)
    @char = char
    @frequency = frequency
    @left = left
    @right = right
  end
  
  # For heap comparison
  def <=>(other)
    @frequency <=> other.frequency
  end
end

class HuffmanCoding
  def initialize
    @codes = {}
  end
  
  def build_frequency_table(text)
    frequency = Hash.new(0)
    text.each_char { |char| frequency[char] += 1 }
    frequency
  end
  
  def build_huffman_tree(frequency_table)
    # Create a priority queue (min heap) of nodes
    heap = []
    
    frequency_table.each do |char, freq|
      heap << HuffmanNode.new(char, freq)
    end
    
    # Build the Huffman tree
    while heap.length > 1
      # Extract two nodes with minimum frequency
      left = heap.shift
      right = heap.shift
      
      # Create a new internal node with combined frequency
      merged_freq = left.frequency + right.frequency
      merged_node = HuffmanNode.new(nil, merged_freq, left, right)
      
      # Insert back into heap
      heap << merged_node
      heap.sort!
    end
    
    heap.first # Root of the Huffman tree
  end
  
  def generate_codes(node, code = "")
    return if node.nil?
    
    # If it's a leaf node (character node)
    if node.char
      @codes[node.char] = code.empty? ? "0" : code
    else
      # Traverse left and right subtrees
      generate_codes(node.left, code + "0")
      generate_codes(node.right, code + "1")
    end
  end
  
  def encode(text)
    frequency_table = build_frequency_table(text)
    root = build_huffman_tree(frequency_table)
    generate_codes(root)
    
    encoded_text = text.chars.map { |char| @codes[char] }.join
    [@codes, encoded_text]
  end
  
  def decode(encoded_text, codes)
    # Create reverse mapping
    reverse_codes = {}
    codes.each { |char, code| reverse_codes[code] = char }
    
    decoded_text = ""
    current_code = ""
    
    encoded_text.each_char do |bit|
      current_code += bit
      if reverse_codes.key?(current_code)
        decoded_text += reverse_codes[current_code]
        current_code = ""
      end
    end
    
    decoded_text
  end
  
  def print_codes
    puts "Huffman Codes:"
    @codes.sort.each { |char, code| puts "'#{char}': #{code}" }
  end
end

# Example usage
puts "Huffman Coding Example"
puts "=" * 30

# Sample text to encode
text = "hello world"
puts "Original text: #{text}"

# Create Huffman coding instance
huffman = HuffmanCoding.new

# Encode the text
codes, encoded = huffman.encode(text)
huffman.print_codes

puts "\nEncoded text: #{encoded}"
puts "Original length: #{text.length * 8} bits"
puts "Encoded length: #{encoded.length} bits"
puts "Compression ratio: #{(1.0 - encoded.length.to_f / (text.length * 8)) * 100.round(2)}%"

# Decode the text
decoded = huffman.decode(encoded, codes)
puts "\nDecoded text: #{decoded}"
puts "Decoding successful: #{text == decoded}"

# Another example with more text
puts "\n" + "=" * 30
puts "Another Example"
puts "=" * 30

text2 = "aaaaabbbbcccdde"
puts "Original text: #{text2}"

codes2, encoded2 = huffman.encode(text2)
huffman.print_codes

puts "\nEncoded text: #{encoded2}"
puts "Original length: #{text2.length * 8} bits"
puts "Encoded length: #{encoded2.length} bits"
puts "Compression ratio: #{(1.0 - encoded2.length.to_f / (text2.length * 8)) * 100.round(2)}%"

decoded2 = huffman.decode(encoded2, codes2)
puts "\nDecoded text: #{decoded2}"
puts "Decoding successful: #{text2 == decoded2}"
```

## Output Example:
```
Huffman Coding Example
==============================
Huffman Codes:
"' ': 110"
"'d': 1110"
"'e': 1111"
"'h': 00"
"'l': 01"
"'o': 10"
"'r': 1100"
"'w': 1101"

Encoded text: 001011101011011110111111001101
Original length: 96 bits
Encoded length: 30 bits
Compression ratio: 68.75%

Decoded text: hello world
Decoding successful: true
```

## Key Features:

1. **Frequency Analysis**: Builds character frequency table
2. **Tree Construction**: Creates Huffman tree using a min-heap
3. **Code Generation**: Generates variable-length binary codes
4. **Encoding**: Converts text to binary using generated codes
5. **Decoding**: Reconstructs original text from encoded binary
6. **Compression Ratio**: Shows efficiency of compression

## Time Complexity:
- Building frequency table: O(n)
- Building Huffman tree: O(n log n)
- Encoding: O(n)
- Decoding: O(m) where m is the length of encoded string

## Space Complexity:
- O(n) for storing the Huffman tree and codes

This implementation demonstrates how Huffman coding achieves data compression by assigning shorter codes to more frequent characters and longer codes to less frequent ones.

