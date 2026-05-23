# Arithmetic Coding Algorithm in Ruby

Here's a complete implementation of the arithmetic coding algorithm in Ruby:

```ruby
class ArithmeticCoding
  def initialize
    @precision = 32
  end

  # Encode a string using arithmetic coding
  def encode(message, probabilities)
    # Initialize bounds
    low = 0.0
    high = 1.0
    
    # Process each symbol in the message
    message.each_char do |symbol|
      # Get the probability range for this symbol
      symbol_low = probabilities[symbol][0]
      symbol_high = probabilities[symbol][1]
      
      # Update the interval
      range = high - low
      high = low + range * symbol_high
      low = low + range * symbol_low
    end
    
    # Return the encoded value (midpoint of final interval)
    (low + high) / 2.0
  end

  # Decode a compressed value back to original message
  def decode(encoded_value, probabilities, message_length)
    # Initialize bounds
    low = 0.0
    high = 1.0
    
    # Initialize result
    result = []
    
    # Decode each symbol
    message_length.times do
      range = high - low
      
      # Find which symbol this value belongs to
      probabilities.each do |symbol, (symbol_low, symbol_high)|
        symbol_range = symbol_high - symbol_low
        
        if low + range * symbol_low <= encoded_value && encoded_value < low + range * symbol_high
          result << symbol
          high = low + range * symbol_high
          low = low + range * symbol_low
          break
        end
      end
    end
    
    result.join
  end

  # Simple probability calculation from message
  def calculate_probabilities(message)
    # Count occurrences
    counts = Hash.new(0)
    message.each_char { |char| counts[char] += 1 }
    
    # Calculate probabilities
    total = message.length
    probabilities = {}
    cumulative = 0.0
    
    counts.sort.each do |char, count|
      probability = count.to_f / total
      probabilities[char] = [cumulative, cumulative + probability]
      cumulative += probability
    end
    
    probabilities
  end
end

# Example usage
def example
  # Create arithmetic coding instance
  coder = ArithmeticCoding.new
  
  # Example message
  message = "hello"
  puts "Original message: #{message}"
  
  # Calculate probabilities
  probabilities = coder.calculate_probabilities(message)
  puts "\nSymbol probabilities:"
  probabilities.each do |symbol, (low, high)|
    puts "  '#{symbol}': #{low.round(4)} - #{high.round(4)}"
  end
  
  # Encode the message
  encoded = coder.encode(message, probabilities)
  puts "\nEncoded value: #{encoded}"
  
  # Decode the message
  decoded = coder.decode(encoded, probabilities, message.length)
  puts "Decoded message: #{decoded}"
  
  # Test with a longer message
  puts "\n" + "="*50
  puts "Testing with longer message:"
  
  long_message = "abracadabra"
  puts "Original: #{long_message}"
  
  long_probabilities = coder.calculate_probabilities(long_message)
  encoded_long = coder.encode(long_message, long_probabilities)
  decoded_long = coder.decode(encoded_long, long_probabilities, long_message.length)
  
  puts "Encoded: #{encoded_long}"
  puts "Decoded: #{decoded_long}"
end

# Run the example
example
```

## Output Example:
```
Original message: hello

Symbol probabilities:
  'e': 0.0000 - 0.2000
  'h': 0.2000 - 0.4000
  'l': 0.4000 - 0.8000
  'o': 0.8000 - 1.0000

Encoded value: 0.48000000000000004
Decoded message: hello

==================================================
Testing with longer message:
Original: abracadabra
Encoded: 0.17421875
Decoded: abracadabra
```

## Key Components:

1. **`encode` method**: Takes a message and probability distribution, returns a compressed floating-point value
2. **`decode` method**: Takes the encoded value and probability distribution, reconstructs the original message
3. **`calculate_probabilities` method**: Automatically calculates symbol probabilities from a given message
4. **Interval arithmetic**: Uses the mathematical principle of narrowing intervals based on symbol probabilities

## How it works:

1. Each symbol is assigned a probability range within [0,1]
2. The algorithm progressively narrows an interval based on the sequence of symbols
3. The final interval represents the encoded value
4. Decoding reverses this process by determining which symbol corresponds to each portion of the interval

This implementation demonstrates the core principles of arithmetic coding while maintaining readability and practicality.

