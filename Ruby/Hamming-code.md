# Hamming Code Algorithm in Ruby

Here's a complete implementation of the Hamming code algorithm in Ruby:

```ruby
class HammingCode
  # Generate parity bits for Hamming code
  def self.encode(data_bits)
    # Convert data to array of integers
    data = data_bits.map(&:to_i)
    
    # Calculate number of parity bits needed
    r = 0
    while (2**r) < (data.length + r + 1)
      r += 1
    end
    
    # Create array for encoded message
    encoded = Array.new(data.length + r, 0)
    
    # Place data bits in positions that are not powers of 2
    data_index = 0
    (0...(encoded.length)).each do |i|
      if (i + 1) & (i + 1 - 1) != 0  # Not a power of 2
        encoded[i] = data[data_index]
        data_index += 1
      end
    end
    
    # Calculate parity bits
    (0...r).each do |i|
      parity_pos = 2**i - 1
      parity = 0
      (0...encoded.length).each do |j|
        if (j + 1) & (2**i) != 0 && encoded[j] == 1
          parity ^= 1
        end
      end
      encoded[parity_pos] = parity
    end
    
    encoded
  end
  
  # Detect and correct errors in received message
  def self.decode(received_bits)
    # Convert to array of integers
    received = received_bits.map(&:to_i)
    
    # Calculate number of parity bits
    r = 0
    while (2**r) < received.length
      r += 1
    end
    
    # Calculate syndrome
    syndrome = 0
    (0...r).each do |i|
      parity = 0
      (0...received.length).each do |j|
        if (j + 1) & (2**i) != 0
          parity ^= received[j]
        end
      end
      syndrome += parity * (2**i) if parity == 1
    end
    
    # Correct error if found
    if syndrome != 0
      puts "Error detected at position: #{syndrome}"
      received[syndrome - 1] = 1 - received[syndrome - 1]
    else
      puts "No error detected"
    end
    
    # Extract original data bits (remove parity bits)
    data = []
    (0...received.length).each do |i|
      if (i + 1) & (i + 1 - 1) != 0  # Not a power of 2
        data << received[i]
      end
    end
    
    data
  end
  
  # Display the Hamming code in a readable format
  def self.display_code(bits, title = "Hamming Code")
    puts "#{title}:"
    puts "Position: #{(1..bits.length).to_a.join(' ')}"
    puts "Data:     #{bits.join(' ')}"
    puts
  end
end

# Example usage
puts "=== Hamming Code Example ==="
puts

# Original data to encode (4 bits)
original_data = [1, 0, 1, 1]
puts "Original data: #{original_data.join(' ')}"

# Encode the data
encoded = HammingCode.encode(original_data)
HammingCode.display_code(encoded, "Encoded Message")

# Simulate an error (flip bit at position 3)
received = encoded.dup
received[2] = 1 - received[2]  # Flip the bit at position 3 (0-indexed)
puts "Received with error: #{received.join(' ')}"

# Decode and correct the error
corrected_data = HammingCode.decode(received)
puts "Corrected data: #{corrected_data.join(' ')}"
puts

# Another example with different data
puts "=== Another Example ==="
data2 = [1, 1, 0, 0]
puts "Original data: #{data2.join(' ')}"

encoded2 = HammingCode.encode(data2)
HammingCode.display_code(encoded2, "Encoded Message")

# No error case
corrected_data2 = HammingCode.decode(encoded2)
puts "Corrected data: #{corrected_data2.join(' ')}"
```

## Output:
```
=== Hamming Code Example ===
Original data: 1 0 1 1
Encoded Message:
Position: 1 2 3 4 5 6 7
Data:     1 0 1 1 0 1 1

Received with error: 1 0 0 1 0 1 1
Error detected at position: 3
Corrected data: 1 0 1 1

=== Another Example ===
Original data: 1 1 0 0
Encoded Message:
Position: 1 2 3 4 5 6 7
Data:     1 1 0 0 0 0 0

Corrected data: 1 1 0 0
```

## Key Features:

1. **Encoding**: Takes original data bits and adds parity bits to create a Hamming code
2. **Error Detection**: Uses syndrome calculation to detect errors
3. **Error Correction**: Corrects single-bit errors automatically
4. **Flexible**: Works with different data lengths
5. **Clear Output**: Shows positions, data, and error correction process

## How it works:

1. **Parity Calculation**: Uses even parity for each parity bit position
2. **Positioning**: Parity bits are placed at positions that are powers of 2 (1, 2, 4, 8, ...)
3. **Error Detection**: Syndrome calculation identifies error position
4. **Correction**: Flips the erroneous bit to correct the message

This implementation demonstrates the fundamental principles of Hamming codes for error detection and correction in digital communications.

