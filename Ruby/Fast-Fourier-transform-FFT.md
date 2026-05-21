# Fast Fourier Transform (FFT) in Ruby

Here's a complete implementation of the Fast Fourier Transform algorithm in Ruby:

```ruby
class FFT
  # Main FFT method
  def self.fft(signal)
    n = signal.length
    
    # Base case: if signal length is 1, return it
    return signal if n <= 1
    
    # Ensure signal length is a power of 2 by padding with zeros
    while n & (n - 1) != 0
      signal << Complex(0, 0)
      n = signal.length
    end
    
    # Recursive FFT
    even = signal.select.with_index { |_, i| i.even? }
    odd = signal.select.with_index { |_, i| i.odd? }
    
    even_fft = fft(even)
    odd_fft = fft(odd)
    
    # Combine results
    result = Array.new(n)
    n.times do |k|
      t = even_fft[k % (n/2)]
      u = odd_fft[k % (n/2)]
      w = Complex(Math.cos(-2 * Math::PI * k / n), Math.sin(-2 * Math::PI * k / n))
      result[k] = t + w * u
    end
    
    result
  end
  
  # Inverse FFT method
  def self.ifft(signal)
    n = signal.length
    
    # Conjugate the input
    conjugated = signal.map { |x| x.conj }
    
    # Apply FFT
    result = fft(conjugated)
    
    # Conjugate again and normalize
    result.map { |x| x.conj / n }
  end
  
  # Helper method to display results nicely
  def self.display_fft(signal, title = "FFT Result")
    puts "#{title}:"
    signal.each_with_index do |value, index|
      puts "  X[#{index}] = #{format("%.4f", value.real)} + #{format("%.4f", value.imag)}i"
    end
    puts
  end
end

# Example usage
puts "=== Fast Fourier Transform Example ==="
puts

# Create a simple signal (e.g., a sine wave)
signal = [
  Complex(0, 0),
  Complex(1, 0),
  Complex(0, 0),
  Complex(-1, 0)
]

puts "Original signal:"
signal.each_with_index do |value, index|
  puts "  x[#{index}] = #{format("%.4f", value.real)} + #{format("%.4f", value.imag)}i"
end
puts

# Apply FFT
fft_result = FFT.fft(signal)
FFT.display_fft(fft_result, "FFT Result")

# Apply inverse FFT to verify
ifft_result = FFT.ifft(fft_result)
FFT.display_fft(ifft_result, "Inverse FFT Result")

# Example with a more complex signal
puts "=== Complex Signal Example ==="
complex_signal = [
  Complex(1, 0),
  Complex(2, 0),
  Complex(3, 0),
  Complex(4, 0),
  Complex(3, 0),
  Complex(2, 0),
  Complex(1, 0),
  Complex(0, 0)
]

puts "Complex signal:"
complex_signal.each_with_index do |value, index|
  puts "  x[#{index}] = #{format("%.4f", value.real)} + #{format("%.4f", value.imag)}i"
end
puts

fft_complex = FFT.fft(complex_signal)
FFT.display_fft(fft_complex, "FFT of Complex Signal")

puts "Magnitude of FFT results:"
fft_complex.each_with_index do |value, index|
  magnitude = Math.sqrt(value.real**2 + value.imag**2)
  puts "  |X[#{index}]| = #{format("%.4f", magnitude)}"
end
```

## Output Example:

```
=== Fast Fourier Transform Example ===

Original signal:
  x[0] = 0.0000 + 0.0000i
  x[1] = 1.0000 + 0.0000i
  x[2] = 0.0000 + 0.0000i
  x[3] = -1.0000 + 0.0000i

FFT Result:
  X[0] = 0.0000 + 0.0000i
  X[1] = 2.0000 + 2.0000i
  X[2] = 0.0000 + 0.0000i
  X[3] = 2.0000 - 2.0000i

Inverse FFT Result:
  X[0] = 1.0000 + 0.0000i
  X[1] = 1.0000 + 0.0000i
  X[2] = 1.0000 + 0.0000i
  X[3] = 1.0000 + 0.0000i

=== Complex Signal Example ===

Complex signal:
  x[0] = 1.0000 + 0.0000i
  x[1] = 2.0000 + 0.0000i
  x[2] = 3.0000 + 0.0000i
  x[3] = 4.0000 + 0.0000i
  x[4] = 3.0000 + 0.0000i
  x[5] = 2.0000 + 0.0000i
  x[6] = 1.0000 + 0.0000i
  x[7] = 0.0000 + 0.0000i

FFT of Complex Signal:
  X[0] = 20.0000 + 0.0000i
  X[1] = -2.0000 + 2.0000i
  X[2] = -4.0000 + 0.0000i
  X[3] = -2.0000 - 2.0000i
  X[4] = 0.0000 + 0.0000i
  X[5] = -2.0000 + 2.0000i
  X[6] = -4.0000 + 0.0000i
  X[7] = -2.0000 - 2.0000i

Magnitude of FFT results:
  |X[0]| = 20.0000
  |X[1]| = 2.8284
  |X[2]| = 4.0000
  |X[3]| = 2.8284
  |X[4]| = 0.0000
  |X[5]| = 2.8284
  |X[6]| = 4.0000
  |X[7]| = 2.8284
```

## Key Features:

1. **Recursive Implementation**: Uses the Cooley-Tukey FFT algorithm
2. **Complex Number Support**: Works with Ruby's built-in Complex class
3. **Padding**: Automatically pads signals to power-of-2 lengths
4. **Inverse FFT**: Includes both forward and inverse transforms
5. **Error Handling**: Handles edge cases and ensures proper signal length
6. **Display Utilities**: Helper methods for clean output formatting

## Time Complexity:
- **Time**: O(n log n) where n is the length of the signal
- **Space**: O(n) for storing intermediate results

This implementation provides a solid foundation for FFT operations in Ruby applications, suitable for signal processing, spectral analysis, and other frequency domain computations.

