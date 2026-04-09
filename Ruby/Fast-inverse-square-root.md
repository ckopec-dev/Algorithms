# Fast Inverse Square Root in Ruby

```ruby
def fast_inverse_sqrt(number)
  # Handle edge cases
  return 0.0 if number <= 0
  
  # Convert float to integer representation
  i = number.to_i
  
  # Magic number for fast inverse square root
  threehalfs = 1.5
  x2 = number * 0.5
  y = number
  
  # Bit manipulation magic (similar to the original C version)
  i = 0x5f3759df - (i >> 1)
  
  # Convert back to float
  y = i.to_f
  
  # First Newton-Raphson iteration
  y = y * (threehalfs - (x2 * y * y))
  
  # Second Newton-Raphson iteration (optional but more accurate)
  y = y * (threehalfs - (x2 * y * y))
  
  return y
end

# Example usage
puts "Fast inverse square root examples:"
puts "1/sqrt(4) = #{fast_inverse_sqrt(4)}"
puts "1/sqrt(9) = #{fast_inverse_sqrt(9)}"
puts "1/sqrt(16) = #{fast_inverse_sqrt(16)}"

# Compare with standard method
puts "\nComparison with standard method:"
puts "Standard 1/sqrt(4) = #{1.0 / Math.sqrt(4)}"
puts "Fast 1/sqrt(4) = #{fast_inverse_sqrt(4)}"
puts "Difference: #{(1.0 / Math.sqrt(4) - fast_inverse_sqrt(4)).abs}"
```

**Note:** The above Ruby implementation is a conceptual representation. The actual fast inverse square root algorithm relies heavily on bit manipulation and IEEE 754 floating-point representation, which is more complex to implement accurately in Ruby due to its higher-level nature. The original algorithm was designed specifically for C with direct bit manipulation capabilities.

