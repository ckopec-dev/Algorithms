# Binary Exponentiation Algorithm in Ruby

Binary exponentiation is an efficient algorithm to compute large powers of a number. Instead of multiplying the base by itself n times, it uses the binary representation of the exponent to reduce the number of multiplications.

## Implementation

```ruby
def binary_exponentiation(base, exponent)
  # Handle edge cases
  return 1 if exponent == 0
  return base if exponent == 1
  return 0 if base == 0
  
  # Handle negative exponents
  if exponent < 0
    base = 1.0 / base
    exponent = -exponent
  end
  
  result = 1
  
  while exponent > 0
    # If exponent is odd, multiply result by base
    if exponent.odd?
      result *= base
    end
    
    # Square the base and halve the exponent
    base *= base
    exponent /= 2
  end
  
  result
end

# Example usage
puts binary_exponentiation(2, 10)   # Output: 1024
puts binary_exponentiation(3, 4)    # Output: 81
puts binary_exponentiation(5, 0)    # Output: 1
puts binary_exponentiation(2, -3)   # Output: 0.125
```

## How it works:

1. **Base cases**: Handle exponent = 0 (return 1) and exponent = 1 (return base)
2. **Negative exponents**: Convert to positive exponent and take reciprocal
3. **Main loop**: 
   - If current exponent is odd, multiply result by current base
   - Square the base and halve the exponent
4. **Repeat** until exponent becomes 0

## Time Complexity: O(log n)
## Space Complexity: O(1)

The algorithm reduces the number of multiplications from O(n) to O(log n) by leveraging the binary representation of the exponent.

