# Binary Exponentiation Algorithm in Ruby

Binary exponentiation (also known as exponentiation by squaring) is an algorithm to compute large powers efficiently with O(log n) time complexity.

```ruby
def binary_exponentiation(base, exponent)
  # Handle edge cases
  return 1 if exponent == 0
  return base if exponent == 1
  return 1.0 / binary_exponentiation(base, -exponent) if exponent < 0
  
  result = 1
  current_base = base
  
  while exponent > 0
    # If exponent is odd, multiply result by current base
    if exponent % 2 == 1
      result *= current_base
    end
    
    # Square the base and halve the exponent
    current_base *= current_base
    exponent /= 2
  end
  
  result
end

# Alternative recursive implementation
def binary_exp_recursive(base, exponent)
  return 1 if exponent == 0
  return base if exponent == 1
  return 1.0 / binary_exp_recursive(base, -exponent) if exponent < 0
  
  if exponent % 2 == 0
    # Even exponent: (base^(n/2))^2
    half_power = binary_exp_recursive(base, exponent / 2)
    half_power * half_power
  else
    # Odd exponent: base * (base^(n-1))
    base * binary_exp_recursive(base, exponent - 1)
  end
end

# Example usage
puts "2^10 = #{binary_exponentiation(2, 10)}"        # Output: 1024
puts "3^5 = #{binary_exponentiation(3, 5)}"          # Output: 243
puts "5^0 = #{binary_exponentiation(5, 0)}"          # Output: 1
puts "2^-3 = #{binary_exponentiation(2, -3)}"        # Output: 0.125
puts "10^6 = #{binary_exponentiation(10, 6)}"        # Output: 1000000

# Compare with built-in power operator
puts "2^10 (built-in) = #{2**10}"                    # Output: 1024
```

## How it works:

1. **Base cases**: Handle exponents of 0 and 1
2. **Negative exponents**: Convert to positive and take reciprocal
3. **Main loop**: 
   - If current exponent is odd, multiply result by current base
   - Square the base and halve the exponent
   - Continue until exponent becomes 0

## Time Complexity: O(log n)
## Space Complexity: O(1) for iterative, O(log n) for recursive

The algorithm is particularly useful for computing large powers efficiently, such as in cryptography, modular arithmetic, and mathematical computations.

