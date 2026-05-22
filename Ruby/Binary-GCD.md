# Binary GCD Algorithm in Ruby

Here's an implementation of the Binary GCD (Stein's Algorithm) in Ruby:

```ruby
def binary_gcd(a, b)
  # Handle edge cases
  return a if b == 0
  return b if a == 0
  
  # Handle negative numbers
  a = a.abs
  b = b.abs
  
  # Count common factors of 2
  shift = 0
  while ((a | b) & 1) == 0
    a >>= 1
    b >>= 1
    shift += 1
  end
  
  # Remove remaining factors of 2 from a
  while (a & 1) == 0
    a >>= 1
  end
  
  loop do
    # Remove factors of 2 from b
    while (b & 1) == 0
      b >>= 1
    end
    
    # Ensure a <= b
    if a > b
      a, b = b, a
    end
    
    # Subtract smaller from larger
    b = b - a
    
    # If b becomes 0, we're done
    break if b == 0
  end
  
  # Restore common factors of 2
  a << shift
end

# Example usage
puts "Binary GCD Examples:"
puts "binary_gcd(48, 18) = #{binary_gcd(48, 18)}"    # Output: 6
puts "binary_gcd(100, 25) = #{binary_gcd(100, 25)}"  # Output: 25
puts "binary_gcd(17, 13) = #{binary_gcd(17, 13)}"    # Output: 1
puts "binary_gcd(0, 5) = #{binary_gcd(0, 5)}"        # Output: 5
puts "binary_gcd(-12, 8) = #{binary_gcd(-12, 8)}"    # Output: 4
```

## How it works:

1. **Handle edge cases**: If either number is 0, return the other number
2. **Normalize**: Convert to positive numbers since GCD is always positive
3. **Count common 2 factors**: Use bit shifting to find how many common factors of 2 exist
4. **Remove 2 factors**: Remove all factors of 2 from both numbers
5. **Iterative subtraction**: Repeatedly subtract the smaller number from the larger until one becomes 0
6. **Restore factors**: Multiply the result by the common factors of 2 that were removed

## Time Complexity:
- **Time**: O(log(min(a,b)))
- **Space**: O(1)

This algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations instead of division.

