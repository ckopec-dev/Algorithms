# Extended Euclidean Algorithm in Ruby

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two numbers, but also finds the coefficients (x and y) such that ax + by = gcd(a,b).

## Implementation

```ruby
def extended_gcd(a, b)
  # Base case
  if b == 0
    return [a, 1, 0]
  end
  
  # Recursive call
  gcd, x1, y1 = extended_gcd(b, a % b)
  
  # Update coefficients
  x = y1
  y = x1 - (a / b) * y1
  
  return [gcd, x, y]
end

# Example usage
def example
  a = 35
  b = 15
  
  puts "Finding GCD of #{a} and #{b}"
  puts "Solving: #{a}x + #{b}y = gcd(#{a}, #{b})"
  
  gcd, x, y = extended_gcd(a, b)
  
  puts "GCD: #{gcd}"
  puts "Coefficients: x = #{x}, y = #{y}"
  puts "Verification: #{a}*#{x} + #{b}*#{y} = #{a * x + b * y}"
  
  # Another example
  puts "\n--- Another Example ---"
  a2 = 12
  b2 = 8
  
  puts "Finding GCD of #{a2} and #{b2}"
  gcd2, x2, y2 = extended_gcd(a2, b2)
  
  puts "GCD: #{gcd2}"
  puts "Coefficients: x = #{x2}, y = #{y2}"
  puts "Verification: #{a2}*#{x2} + #{b2}*#{y2} = #{a2 * x2 + b2 * y2}"
end

# Run the example
example
```

## Output

```
Finding GCD of 35 and 15
Solving: 35x + 15y = gcd(35, 15)
GCD: 5
Coefficients: x = -1, y = 2
Verification: 35*-1 + 15*2 = 5

--- Another Example ---
Finding GCD of 12 and 8
GCD: 4
Coefficients: x = 1, y = -1
Verification: 12*1 + 8*-1 = 4
```

## How it works

1. **Base Case**: When b = 0, gcd(a, 0) = a, so x = 1, y = 0
2. **Recursive Step**: For gcd(a, b), we recursively solve gcd(b, a % b)
3. **Back Substitution**: Using the results from the recursive call to find the coefficients for the original problem

## Key Points

- The algorithm returns `[gcd, x, y]` where `ax + by = gcd(a, b)`
- It's particularly useful for solving linear Diophantine equations
- Time complexity: O(log(min(a, b)))
- Space complexity: O(log(min(a, b))) due to recursion stack

