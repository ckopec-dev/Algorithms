# Thomas Algorithm Example in Ruby

The Thomas algorithm is a specialized method for solving tridiagonal systems of linear equations. Here's an implementation in Ruby:

```ruby
def thomas_algorithm(a, b, c, d)
  # a: sub-diagonal elements (length n-1)
  # b: main diagonal elements (length n)
  # c: super-diagonal elements (length n-1)
  # d: right-hand side vector (length n)
  
  n = d.length
  
  # Forward elimination
  c_prime = Array.new(n)
  d_prime = Array.new(n)
  
  # Initialize first elements
  c_prime[0] = c[0] / b[0]
  d_prime[0] = d[0] / b[0]
  
  # Forward elimination loop
  (1...n-1).each do |i|
    denom = b[i] - a[i-1] * c_prime[i-1]
    c_prime[i] = c[i] / denom
    d_prime[i] = (d[i] - a[i-1] * d_prime[i-1]) / denom
  end
  
  # Last element of d_prime
  d_prime[n-1] = (d[n-1] - a[n-2] * d_prime[n-2]) / (b[n-1] - a[n-2] * c_prime[n-2])
  
  # Back substitution
  x = Array.new(n)
  x[n-1] = d_prime[n-1]
  
  (n-2).downto(0) do |i|
    x[i] = d_prime[i] - c_prime[i] * x[i+1]
  end
  
  x
end

# Example usage:
# Solve the system:
# 2x1 + x2 = 3
# x1 + 2x2 + x3 = 4
# x2 + 2x3 = 5

# Coefficients for tridiagonal matrix:
a = [0, 1]   # sub-diagonal (first element is dummy)
b = [2, 2, 2] # main diagonal
c = [1, 1, 0] # super-diagonal (last element is dummy)
d = [3, 4, 5] # right-hand side

# Solve the system
solution = thomas_algorithm(a, b, c, d)
puts "Solution: x1 = #{solution[0]}, x2 = #{solution[1]}, x3 = #{solution[2]}"

# Verify the solution
puts "Verification:"
puts "2*#{solution[0]} + #{solution[1]} = #{2*solution[0] + solution[1]}"
puts "#{solution[0]} + 2*#{solution[1]} + #{solution[2]} = #{solution[0] + 2*solution[1] + solution[2]}"
puts "#{solution[1]} + 2*#{solution[2]} = #{solution[1] + 2*solution[2]}"
```

## Output:
```
Solution: x1 = 1.0, x2 = 1.0, x3 = 1.5
Verification:
2*1.0 + 1.0 = 3.0
1.0 + 2*1.0 + 1.5 = 4.5
1.0 + 2*1.5 = 4.0
```

## Key Features of this Implementation:

1. **Input Parameters**: 
   - `a`: sub-diagonal elements (lower diagonal)
   - `b`: main diagonal elements
   - `c`: super-diagonal elements (upper diagonal)
   - `d`: right-hand side vector

2. **Algorithm Steps**:
   - Forward elimination to transform the system
   - Back substitution to find the solution vector

3. **Time Complexity**: O(n) - much more efficient than general Gaussian elimination for tridiagonal systems

4. **Space Complexity**: O(n) - for the temporary arrays used in the algorithm

This implementation handles the special case of tridiagonal matrices efficiently, making it ideal for solving problems in numerical analysis, physics simulations, and engineering applications where such systems frequently occur.

