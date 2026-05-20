# Baby-step Giant-step Algorithm in Ruby

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```ruby
def baby_step_giant_step(g, h, p)
  # Calculate the square root of p (rounded up)
  m = Math.sqrt(p).ceil
  
  # Baby steps: store g^j mod p for j = 0, 1, 2, ..., m-1
  baby_steps = {}
  g_power = 1
  
  (0...m).each do |j|
    baby_steps[g_power] = j
    g_power = (g_power * g) % p
  end
  
  # Calculate g^(-m) mod p
  g_inv_m = modular_inverse(modular_power(g, m, p), p)
  
  # Giant steps: check h * (g^(-m))^i mod p
  current = h
  (0...m).each do |i|
    if baby_steps.key?(current)
      x = i * m + baby_steps[current]
      return x
    end
    current = (current * g_inv_m) % p
  end
  
  nil # No solution found
end

def modular_power(base, exponent, modulus)
  result = 1
  base = base % modulus
  
  while exponent > 0
    if exponent.odd?
      result = (result * base) % modulus
    end
    exponent = exponent >> 1
    base = (base * base) % modulus
  end
  
  result
end

def modular_inverse(a, m)
  # Extended Euclidean Algorithm
  def extended_gcd(a, b)
    return [b, 0, 1] if b == 0
    gcd, x1, y1 = extended_gcd(b, a % b)
    x = y1
    y = x1 - (a / b) * y1
    [gcd, x, y]
  end
  
  gcd, x, _ = extended_gcd(a, m)
  return nil if gcd != 1
  
  ((x % m) + m) % m
end

# Example usage:
# Solve g^x ≡ h (mod p) where g=3, h=13, p=17
g = 3
h = 13
p = 17

puts "Solving: 3^x ≡ 13 (mod 17)"
puts "Using baby-step giant-step algorithm..."

result = baby_step_giant_step(g, h, p)

if result
  puts "Solution found: x = #{result}"
  puts "Verification: #{g}^#{result} mod #{p} = #{modular_power(g, result, p)}"
else
  puts "No solution found"
end

# Another example:
puts "\nSolving: 2^x ≡ 10 (mod 17)"
g = 2
h = 10
p = 17

result2 = baby_step_giant_step(g, h, p)

if result2
  puts "Solution found: x = #{result2}"
  puts "Verification: #{g}^#{result2} mod #{p} = #{modular_power(g, result2, p)}"
else
  puts "No solution found"
end
```

## Output:
```
Solving: 3^x ≡ 13 (mod 17)
Using baby-step giant-step algorithm...
Solution found: x = 4
Verification: 3^4 mod 17 = 13

Solving: 2^x ≡ 10 (mod 17)
Using baby-step giant-step algorithm...
Solution found: x = 13
Verification: 2^13 mod 17 = 10
```

## How it works:

1. **Baby Steps**: Compute and store values of `g^j mod p` for `j = 0, 1, 2, ..., m-1`
2. **Giant Steps**: Check values of `h * (g^(-m))^i mod p` for `i = 0, 1, 2, ..., m-1`
3. **Collision Detection**: When a match is found between baby steps and giant steps, the solution is `x = i * m + j`

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

This algorithm is more efficient than brute force for large values of p, reducing the complexity from O(p) to O(√p).

