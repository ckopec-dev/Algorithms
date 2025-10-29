# Karatsuba Multiplication Algorithm in Ruby

```ruby
def karatsuba(x, y)
  # Convert to strings to work with digits
  x = x.to_s
  y = y.to_s
  
  # Base case: if either number is single digit, multiply directly
  if x.length == 1 && y.length == 1
    return x.to_i * y.to_i
  end
  
  # Make both numbers the same length by padding with zeros
  max_len = [x.length, y.length].max
  x = x.rjust(max_len, '0')
  y = y.rjust(max_len, '0')
  
  # If length is odd, make it even
  if max_len % 2 == 1
    max_len += 1
    x = x.rjust(max_len, '0')
    y = y.rjust(max_len, '0')
  end
  
  # Split the numbers
  half = max_len / 2
  x_high = x[0...half].to_i
  x_low = x[half..-1].to_i
  y_high = y[0...half].to_i
  y_low = y[half..-1].to_i
  
  # Recursive calls
  z0 = karatsuba(x_low, y_low)
  z1 = karatsuba((x_low + x_high), (y_low + y_high))
  z2 = karatsuba(x_high, y_high)
  
  # Combine results
  return (z2 * (10 ** (2 * half))) + ((z1 - z2 - z0) * (10 ** half)) + z0
end

# Example usage
puts "Karatsuba Multiplication Examples:"
puts "-----------------------------"

# Example 1
a = 1234
b = 5678
result = karatsuba(a, b)
puts "#{a} × #{b} = #{result}"
puts "Verification: #{a * b}"

# Example 2
c = 12345
d = 67890
result2 = karatsuba(c, d)
puts "#{c} × #{d} = #{result2}"
puts "Verification: #{c * d}"

# Example 3 - Large numbers
e = 123456789
f = 987654321
result3 = karatsuba(e, f)
puts "#{e} × #{f} = #{result3}"
puts "Verification: #{e * f}"
```

## Output:
```
Karatsuba Multiplication Examples:
-----------------------------
1234 × 5678 = 7006652
Verification: 7006652
12345 × 67890 = 838102050
Verification: 838102050
123456789 × 987654321 = 121932631112635269
Verification: 121932631112635269
```

## How it works:

1. **Base Case**: If both numbers are single digits, multiply them directly
2. **Padding**: Make both numbers the same length by padding with leading zeros
3. **Split**: Divide each number into two halves
4. **Recursive Calls**: Calculate three products:
   - `z0 = low × low`
   - `z1 = (low + high) × (low + high)`
   - `z2 = high × high`
5. **Combine**: Use the formula: `z2 × 10^(2n) + (z1 - z2 - z0) × 10^n + z0`

The Karatsuba algorithm reduces the complexity from O(n²) to O(n^log₂3) ≈ O(n^1.585), making it more efficient for large numbers.

