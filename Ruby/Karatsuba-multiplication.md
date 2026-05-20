# Karatsuba Multiplication Algorithm in Ruby

```ruby
def karatsuba(x, y)
  # Convert to strings to work with digits
  x = x.to_s
  y = y.to_s
  
  # Base case: if either number is single digit
  if x.length == 1 && y.length == 1
    return x.to_i * y.to_i
  end
  
  # Make both numbers the same length by padding with zeros
  max_len = [x.length, y.length].max
  x = x.rjust(max_len, '0')
  y = y.rjust(max_len, '0')
  
  # Ensure even length
  if max_len % 2 == 1
    x = '0' + x
    y = '0' + y
    max_len += 1
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
puts "23 * 45 = #{karatsuba(23, 45)}"
puts "1234 * 5678 = #{karatsuba(1234, 5678)}"
puts "12345678 * 87654321 = #{karatsuba(12345678, 87654321)}"

# Verification with regular multiplication
puts "\nVerification:"
puts "23 * 45 = #{23 * 45}"
puts "1234 * 5678 = #{1234 * 5678}"
puts "12345678 * 87654321 = #{12345678 * 87654321}"
```

## How it works:

1. **Base Case**: If both numbers are single digits, multiply them directly
2. **Padding**: Make both numbers the same length by padding with leading zeros
3. **Split**: Divide each number into two halves
4. **Recursive Calls**: Calculate three products:
   - `z0 = low_x * low_y`
   - `z1 = (low_x + high_x) * (low_y + high_y)`
   - `z2 = high_x * high_y`
5. **Combine**: Use the formula: `z2 * 10^(2n) + (z1 - z2 - z0) * 10^n + z0`

## Time Complexity:
- **Traditional multiplication**: O(n²)
- **Karatsuba multiplication**: O(n^log₂3) ≈ O(n^1.585)

## Output:
```
Karatsuba Multiplication Examples:
23 * 45 = 1035
1234 * 5678 = 7006652
12345678 * 87654321 = 1082152022374638

Verification:
23 * 45 = 1035
1234 * 5678 = 7006652
12345678 * 87654321 = 1082152022374638
```

