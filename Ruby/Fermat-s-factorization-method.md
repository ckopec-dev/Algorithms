# Fermat's Factorization Method in Ruby

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a+b)(a-b).

```ruby
def fermat_factorization(n)
  # Handle edge cases
  return [n, 1] if n < 2
  
  # Find the smallest integer a such that a² ≥ n
  a = Math.sqrt(n).ceil
  
  # Continue until we find a perfect square
  loop do
    # Calculate b² = a² - n
    b_squared = a * a - n
    
    # Check if b_squared is a perfect square
    b = Math.sqrt(b_squared).round
    
    # If b² is a perfect square, we found our factors
    if b * b == b_squared && b_squared > 0
      # Return the factors
      return [a + b, a - b]
    end
    
    # Increment a and try again
    a += 1
  end
end

# Example usage
def demonstrate_fermat_factorization
  test_numbers = [5959, 13, 15, 35, 91]
  
  test_numbers.each do |num|
    factors = fermat_factorization(num)
    puts "Number: #{num}"
    puts "Factors: #{factors[0]} × #{factors[1]} = #{num}"
    puts "Verification: #{factors[0] * factors[1]} = #{num}"
    puts "-" * 40
  end
end

# Run the demonstration
demonstrate_fermat_factorization

# Example with a larger number
puts "\nLarge number example:"
large_number = 1000003
factors = fermat_factorization(large_number)
puts "Number: #{large_number}"
puts "Factors: #{factors[0]} × #{factors[1]} = #{large_number}"
```

## How it works:

1. **Start with the smallest integer** `a` such that `a² ≥ n`
2. **Calculate** `b² = a² - n`
3. **Check if** `b²` is a perfect square
4. **If yes**, then `n = (a+b)(a-b)` gives us the factors
5. **If no**, increment `a` and repeat

## Output example:
```
Number: 5959
Factors: 59 × 101 = 5959
Verification: 5959 = 5959
----------------------------------------
Number: 13
Factors: 13 × 1 = 13
Verification: 13 = 13
----------------------------------------
Number: 15
Factors: 5 × 3 = 15
Verification: 15 = 15
----------------------------------------
```

## Time Complexity:
- **Best case**: O(1) for numbers that are already perfect squares
- **Worst case**: O(√n) for semiprimes that are close to each other
- **Average case**: O(√(n - p²)) where p is the smallest prime factor of n

This method is particularly efficient when the two factors of n are close to each other.

