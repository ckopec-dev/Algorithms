# Sieve of Atkin Algorithm in Ruby

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer, which is more efficient than the classic Sieve of Eratosthenes for large numbers.

```ruby
def sieve_of_atkin(limit)
  return [] if limit < 2
  
  # Initialize boolean array and set all entries as true
  is_prime = Array.new(limit + 1, false)
  
  # Mark 2 and 3 as prime
  is_prime[2] = true
  is_prime[3] = true
  
  # Apply the Sieve of Atkin algorithm
  (1..Math.sqrt(limit)).each do |x|
    (1..Math.sqrt(limit)).each do |y|
      # Generate potential primes using the three formulas
      n = 4 * x * x + y * y
      
      # First formula: 4x² + y² = n, where n % 12 = 1 or n % 12 = 5
      if n <= limit && (n % 12 == 1 || n % 12 == 5)
        is_prime[n] = !is_prime[n]
      end
      
      # Second formula: 3x² + y² = n, where n % 12 = 7
      n = 3 * x * x + y * y
      if n <= limit && n % 12 == 7
        is_prime[n] = !is_prime[n]
      end
      
      # Third formula: 3x² - y² = n, where x > y and n % 12 = 11
      n = 3 * x * x - y * y
      if n <= limit && x > y && n % 12 == 11
        is_prime[n] = !is_prime[n]
      end
    end
  end
  
  # Remove multiples of square numbers
  (5..Math.sqrt(limit)).each do |i|
    if is_prime[i]
      # Mark all multiples of i² as not prime
      (i * i).step(limit, i * i) do |j|
        is_prime[j] = false
      end
    end
  end
  
  # Collect all prime numbers
  primes = []
  (2..limit).each do |i|
    primes << i if is_prime[i]
  end
  
  primes
end

# Example usage
puts "Prime numbers up to 30:"
primes = sieve_of_atkin(30)
puts primes.join(", ")

puts "\nPrime numbers up to 50:"
primes = sieve_of_atkin(50)
puts primes.join(", ")

# Test with a larger number
puts "\nFirst 10 prime numbers:"
primes = sieve_of_atkin(100)
puts primes.first(10).join(", ")
```

## Output:
```
Prime numbers up to 30:
2, 3, 5, 7, 11, 13, 17, 19, 23, 29

Prime numbers up to 50:
2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47

First 10 prime numbers:
2, 3, 5, 7, 11, 13, 17, 19, 23, 29
```

## How it works:

1. **Initialization**: Create a boolean array to track prime numbers
2. **Formula Application**: Use three mathematical formulas to mark potential primes:
   - 4x² + y² = n where n % 12 = 1 or 5
   - 3x² + y² = n where n % 12 = 7  
   - 3x² - y² = n where x > y and n % 12 = 11
3. **Toggle Marks**: Toggle the prime status for numbers that satisfy the formulas
4. **Square Removal**: Remove multiples of square numbers (like 4, 9, 25, etc.)
5. **Collect Results**: Gather all numbers marked as prime

The algorithm has a time complexity of O(n / log log n) and is particularly efficient for finding large prime numbers.

