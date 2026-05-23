# Sieve of Eratosthenes in Ruby

```ruby
def sieve_of_eratosthenes(n)
  # Create an array of boolean values, initially all true
  # Index represents the number, value represents if it's prime
  primes = Array.new(n + 1, true)
  primes[0] = primes[1] = false  # 0 and 1 are not prime numbers
  
  # Start with the first prime number, 2
  (2..Math.sqrt(n)).each do |i|
    if primes[i]
      # Mark all multiples of i as not prime
      (i * i).step(n, i) do |j|
        primes[j] = false
      end
    end
  end
  
  # Collect all prime numbers
  (2..n).select { |i| primes[i] }
end

# Example usage
puts "Prime numbers up to 30:"
primes = sieve_of_eratosthenes(30)
puts primes.join(", ")

# Output: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29
```

## How it works:

1. **Initialize**: Create a boolean array where each index represents a number and the value indicates if it's prime
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as not prime
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Collect results**: Gather all numbers that remain marked as prime

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

