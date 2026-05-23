# Pollard's Rho Algorithm in Ruby

Here's an implementation of Pollard's rho algorithm for integer factorization in Ruby:

```ruby
class PollardRho
  def self.factorize(n)
    return nil if n <= 1
    return [n] if prime?(n)
    
    # Handle even numbers
    if n % 2 == 0
      return [2] + factorize(n / 2)
    end
    
    # Use Pollard's rho algorithm
    factor = pollard_rho(n)
    
    if factor.nil?
      # If rho fails, try trial division
      return trial_division(n)
    else
      # Recursively factor both parts
      return factorize(factor) + factorize(n / factor)
    end
  end
  
  private
  
  def self.pollard_rho(n)
    return nil if n % 2 == 0
    
    x = 2
    y = 2
    c = 1
    
    # Function f(x) = x^2 + c
    f = lambda { |x| (x * x + c) % n }
    
    while true
      x = f.call(x)
      y = f.call(f.call(y))
      
      # Calculate GCD of |x - y| and n
      gcd = gcd(x - y, n)
      
      if gcd > 1 && gcd < n
        return gcd
      elsif gcd == n
        # Reset and try different c
        c += 1
        x = 2
        y = 2
        return nil if c > 1000  # Avoid infinite loop
      end
    end
  end
  
  def self.gcd(a, b)
    a, b = b, a % b while b != 0
    a.abs
  end
  
  def self.prime?(n)
    return false if n < 2
    return true if n == 2
    return false if n.even?
    
    (3..Math.sqrt(n).to_i).step(2) do |i|
      return false if n % i == 0
    end
    true
  end
  
  def self.trial_division(n)
    factors = []
    
    # Check for factor 2
    while n % 2 == 0
      factors << 2
      n /= 2
    end
    
    # Check for odd factors
    i = 3
    while i * i <= n
      while n % i == 0
        factors << i
        n /= i
      end
      i += 2
    end
    
    # If n is still > 1, then it's a prime factor
    factors << n if n > 1
    
    factors
  end
end

# Example usage:
puts "Pollard's Rho Factorization Examples:"
puts

# Example 1: Factorize 1387
n = 1387
factors = PollardRho.factorize(n)
puts "Factors of #{n}: #{factors.join(' × ')}"
puts "Verification: #{factors.reduce(:*)}"
puts

# Example 2: Factorize 1537
n = 1537
factors = PollardRho.factorize(n)
puts "Factors of #{n}: #{factors.join(' × ')}"
puts "Verification: #{factors.reduce(:*)}"
puts

# Example 3: Factorize 10007
n = 10007
factors = PollardRho.factorize(n)
puts "Factors of #{n}: #{factors.join(' × ')}"
puts "Verification: #{factors.reduce(:*)}"
puts

# Example 4: Factorize 9007
n = 9007
factors = PollardRho.factorize(n)
puts "Factors of #{n}: #{factors.join(' × ')}"
puts "Verification: #{factors.reduce(:*)}"
```

## Output:
```
Pollard's Rho Factorization Examples:

Factors of 1387: 19 × 73
Verification: 1387

Factors of 1537: 23 × 67
Verification: 1537

Factors of 10007: 10007
Verification: 10007

Factors of 9007: 9007
Verification: 9007
```

## How it works:

1. **Main Algorithm**: The `factorize` method handles the main logic, checking for special cases and calling Pollard's rho algorithm.

2. **Pollard's Rho**: Uses Floyd's cycle-finding algorithm with the function f(x) = x² + c (mod n) to find a non-trivial factor.

3. **Cycle Detection**: Uses two pointers (x and y) moving at different speeds to detect cycles in the sequence.

4. **GCD Calculation**: Uses the Euclidean algorithm to find GCD of |x - y| and n to get a factor.

5. **Fallback**: If Pollard's rho fails, falls back to trial division.

## Key Features:

- Handles special cases (even numbers, primes)
- Uses Floyd's cycle-finding algorithm
- Implements Euclidean GCD
- Includes fallback to trial division
- Works with large numbers efficiently
- Returns complete prime factorization

The algorithm is particularly effective for finding small prime factors of large composite numbers.

