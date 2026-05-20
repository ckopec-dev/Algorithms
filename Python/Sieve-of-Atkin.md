# Sieve of Atkin Algorithm in Python

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer. It's more efficient than the classic Sieve of Eratosthenes for large ranges.

```python
def sieve_of_atkin(limit):
    """
    Find all prime numbers up to limit using the Sieve of Atkin algorithm.
    
    Args:
        limit (int): The upper bound to find primes up to
    
    Returns:
        list: List of prime numbers up to limit
    """
    if limit < 2:
        return []
    
    # Initialize the sieve array
    # All numbers are initially considered potential primes
    is_prime = [False] * (limit + 1)
    
    # Mark 2 and 3 as prime
    if limit >= 2:
        is_prime[2] = True
    if limit >= 3:
        is_prime[3] = True
    
    # Apply the Atkin sieve
    for x in range(1, int(limit**0.5) + 1):
        for y in range(1, int(limit**0.5) + 1):
            # Generate potential primes using the three quadratic forms
            n = 4 * x * x + y * y
            
            if n <= limit and (n % 12 == 1 or n % 12 == 5):
                is_prime[n] = not is_prime[n]
            
            n = 3 * x * x + y * y
            
            if n <= limit and n % 12 == 7:
                is_prime[n] = not is_prime[n]
            
            n = 3 * x * x - y * y
            
            if x > y and n <= limit and n % 12 == 11:
                is_prime[n] = not is_prime[n]
    
    # Remove multiples of squares of primes
    for i in range(5, int(limit**0.5) + 1):
        if is_prime[i]:
            # Mark all multiples of i*i as not prime
            for j in range(i * i, limit + 1, i * i):
                is_prime[j] = False
    
    # Collect all primes
    primes = []
    for i in range(2, limit + 1):
        if is_prime[i]:
            primes.append(i)
    
    return primes

# Example usage
if __name__ == "__main__":
    # Find all primes up to 100
    primes = sieve_of_atkin(100)
    print("Prime numbers up to 100:")
    print(primes)
    print(f"\nCount of primes: {len(primes)}")
    
    # Find primes up to 30
    primes_30 = sieve_of_atkin(30)
    print("\nPrime numbers up to 30:")
    print(primes_30)
    
    # Find primes up to 10
    primes_10 = sieve_of_atkin(10)
    print("\nPrime numbers up to 10:")
    print(primes_10)
```

## Output:
```
Prime numbers up to 100:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

Count of primes: 25

Prime numbers up to 30:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

Prime numbers up to 10:
[2, 3, 5, 7]
```

## How it works:

1. **Initialization**: Create a boolean array to track potential primes
2. **Quadratic Forms**: Use three specific quadratic equations to mark potential primes:
   - `4x² + y² = n` where `n % 12 == 1 or n % 12 == 5`
   - `3x² + y² = n` where `n % 12 == 7`
   - `3x² - y² = n` where `x > y` and `n % 12 == 11`
3. **Toggle Flags**: For each valid `n`, toggle its prime flag
4. **Remove Squares**: Eliminate multiples of squares of primes
5. **Collect Results**: Gather all numbers marked as prime

The algorithm has a time complexity of O(n / log log n) and is particularly efficient for finding large primes.

