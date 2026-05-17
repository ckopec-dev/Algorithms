# Quadratic Sieve Algorithm Implementation

```python
import math
import random
from collections import defaultdict

def gcd(a, b):
    """Calculate greatest common divisor"""
    while b:
        a, b = b, a % b
    return a

def is_prime(n):
    """Check if a number is prime"""
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

def factor_base_primes(limit):
    """Generate prime numbers up to limit"""
    primes = []
    for i in range(2, limit + 1):
        if is_prime(i):
            primes.append(i)
    return primes

def quadratic_sieve(n, limit=1000):
    """
    Quadratic Sieve algorithm for integer factorization
    Returns a list of prime factors of n
    """
    print(f"Factoring {n} using Quadratic Sieve")
    
    # Step 1: Choose factor base
    factor_base = factor_base_primes(limit)
    print(f"Factor base primes: {factor_base}")
    
    # Step 2: Find smooth numbers
    smooth_relations = []
    sieve_limit = int(math.sqrt(n)) + 1000
    
    # Generate relations
    for i in range(-sieve_limit, sieve_limit):
        x = i + int(math.sqrt(n))
        if x <= 0:
            continue
            
        # Calculate x^2 mod n
        x_squared_mod_n = (x * x) % n
        
        # Check if x^2 is smooth (can be factored completely over factor base)
        factors = []
        temp = x_squared_mod_n
        
        for prime in factor_base:
            while temp % prime == 0:
                factors.append(prime)
                temp //= prime
            
            if temp == 1:
                break
        
        # If we completely factored x^2 mod n, we have a smooth relation
        if temp == 1:
            smooth_relations.append((x, factors))
            print(f"x = {x}, x² ≡ {x_squared_mod_n} (mod {n})")
            print(f"Factors: {factors}")
    
    print(f"Found {len(smooth_relations)} smooth relations")
    
    # Step 3: Find linearly dependent relations
    # For simplicity, we'll use a basic approach here
    if len(smooth_relations) < len(factor_base):
        print("Not enough relations for factorization")
        return None
    
    # Simple approach: try to find a subset that gives a perfect square
    for i in range(len(smooth_relations)):
        for j in range(i + 1, len(smooth_relations)):
            x1, factors1 = smooth_relations[i]
            x2, factors2 = smooth_relations[j]
            
            # Combine factors
            combined_factors = factors1 + factors2
            factor_count = defaultdict(int)
            
            for factor in combined_factors:
                factor_count[factor] += 1
            
            # Check if all exponents are even (perfect square)
            all_even = all(count % 2 == 0 for count in factor_count.values())
            
            if all_even:
                # Calculate the square root of the product
                a = (x1 * x2) % n
                b = 1
                for factor, count in factor_count.items():
                    b *= factor ** (count // 2)
                
                # Try to find gcd
                g = gcd(a - b, n)
                if 1 < g < n:
                    print(f"Found factor: {g}")
                    return [g, n // g]
    
    return None

# Example usage
if __name__ == "__main__":
    # Example 1: Factor a small number
    print("=== Example 1: Factor 133 ===")
    result = quadratic_sieve(133)
    if result:
        print(f"Factors: {result}")
        print(f"Verification: {result[0]} × {result[1]} = {result[0] * result[1]}")
    
    print("\n=== Example 2: Factor 143 ===")
    result = quadratic_sieve(143)
    if result:
        print(f"Factors: {result}")
        print(f"Verification: {result[0]} × {result[1]} = {result[0] * result[1]}")
```

## Simplified Version for Better Understanding

```python
def simple_quadratic_sieve(n):
    """
    Simplified version of quadratic sieve for demonstration
    """
    print(f"Attempting to factor {n}")
    
    # For demonstration, let's use a simple approach
    # This is not the full quadratic sieve but shows the concept
    
    # Try small primes as factors
    factors = []
    
    for i in range(2, int(math.sqrt(n)) + 1):
        while n % i == 0:
            factors.append(i)
            n //= i
    
    if n > 1:
        factors.append(n)
    
    return factors

# Simple test
print("=== Simple Factorization ===")
print(f"Factors of 143: {simple_quadratic_sieve(143)}")
print(f"Factors of 133: {simple_quadratic_sieve(133)}")
```

## Key Components of Quadratic Sieve:

1. **Factor Base Selection**: Choose small primes that are quadratic residues modulo n
2. **Sieve Process**: Find numbers x where x² ≡ r (mod n) and r is smooth
3. **Linear Algebra**: Find linearly dependent relations to form perfect squares
4. **GCD Calculation**: Use the square to find non-trivial factors

## Time Complexity:
- The quadratic sieve has sub-exponential complexity: O(exp(√(log n log log n)))
- More efficient than trial division for large numbers

## Note:
This is a simplified implementation for educational purposes. A full quadratic sieve implementation requires more sophisticated algorithms for:
- Efficient sieving
- Linear algebra over finite fields
- Optimized smoothness testing
- Advanced matrix operations

The algorithm is most effective for numbers with 100+ digits and is one of the fastest known general-purpose factoring algorithms.

