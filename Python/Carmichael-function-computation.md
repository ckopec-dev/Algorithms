# Carmichael Function Computation in Python

The Carmichael function λ(n) (also known as the reduced totient function) is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.

```python
def gcd(a, b):
    """Compute greatest common divisor using Euclidean algorithm"""
    while b:
        a, b = b, a % b
    return a

def is_coprime(a, b):
    """Check if two numbers are coprime"""
    return gcd(a, b) == 1

def prime_factors(n):
    """Get prime factorization of n"""
    factors = []
    d = 2
    while d * d <= n:
        while n % d == 0:
            factors.append(d)
            n //= d
        d += 1
    if n > 1:
        factors.append(n)
    return factors

def carmichael_function(n):
    """
    Compute the Carmichael function λ(n)
    
    For a number n with prime factorization n = p1^a1 * p2^a2 * ... * pk^ak,
    λ(n) = lcm(φ(p1^a1), φ(p2^a2), ..., φ(pk^ak))
    where φ is Euler's totient function
    
    Special cases:
    - λ(1) = 1
    - λ(2) = 1
    - λ(2^k) = 2^(k-1) for k ≥ 3
    - λ(p^k) = p^(k-1)(p-1) for odd prime p
    """
    if n == 1:
        return 1
    
    # Get prime factorization
    factors = prime_factors(n)
    
    # Count prime factor multiplicities
    factor_counts = {}
    for factor in factors:
        factor_counts[factor] = factor_counts.get(factor, 0) + 1
    
    # Calculate λ(n) using the formula
    lambda_n = 1
    
    for prime, count in factor_counts.items():
        if prime == 2 and count >= 3:
            # Special case for powers of 2 >= 8
            lambda_n = lcm(lambda_n, 2**(count-2))
        elif prime == 2 and count == 2:
            # λ(4) = 2
            lambda_n = lcm(lambda_n, 2)
        elif prime == 2 and count == 1:
            # λ(2) = 1
            lambda_n = lcm(lambda_n, 1)
        else:
            # For odd primes: λ(p^k) = p^(k-1)(p-1)
            lambda_n = lcm(lambda_n, (prime**(count-1)) * (prime-1))
    
    return lambda_n

def lcm(a, b):
    """Compute least common multiple"""
    return abs(a * b) // gcd(a, b)

def euler_totient(n):
    """Compute Euler's totient function φ(n)"""
    if n == 1:
        return 1
    
    result = n
    p = 2
    
    while p * p <= n:
        if n % p == 0:
            while n % p == 0:
                n //= p
            result -= result // p
        p += 1
    
    if n > 1:
        result -= result // n
    
    return result

# Example usage and testing
def test_carmichael_function():
    """Test the Carmichael function with examples"""
    test_cases = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16, 20, 21, 24, 25, 27, 30]
    
    print("n\tλ(n)\tφ(n)\tVerification")
    print("-" * 40)
    
    for n in test_cases:
        lambda_n = carmichael_function(n)
        phi_n = euler_totient(n)
        
        # Verify that λ(n) divides φ(n)
        verification = "✓" if phi_n % lambda_n == 0 else "✗"
        
        print(f"{n}\t{lambda_n}\t{phi_n}\t{verification}")

# Run the test
if __name__ == "__main__":
    test_carmichael_function()
    
    # Additional examples
    print("\nDetailed examples:")
    print(f"λ(12) = {carmichael_function(12)}")
    print(f"λ(15) = {carmichael_function(15)}")
    print(f"λ(20) = {carmichael_function(20)}")
    
    # Verification that a^λ(n) ≡ 1 (mod n) for some a coprime to n
    print("\nVerification examples:")
    n = 15
    lambda_n = carmichael_function(n)
    a = 2  # a coprime to 15
    
    if is_coprime(a, n):
        result = (a ** lambda_n) % n
        print(f"For n={n}, a={a}: {a}^{lambda_n} ≡ {result} (mod {n})")
```

## Output Example:
```
n	λ(n)	φ(n)	Verification
----------------------------------------
1	1	1	✓
2	1	1	✓
3	2	2	✓
4	2	2	✓
5	4	4	✓
6	2	2	✓
7	6	6	✓
8	2	4	✓
9	6	6	✓
10	4	4	✓
12	2	4	✓
15	4	8	✓
16	4	8	✓
20	4	8	✓
21	6	12	✓
24	2	8	✓
25	20	20	✓
27	18	18	✓
30	4	8	✓

Detailed examples:
λ(12) = 2
λ(15) = 4
λ(20) = 4

Verification examples:
For n=15, a=2: 2^4 ≡ 1 (mod 15)
```

## Key Points:

1. **Algorithm Logic**: The Carmichael function uses the prime factorization of n to compute the least common multiple of the values λ(p^k) for each prime power in the factorization.

2. **Special Cases**: 
   - λ(2^k) = 2^(k-1) for k ≥ 3
   - λ(2^2) = 2
   - λ(2^1) = 1
   - λ(p^k) = p^(k-1)(p-1) for odd primes p

3. **Mathematical Significance**: The Carmichael function gives the smallest exponent m such that a^m ≡ 1 (mod n) for all a coprime to n, making it useful in cryptography and number theory.

4. **Time Complexity**: O(√n) for prime factorization, making it efficient for reasonable inputs.

