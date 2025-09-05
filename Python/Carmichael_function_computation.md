```python
def gcd(a, b):
    """Compute greatest common divisor using Euclidean algorithm"""
    while b:
        a, b = b, a % b
    return a

def carmichael_lambda(n):
    """
    Compute the Carmichael function λ(n) for a given positive integer n
    λ(n) is the smallest positive integer m such that a^m ≡ 1 (mod n) for all a coprime to n
    """
    if n <= 0:
        return 0
    
    if n == 1:
        return 1
    
    # Get prime factorization of n
    factors = []
    temp_n = n
    
    # Check for factor 2
    if temp_n % 2 == 0:
        count = 0
        while temp_n % 2 == 0:
            temp_n //= 2
            count += 1
        factors.append((2, count))
    
    # Check for odd factors
    i = 3
    while i * i <= temp_n:
        if temp_n % i == 0:
            count = 0
            while temp_n % i == 0:
                temp_n //= i
                count += 1
            factors.append((i, count))
        i += 2
    
    # If temp_n is still > 1, then it's a prime factor
    if temp_n > 1:
        factors.append((temp_n, 1))
    
    # Compute λ(n) based on prime factorization
    result = 1
    
    for prime, power in factors:
        if prime == 2 and power >= 3:
            # For 2^k where k >= 3, λ(2^k) = 2^(k-2)
            result = result * (2 ** (power - 2))
        else:
            # For p^k where p is odd prime, λ(p^k) = p^(k-1)(p-1)
            result = result * (prime ** (power - 1)) * (prime - 1)
    
    return result

def carmichael_lambda_manual(n):
    """
    Alternative implementation that computes λ(n) by finding the LCM of
    orders of all elements in the multiplicative group Z_n^*
    """
    if n <= 0:
        return 0
    
    if n == 1:
        return 1
    
    # Find all numbers coprime to n
    coprimes = []
    for i in range(1, n):
        if gcd(i, n) == 1:
            coprimes.append(i)
    
    # For each coprime a, find its order mod n
    orders = []
    
    for a in coprimes:
        order = 1
        temp = a % n
        while temp != 1:
            temp = (temp * a) % n
            order += 1
        orders.append(order)
    
    # λ(n) is the LCM of all these orders
    from math import gcd
    
    def lcm(a, b):
        return abs(a * b) // gcd(a, b)
    
    result = orders[0]
    for i in range(1, len(orders)):
        result = lcm(result, orders[i])
    
    return result

# Example usage and testing
def test_carmichael():
    """Test the Carmichael function with some known values"""
    test_cases = [
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 2),
        (5, 4),
        (6, 2),
        (8, 2),
        (9, 6),
        (10, 4),
        (12, 4),
        (15, 4),
        (21, 6),
        (24, 2),
        (30, 4)
    ]
    
    print("Carmichael Function λ(n) Test Results:")
    print("-" * 30)
    
    for n, expected in test_cases:
        computed = carmichael_lambda(n)
        status = "✓" if computed == expected else "✗"
        print(f"{status} λ({n}) = {computed} (expected: {expected})")

# Run the test
test_carmichael()

# Example of computing specific values
print("\nDetailed Examples:")
print("=" * 30)

# Example 1: λ(12)
n = 12
print(f"Computing λ({n}):")
print(f"Prime factorization of {n}: 2² × 3¹")
print(f"λ(2²) = 2^(2-2) = 1 (since 2 ≥ 3 is false)")
print(f"λ(3¹) = 3^(1-1)(3-1) = 1 × 2 = 2")
print(f"λ({n}) = lcm(1, 2) = 2")

# Example 2: λ(21)
n = 21
print(f"\nComputing λ({n}):")
print(f"Prime factorization of {n}: 3¹ × 7¹")
print(f"λ(3¹) = 3^(1-1)(3-1) = 1 × 2 = 2")
print(f"λ(7¹) = 7^(1-1)(7-1) = 1 × 6 = 6")
print(f"λ({n}) = lcm(2, 6) = 6")

# Verification using the manual method for small numbers
print(f"\nVerification for λ(12):")
manual_result = carmichael_lambda_manual(12)
print(f"Manual method result: {manual_result}")
```


