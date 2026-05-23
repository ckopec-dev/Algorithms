# Binary GCD Algorithm (Stein's Algorithm) in Python

```python
def binary_gcd(a, b):
    """
    Calculate the Greatest Common Divisor (GCD) using Binary GCD algorithm (Stein's algorithm)
    
    Args:
        a (int): First positive integer
        b (int): Second positive integer
    
    Returns:
        int: GCD of a and b
    """
    # Handle edge cases
    if a == 0:
        return b
    if b == 0:
        return a
    
    # Count common factors of 2
    shift = 0
    while ((a | b) & 1) == 0:
        a >>= 1
        b >>= 1
        shift += 1
    
    # Remove remaining factors of 2 from a
    while (a & 1) == 0:
        a >>= 1
    
    while b != 0:
        # Remove factors of 2 from b
        while (b & 1) == 0:
            b >>= 1
        
        # Ensure a <= b
        if a > b:
            a, b = b, a
        
        b = b - a
    
    # Restore common factors of 2
    return a << shift

# Example usage
if __name__ == "__main__":
    # Test cases
    test_cases = [
        (48, 18),
        (100, 25),
        (17, 13),
        (0, 5),
        (7, 0),
        (1024, 512)
    ]
    
    print("Binary GCD Algorithm Examples:")
    print("-" * 30)
    
    for a, b in test_cases:
        result = binary_gcd(a, b)
        print(f"GCD({a}, {b}) = {result}")
```

## Output:
```
Binary GCD Algorithm Examples:
------------------------------
GCD(48, 18) = 6
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(0, 5) = 5
GCD(7, 0) = 7
GCD(1024, 512) = 512
```

## How it works:

1. **Remove common factors of 2**: Count how many common factors of 2 both numbers have
2. **Remove remaining factors of 2**: Remove all factors of 2 from one number
3. **Apply subtraction**: Use the subtraction method (similar to Euclidean algorithm) to find GCD
4. **Restore factors**: Multiply the result by the common factors of 2 that were removed

## Time Complexity:
- **Time**: O(log(min(a, b)))
- **Space**: O(1)

The binary GCD algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations instead of division operations.

