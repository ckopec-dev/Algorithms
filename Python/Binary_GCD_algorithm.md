```python
def binary_gcd(a, b):
    """
    Binary GCD algorithm (Stein's algorithm)
    Returns the greatest common divisor of a and b
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
    print(f"binary_gcd(48, 18) = {binary_gcd(48, 18)}")  # Output: 6
    print(f"binary_gcd(100, 25) = {binary_gcd(100, 25)}")  # Output: 25
    print(f"binary_gcd(17, 13) = {binary_gcd(17, 13)}")  # Output: 1
    print(f"binary_gcd(0, 5) = {binary_gcd(0, 5)}")  # Output: 5
```


