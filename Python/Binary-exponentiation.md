# Binary Exponentiation Algorithm in Python

Binary exponentiation is an efficient algorithm to compute large powers of a number. Instead of multiplying the base by itself n times, it uses the binary representation of the exponent to reduce the number of multiplications.

## Algorithm Explanation

The key insight is that any exponent can be represented in binary, and we can use this representation to compute the power more efficiently:
- If the exponent is even: `a^n = (a^(n/2))^2`
- If the exponent is odd: `a^n = a * a^(n-1)`

## Python Implementation

```python
def binary_exponentiation(base, exponent):
    """
    Compute base^exponent using binary exponentiation algorithm
    
    Args:
        base: The base number
        exponent: The exponent (non-negative integer)
    
    Returns:
        The result of base^exponent
    """
    # Handle edge cases
    if exponent == 0:
        return 1
    if exponent == 1:
        return base
    if base == 0:
        return 0
    
    # Handle negative exponents
    if exponent < 0:
        base = 1 / base
        exponent = -exponent
    
    result = 1
    
    while exponent > 0:
        # If exponent is odd, multiply result by current base
        if exponent % 2 == 1:
            result *= base
        
        # Square the base and halve the exponent
        base *= base
        exponent //= 2
    
    return result

# Example usage
if __name__ == "__main__":
    # Test cases
    print(f"2^10 = {binary_exponentiation(2, 10)}")      # Output: 1024
    print(f"3^5 = {binary_exponentiation(3, 5)}")        # Output: 243
    print(f"5^0 = {binary_exponentiation(5, 0)}")        # Output: 1
    print(f"2^-3 = {binary_exponentiation(2, -3)}")      # Output: 0.125
    print(f"7^4 = {binary_exponentiation(7, 4)}")        # Output: 2401
```

## How it works step by step (example: 2^10)

1. **Initial**: base=2, exponent=10, result=1
2. **Step 1**: exponent=10 (even) → base=4, exponent=5, result=1
3. **Step 2**: exponent=5 (odd) → base=4, exponent=2, result=4
4. **Step 3**: exponent=2 (even) → base=16, exponent=1, result=4
5. **Step 4**: exponent=1 (odd) → base=16, exponent=0, result=64

**Result**: 2^10 = 1024

## Time Complexity
- **Time**: O(log n) where n is the exponent
- **Space**: O(1)

This is much more efficient than the naive approach which would take O(n) time.

