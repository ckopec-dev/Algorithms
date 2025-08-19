```python
def binary_exponentiation(base, exponent):
    """
    Calculate base^exponent using binary exponentiation algorithm
    Time complexity: O(log n)
    """
    if exponent == 0:
        return 1
    
    result = 1
    current_base = base
    
    while exponent > 0:
        # If exponent is odd, multiply result by current base
        if exponent % 2 == 1:
            result *= current_base
        
        # Square the base and halve the exponent
        current_base *= current_base
        exponent //= 2
    
    return result

# Example usage
print(binary_exponentiation(2, 10))  # Output: 1024
print(binary_exponentiation(3, 5))   # Output: 243
print(binary_exponentiation(5, 0))   # Output: 1
print(binary_exponentiation(2, 8))   # Output: 256

# Step-by-step trace for binary_exponentiation(2, 10):
# exponent = 10 (even) -> current_base = 4, exponent = 5
# exponent = 5 (odd)  -> result = 1 * 4 = 4, current_base = 16, exponent = 2
# exponent = 2 (even) -> current_base = 256, exponent = 1
# exponent = 1 (odd)  -> result = 4 * 256 = 1024, current_base = 65536, exponent = 0
# Return 1024
```

