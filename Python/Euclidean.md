# Euclidean Algorithm in Python

The Euclidean algorithm is an efficient method for computing the greatest common divisor (GCD) of two integers.

## Iterative Implementation

```python
def gcd_iterative(a, b):
    """
    Compute GCD using iterative Euclidean algorithm
    """
    while b:
        a, b = b, a % b
    return abs(a)

# Example usage
result = gcd_iterative(48, 18)
print(f"GCD of 48 and 18 is: {result}")  # Output: 6
```

## Recursive Implementation

```python
def gcd_recursive(a, b):
    """
    Compute GCD using recursive Euclidean algorithm
    """
    if b == 0:
        return abs(a)
    return gcd_recursive(b, a % b)

# Example usage
result = gcd_recursive(48, 18)
print(f"GCD of 48 and 18 is: {result}")  # Output: 6
```

## Step-by-step Example

Let's trace through GCD(48, 18):

```
Step 1: a = 48, b = 18
        48 % 18 = 12
        So a = 18, b = 12

Step 2: a = 18, b = 12
        18 % 12 = 6
        So a = 12, b = 6

Step 3: a = 12, b = 6
        12 % 6 = 0
        So a = 6, b = 0

Step 4: b = 0, return |6| = 6
```

## Complete Example with Multiple Test Cases

```python
def gcd(a, b):
    """Compute GCD using Euclidean algorithm"""
    while b:
        a, b = b, a % b
    return abs(a)

# Test cases
test_cases = [(48, 18), (100, 25), (17, 13), (0, 5), (7, 0)]

for a, b in test_cases:
    result = gcd(a, b)
    print(f"GCD({a}, {b}) = {result}")
```

**Output:**
```
GCD(48, 18) = 6
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(0, 5) = 5
GCD(7, 0) = 7
```

## Using Built-in Function

Python also provides a built-in GCD function:

```python
import math

# Using built-in math.gcd function
result = math.gcd(48, 18)
print(f"GCD of 48 and 18 is: {result}")  # Output: 6
```

The Euclidean algorithm has a time complexity of O(log(min(a, b))) and is much more efficient than finding divisors directly.

