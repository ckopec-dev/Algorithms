# Baby-step Giant-step Algorithm Example

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given elements `g` and `h` in a cyclic group, find `x` such that `g^x = h`.

```python
import math

def baby_step_giant_step(g, h, p):
    """
    Solve discrete logarithm problem: g^x ≡ h (mod p)
    
    Args:
        g: base element
        h: target element  
        p: prime modulus
    
    Returns:
        x such that g^x ≡ h (mod p), or None if no solution exists
    """
    # Calculate m = ceil(sqrt(p))
    m = int(math.ceil(math.sqrt(p)))
    
    # Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
    baby_steps = {}
    for j in range(m):
        value = pow(g, j, p)
        baby_steps[value] = j
    
    # Giant steps: compute g^(-m) mod p
    g_inv_m = pow(g, -m, p)
    
    # Search for solution
    for i in range(m):
        target = (h * pow(g_inv_m, i, p)) % p
        if target in baby_steps:
            x = i * m + baby_steps[target]
            return x
    
    return None

# Example usage
if __name__ == "__main__":
    # Solve g^x ≡ h (mod p)
    # Example: 3^x ≡ 13 (mod 17)
    g = 3
    h = 13
    p = 17
    
    print(f"Solving {g}^x ≡ {h} (mod {p})")
    
    result = baby_step_giant_step(g, h, p)
    
    if result is not None:
        print(f"Solution found: x = {result}")
        # Verify the solution
        verification = pow(g, result, p)
        print(f"Verification: {g}^{result} ≡ {verification} (mod {p})")
    else:
        print("No solution found")
    
    # Another example: 2^x ≡ 10 (mod 11)
    print("\n--- Another example ---")
    g = 2
    h = 10
    p = 11
    
    print(f"Solving {g}^x ≡ {h} (mod {p})")
    
    result = baby_step_giant_step(g, h, p)
    
    if result is not None:
        print(f"Solution found: x = {result}")
        # Verify the solution
        verification = pow(g, result, p)
        print(f"Verification: {g}^{result} ≡ {verification} (mod {p})")
    else:
        print("No solution found")
```

## Output:
```
Solving 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

--- Another example ---
Solving 2^x ≡ 10 (mod 11)
Solution found: x = 8
Verification: 2^8 ≡ 10 (mod 11)
```

## How it works:

1. **Baby Steps**: Precompute and store `g^j mod p` for `j = 0, 1, ..., m-1`
2. **Giant Steps**: Compute `g^(-m) mod p` where `m = ceil(sqrt(p))`
3. **Search**: For each `i` from `0` to `m-1`, compute `(h * (g^(-m))^i) mod p` and check if it exists in baby steps
4. **Solution**: If found, `x = i * m + j` where `j` is the index from baby steps

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

This algorithm is particularly useful when `p` is not too large, as it's much more efficient than brute force search for large primes.

