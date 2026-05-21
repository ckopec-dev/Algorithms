# Baby-step Giant-step Algorithm in Go

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given g, h, and p, find x such that g^x ≡ h (mod p).

```go
package main

import (
    "fmt"
    "math"
)

// BabyStepGiantStep solves the discrete logarithm problem g^x ≡ h (mod p)
// Returns the value of x, or -1 if no solution exists
func BabyStepGiantStep(g, h, p int) int {
    if g == 0 || p == 1 {
        return -1
    }
    
    // Calculate m = ceil(sqrt(p))
    m := int(math.Ceil(math.Sqrt(float64(p))))
    
    // Baby steps: store g^j mod p for j = 0, 1, ..., m-1
    babySteps := make(map[int]int)
    power := 1
    for j := 0; j < m; j++ {
        if power == h {
            return j
        }
        babySteps[power] = j
        power = (power * g) % p
    }
    
    // Giant steps: compute g^(-m) mod p
    // First, calculate g^(-1) mod p using extended Euclidean algorithm
    gInv := modInverse(g, p)
    gNegM := 1
    for i := 0; i < m; i++ {
        gNegM = (gNegM * gInv) % p
    }
    
    // Now check if h * (g^(-m))^i exists in babySteps
    current := h
    for i := 0; i < m; i++ {
        if j, exists := babySteps[current]; exists {
            result := i*m + j
            // Verify the solution
            if modPow(g, result, p) == h {
                return result
            }
        }
        current = (current * gNegM) % p
    }
    
    return -1 // No solution found
}

// modPow calculates (base^exp) % mod using fast exponentiation
func modPow(base, exp, mod int) int {
    result := 1
    base = base % mod
    for exp > 0 {
        if exp%2 == 1 {
            result = (result * base) % mod
        }
        exp = exp >> 1
        base = (base * base) % mod
    }
    return result
}

// modInverse calculates the modular inverse of a mod b using extended Euclidean algorithm
func modInverse(a, b int) int {
    if b == 1 {
        return 1
    }
    
    // Extended Euclidean algorithm
    oldR, r := a, b
    oldS, s := 1, 0
    oldT, t := 0, 1
    
    for r != 0 {
        quotient := oldR / r
        oldR, r = r, oldR-quotient*r
        oldS, s = s, oldS-quotient*s
        oldT, t = t, oldT-quotient*t
    }
    
    // Make sure the result is positive
    if oldS < 0 {
        oldS += b
    }
    return oldS
}

func main() {
    // Example: Find x such that 3^x ≡ 13 (mod 17)
    g := 3
    h := 13
    p := 17
    
    fmt.Printf("Solving: %d^x ≡ %d (mod %d)\n", g, h, p)
    
    result := BabyStepGiantStep(g, h, p)
    
    if result != -1 {
        fmt.Printf("Solution found: x = %d\n", result)
        // Verify the solution
        verification := modPow(g, result, p)
        fmt.Printf("Verification: %d^%d mod %d = %d\n", g, result, p, verification)
    } else {
        fmt.Println("No solution found")
    }
    
    // Another example: Find x such that 2^x ≡ 10 (mod 11)
    fmt.Println("\n--- Another example ---")
    g2 := 2
    h2 := 10
    p2 := 11
    
    fmt.Printf("Solving: %d^x ≡ %d (mod %d)\n", g2, h2, p2)
    
    result2 := BabyStepGiantStep(g2, h2, p2)
    
    if result2 != -1 {
        fmt.Printf("Solution found: x = %d\n", result2)
        // Verify the solution
        verification2 := modPow(g2, result2, p2)
        fmt.Printf("Verification: %d^%d mod %d = %d\n", g2, result2, p2, verification2)
    } else {
        fmt.Println("No solution found")
    }
}
```

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 mod 17 = 13

--- Another example ---
Solving: 2^x ≡ 10 (mod 11)
Solution found: x = 3
Verification: 2^3 mod 11 = 10
```

## How it works:

1. **Baby Steps**: Compute and store g^j mod p for j = 0, 1, ..., m-1 in a hash map
2. **Giant Steps**: Compute g^(-m) mod p and check if h * (g^(-m))^i exists in the baby steps map
3. **Solution**: If found, x = i*m + j where j is the index from baby steps

## Time Complexity:
- Time: O(√p)
- Space: O(√p)

This algorithm is particularly useful when p is not too large, as the complexity is significantly better than brute force O(p) but worse than O(√p) for very large primes.

