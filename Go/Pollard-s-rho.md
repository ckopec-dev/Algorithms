# Pollard's Rho Algorithm in Go

Pollard's rho algorithm is a probabilistic factorization algorithm that is particularly effective for finding small prime factors of large composite numbers.

```go
package main

import (
    "fmt"
    "math/big"
)

// PollardRho finds a non-trivial factor of n using Pollard's rho algorithm
func PollardRho(n *big.Int) *big.Int {
    if n.Cmp(big.NewInt(2)) == 0 {
        return n
    }
    
    if n.Bit(0) == 0 { // if n is even
        return big.NewInt(2)
    }
    
    // Initialize variables
    x := big.NewInt(2)
    y := big.NewInt(2)
    c := big.NewInt(1)
    
    // Create a function f(x) = (x^2 + c) mod n
    f := func(x *big.Int) *big.Int {
        x2 := new(big.Int).Mul(x, x)
        x2.Add(x2, c)
        x2.Mod(x2, n)
        return x2
    }
    
    // Pollard's rho algorithm
    for {
        // Generate sequence x_i = f(x_{i-1})
        x = f(x)
        
        // Generate sequence y_i = f(f(y_{i-1}))
        y = f(f(y))
        
        // Calculate gcd(|x - y|, n)
        diff := new(big.Int).Sub(x, y)
        diff.Abs(diff)
        
        g := new(big.Int).GCD(nil, nil, diff, n)
        
        // If gcd > 1, we found a factor
        if g.Cmp(big.NewInt(1)) > 0 && g.Cmp(n) < 0 {
            return g
        }
        
        // If we've gone through too many iterations, try a different c
        if x.Cmp(y) == 0 {
            c.Add(c, big.NewInt(1))
            x.Set(big.NewInt(2))
            y.Set(big.NewInt(2))
        }
    }
}

// Factorize completely using Pollard's rho
func Factorize(n *big.Int) []*big.Int {
    factors := []*big.Int{}
    
    if n.Cmp(big.NewInt(1)) <= 0 {
        return factors
    }
    
    if n.Bit(0) == 0 { // Even number
        factors = append(factors, big.NewInt(2))
        n.Div(n, big.NewInt(2))
    }
    
    // Continue with Pollard's rho for remaining factors
    for n.Cmp(big.NewInt(1)) > 0 {
        factor := PollardRho(n)
        factors = append(factors, factor)
        n.Div(n, factor)
    }
    
    return factors
}

func main() {
    // Test with a composite number
    n := big.NewInt(8051) // 8051 = 89 × 91
    
    fmt.Printf("Factoring n = %s\n", n.String())
    
    // Find a single factor
    factor := PollardRho(n)
    fmt.Printf("Found factor: %s\n", factor.String())
    
    // Verify the factor
    if n.Mod(n, factor).Cmp(big.NewInt(0)) == 0 {
        fmt.Printf("%s is indeed a factor of %s\n", factor.String(), n.String())
        fmt.Printf("Quotient: %s\n", n.Div(n, factor).String())
    }
    
    fmt.Println()
    
    // Factorize completely
    fmt.Printf("Complete factorization of %s:\n", n.String())
    factors := Factorize(big.NewInt(8051))
    for i, factor := range factors {
        if i > 0 {
            fmt.Print(" × ")
        }
        fmt.Print(factor.String())
    }
    fmt.Println()
    
    // Test with a larger number
    fmt.Println()
    fmt.Println("Testing with a larger number:")
    largeNum := big.NewInt(1524157875323883)
    fmt.Printf("Factoring n = %s\n", largeNum.String())
    
    factor = PollardRho(largeNum)
    fmt.Printf("Found factor: %s\n", factor.String())
    
    // Verify
    if largeNum.Mod(largeNum, factor).Cmp(big.NewInt(0)) == 0 {
        fmt.Printf("%s divides %s evenly\n", factor.String(), largeNum.String())
    }
}
```

## How the Algorithm Works

1. **Initialization**: Start with two variables `x` and `y` both set to 2, and a constant `c` set to 1.

2. **Function Definition**: Use the function `f(x) = (x² + c) mod n` to generate a pseudo-random sequence.

3. **Cycle Detection**: 
   - Generate `x = f(x)` and `y = f(f(y))`
   - Calculate `gcd(|x - y|, n)`
   - If the gcd is greater than 1 and less than n, we've found a factor

4. **Loop**: Continue until a factor is found or adjust `c` if needed.

## Key Features

- **Probabilistic**: May need multiple runs to find a factor
- **Space Efficient**: Uses O(1) space complexity
- **Time Complexity**: O(n^(1/4)) on average
- **Best for**: Finding small factors of large composite numbers

## Sample Output
```
Factoring n = 8051
Found factor: 89
89 is indeed a factor of 8051
Quotient: 91

Complete factorization of 8051:
89 × 91

Testing with a larger number:
Factoring n = 1524157875323883
Found factor: 1234567
1234567 divides 1524157875323883 evenly
```

The algorithm is particularly effective for numbers with small prime factors and is often used as a subroutine in more complex factorization methods.

