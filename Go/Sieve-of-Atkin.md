# Sieve of Atkin Algorithm in Go

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer. It's more efficient than the classic Sieve of Eratosthenes for large numbers.

```go
package main

import (
    "fmt"
    "math"
)

// SieveOfAtkin finds all prime numbers up to n using the Sieve of Atkin algorithm
func SieveOfAtkin(n int) []int {
    if n < 2 {
        return []int{}
    }
    
    // Create a boolean array and initialize all entries as false
    isPrime := make([]bool, n+1)
    for i := range isPrime {
        isPrime[i] = false
    }
    
    // Mark 2 and 3 as prime
    if n >= 2 {
        isPrime[2] = true
    }
    if n >= 3 {
        isPrime[3] = true
    }
    
    // Use the Sieve of Atkin formula to mark primes
    for x := 1; x*x <= n; x++ {
        for y := 1; y*y <= n; y++ {
            // Formula 1: 4x² + y² = n
            n1 := 4*x*x + y*y
            if n1 <= n && (n1%12 == 1 || n1%12 == 5) {
                isPrime[n1] = !isPrime[n1]
            }
            
            // Formula 2: 3x² + y² = n
            n2 := 3*x*x + y*y
            if n2 <= n && n2%12 == 7 {
                isPrime[n2] = !isPrime[n2]
            }
            
            // Formula 3: 3x² - y² = n (where x > y)
            n3 := 3*x*x - y*y
            if n3 <= n && x > y && n3%12 == 11 {
                isPrime[n3] = !isPrime[n3]
            }
        }
    }
    
    // Mark multiples of squares as non-prime
    for i := 5; i*i <= n; i++ {
        if isPrime[i] {
            // Mark all multiples of i² as non-prime
            for j := i * i; j <= n; j += i * i {
                isPrime[j] = false
            }
        }
    }
    
    // Collect all prime numbers
    primes := []int{}
    for i := 2; i <= n; i++ {
        if isPrime[i] {
            primes = append(primes, i)
        }
    }
    
    return primes
}

// Helper function to print primes in a formatted way
func printPrimes(primes []int, limit int) {
    fmt.Printf("First %d prime numbers:\n", limit)
    for i, prime := range primes {
        if i >= limit {
            break
        }
        fmt.Printf("%d ", prime)
        if (i+1)%10 == 0 {
            fmt.Println()
        }
    }
    if len(primes) > 0 {
        fmt.Println()
    }
}

func main() {
    // Example 1: Find all primes up to 30
    fmt.Println("=== Sieve of Atkin Example ===")
    n := 30
    primes := SieveOfAtkin(n)
    
    fmt.Printf("Prime numbers up to %d:\n", n)
    fmt.Println(primes)
    fmt.Printf("Total count: %d\n\n", len(primes))
    
    // Example 2: Find first 20 primes
    fmt.Println("=== First 20 Prime Numbers ===")
    first20 := SieveOfAtkin(100)
    printPrimes(first20, 20)
    
    // Example 3: Find primes up to 100
    fmt.Println("=== Prime Numbers up to 100 ===")
    primes100 := SieveOfAtkin(100)
    fmt.Printf("Primes up to 100: %v\n", primes100)
    fmt.Printf("Count: %d\n", len(primes100))
    
    // Example 4: Performance test
    fmt.Println("\n=== Performance Test ===")
    n = 1000
    primes1000 := SieveOfAtkin(n)
    fmt.Printf("Primes up to %d: %d primes found\n", n, len(primes1000))
    
    // Show first 10 and last 10 primes for verification
    fmt.Print("First 10: ")
    for i := 0; i < 10; i++ {
        fmt.Printf("%d ", primes1000[i])
    }
    fmt.Println()
    
    fmt.Print("Last 10: ")
    start := len(primes1000) - 10
    for i := start; i < len(primes1000); i++ {
        fmt.Printf("%d ", primes1000[i])
    }
    fmt.Println()
}
```

## How the Algorithm Works

The Sieve of Atkin uses three quadratic forms to identify potential primes:

1. **Formula 1**: `4x² + y² = n` where `n % 12 = 1 or 5`
2. **Formula 2**: `3x² + y² = n` where `n % 12 = 7`  
3. **Formula 3**: `3x² - y² = n` where `x > y` and `n % 12 = 11`

The algorithm:
1. Initialize a boolean array to track prime candidates
2. Apply the three formulas to mark potential primes
3. Flip the boolean values for numbers that satisfy the formulas
4. Remove multiples of squares (like 4, 9, 25, etc.) that were marked as primes
5. Collect all numbers marked as prime

## Output Example

```
=== Sieve of Atkin Example ===
Prime numbers up to 30:
[2 3 5 7 11 13 17 19 23 29]
Total count: 10

=== First 20 Prime Numbers ===
First 20 prime numbers:
2 3 5 7 11 13 17 19 23 29 
31 37 41 43 47 53 59 61 67 71 

=== Prime Numbers up to 100 ===
Primes up to 100: [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]
Count: 25

=== Performance Test ===
Primes up to 1000: 168 primes found
First 10: 2 3 5 7 11 13 17 19 23 29 
Last 10: 937 941 947 953 967 971 977 983 991 997 
```

This implementation is efficient and demonstrates the mathematical elegance of the Sieve of Atkin algorithm.

