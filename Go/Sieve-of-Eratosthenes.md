# Sieve of Eratosthenes in Go

Here's an implementation of the Sieve of Eratosthenes algorithm in Go to find all prime numbers up to a given limit:

```go
package main

import (
    "fmt"
    "math"
)

// sieveOfEratosthenes returns all prime numbers up to n using the Sieve of Eratosthenes
func sieveOfEratosthenes(n int) []int {
    if n < 2 {
        return []int{}
    }
    
    // Create a boolean array and initialize all entries as true
    // isPrime[i] will be false if i is not a prime, true otherwise
    isPrime := make([]bool, n+1)
    for i := 2; i <= n; i++ {
        isPrime[i] = true
    }
    
    // Start with the first prime number, 2
    for p := 2; p*p <= n; p++ {
        if isPrime[p] {
            // Mark all multiples of p as not prime
            for i := p * p; i <= n; i += p {
                isPrime[i] = false
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

func main() {
    // Find all primes up to 30
    limit := 30
    primes := sieveOfEratosthenes(limit)
    
    fmt.Printf("Prime numbers up to %d:\n", limit)
    fmt.Println(primes)
    
    // Find all primes up to 100
    limit = 100
    primes = sieveOfEratosthenes(limit)
    
    fmt.Printf("\nPrime numbers up to %d:\n", limit)
    fmt.Println(primes)
    
    // Example with small number
    limit = 10
    primes = sieveOfEratosthenes(limit)
    
    fmt.Printf("\nPrime numbers up to %d:\n", limit)
    fmt.Println(primes)
}
```

## Output:
```
Prime numbers up to 30:
[2 3 5 7 11 13 17 19 23 29]

Prime numbers up to 100:
[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]

Prime numbers up to 10:
[2 3 5 7]
```

## How it works:

1. **Initialization**: Create a boolean array `isPrime` of size `n+1` and initialize all values to `true` (assuming all numbers are prime initially)

2. **Mark non-primes**: Starting from 2 (the first prime), mark all multiples of each prime as `false` (not prime)

3. **Optimization**: Only check up to √n because any composite number greater than √n would have already been marked by a smaller prime factor

4. **Collection**: Gather all numbers that remain marked as `true` (prime)

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

The algorithm efficiently finds all prime numbers up to a given limit by eliminating multiples of known primes, making it one of the most efficient methods for finding multiple primes.

