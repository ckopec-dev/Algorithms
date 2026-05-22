# Binary Exponentiation Algorithm in Go

Binary exponentiation (also known as exponentiation by squaring) is an efficient algorithm to compute large powers of a number. It reduces the time complexity from O(n) to O(log n).

## Implementation

```go
package main

import "fmt"

// BinaryExponentiation computes base^exponent using binary exponentiation
func binaryExponentiation(base, exponent int) int {
    // Handle edge cases
    if exponent == 0 {
        return 1
    }
    
    if exponent < 0 {
        // For negative exponents, we'd need to return a float
        // This implementation only handles positive integers
        return 0
    }
    
    result := 1
    currentBase := base
    
    // While exponent is greater than 0
    for exponent > 0 {
        // If exponent is odd, multiply result by current base
        if exponent%2 == 1 {
            result *= currentBase
        }
        
        // Square the base and halve the exponent
        currentBase *= currentBase
        exponent /= 2
    }
    
    return result
}

// Alternative implementation using bit manipulation
func binaryExponentiationBitwise(base, exponent int) int {
    if exponent == 0 {
        return 1
    }
    
    if exponent < 0 {
        return 0
    }
    
    result := 1
    
    for exponent > 0 {
        // Check if the least significant bit is 1
        if exponent&1 == 1 {
            result *= base
        }
        
        // Square the base and shift exponent right by 1 bit
        base *= base
        exponent >>= 1
    }
    
    return result
}

func main() {
    // Test cases
    fmt.Println("Binary Exponentiation Examples:")
    fmt.Printf("2^10 = %d\n", binaryExponentiation(2, 10))
    fmt.Printf("3^5 = %d\n", binaryExponentiation(3, 5))
    fmt.Printf("5^0 = %d\n", binaryExponentiation(5, 0))
    fmt.Printf("2^8 = %d\n", binaryExponentiation(2, 8))
    
    fmt.Println("\nUsing Bitwise Implementation:")
    fmt.Printf("2^10 = %d\n", binaryExponentiationBitwise(2, 10))
    fmt.Printf("3^5 = %d\n", binaryExponentiationBitwise(3, 5))
    fmt.Printf("5^3 = %d\n", binaryExponentiationBitwise(5, 3))
}
```

## How it works

The algorithm works by representing the exponent in binary form and using the property:
- If exponent is even: base^n = (base^(n/2))^2
- If exponent is odd: base^n = base × base^(n-1)

## Example Trace for 2^10

```
Exponent: 10 (binary: 1010)
Base: 2

Step 1: exponent = 10 (even)
- No multiplication needed
- base = 2 * 2 = 4
- exponent = 10 / 2 = 5

Step 2: exponent = 5 (odd)
- result = 1 * 4 = 4
- base = 4 * 4 = 16
- exponent = 5 / 2 = 2

Step 3: exponent = 2 (even)
- No multiplication needed
- base = 16 * 16 = 256
- exponent = 2 / 2 = 1

Step 4: exponent = 1 (odd)
- result = 4 * 256 = 1024
- base = 256 * 256 = 65536
- exponent = 1 / 2 = 0

Final result: 1024
```

## Output
```
Binary Exponentiation Examples:
2^10 = 1024
3^5 = 243
5^0 = 1
2^8 = 256

Using Bitwise Implementation:
2^10 = 1024
3^5 = 243
5^3 = 125
```

## Time Complexity
- **Time**: O(log n) where n is the exponent
- **Space**: O(1)

This algorithm is particularly useful when dealing with large exponents, as it significantly reduces the number of multiplications required compared to the naive approach.

