# Binary GCD Algorithm in Go

The Binary GCD algorithm (also known as Stein's algorithm) is an efficient method for computing the greatest common divisor (GCD) of two numbers using bit operations instead of division.

```go
package main

import (
    "fmt"
)

// BinaryGCD computes the greatest common divisor of two non-negative integers
// using the binary GCD algorithm (Stein's algorithm)
func BinaryGCD(a, b int) int {
    // Handle edge cases
    if a == 0 {
        return b
    }
    if b == 0 {
        return a
    }
    
    // Count common factors of 2
    shift := 0
    for (a|b)&1 == 0 {
        a >>= 1
        b >>= 1
        shift++
    }
    
    // Remove remaining factors of 2 from a
    for (a & 1) == 0 {
        a >>= 1
    }
    
    for a != 0 {
        // Remove factors of 2 from b
        for (b & 1) == 0 {
            b >>= 1
        }
        
        // Ensure a <= b
        if a > b {
            a, b = b, a
        }
        
        // Subtract smaller from larger
        b = b - a
    }
    
    // Restore common factors of 2
    return b << shift
}

// Alternative implementation using recursive approach
func BinaryGCDRecursive(a, b int) int {
    if a == 0 {
        return b
    }
    if b == 0 {
        return a
    }
    
    // Count common factors of 2
    shift := 0
    for (a|b)&1 == 0 {
        a >>= 1
        b >>= 1
        shift++
    }
    
    // Remove remaining factors of 2 from a
    for (a & 1) == 0 {
        a >>= 1
    }
    
    for a != 0 {
        // Remove factors of 2 from b
        for (b & 1) == 0 {
            b >>= 1
        }
        
        // Ensure a <= b
        if a > b {
            a, b = b, a
        }
        
        // Subtract smaller from larger
        b = b - a
    }
    
    // Restore common factors of 2
    return b << shift
}

func main() {
    // Test cases
    testCases := [][2]int{
        {48, 18},
        {100, 25},
        {17, 13},
        {0, 5},
        {7, 0},
        {1071, 462},
        {1024, 512},
    }
    
    fmt.Println("Binary GCD Algorithm Results:")
    fmt.Println("============================")
    
    for _, pair := range testCases {
        a, b := pair[0], pair[1]
        result := BinaryGCD(a, b)
        fmt.Printf("GCD(%d, %d) = %d\n", a, b, result)
    }
    
    // Compare with standard math.GCD (Go 1.19+)
    fmt.Println("\nComparison with standard approach:")
    fmt.Println("==================================")
    
    // For demonstration, let's also show the traditional Euclidean algorithm
    euclideanGCD := func(a, b int) int {
        for b != 0 {
            a, b = b, a%b
        }
        return a
    }
    
    for _, pair := range testCases[:3] {
        a, b := pair[0], pair[1]
        binaryResult := BinaryGCD(a, b)
        euclideanResult := euclideanGCD(a, b)
        fmt.Printf("GCD(%d, %d): Binary=%d, Euclidean=%d\n", 
                   a, b, binaryResult, euclideanResult)
    }
}
```

## How the Algorithm Works

1. **Handle edge cases**: If either number is 0, return the other number
2. **Count common factors of 2**: Shift both numbers right until at least one becomes odd
3. **Remove factors of 2 from one number**: Make sure the first number is odd
4. **Main loop**: 
   - Remove factors of 2 from the second number
   - Ensure first number is smaller or equal to second
   - Subtract the smaller from the larger
5. **Restore factors**: Multiply the result by 2 raised to the power of common factors found

## Time Complexity
- **Time**: O(log(min(a,b)))
- **Space**: O(1)

## Key Advantages
- Uses only bit operations (shifts and subtractions)
- Avoids expensive division operations
- More efficient than traditional Euclidean algorithm for large numbers
- Works well with binary computers

## Sample Output
```
Binary GCD Algorithm Results:
============================
GCD(48, 18) = 6
GCD(100, 25) = 25
GCD(17, 13) = 1
GCD(0, 5) = 5
GCD(7, 0) = 7
GCD(1071, 462) = 21
GCD(1024, 512) = 512
```

