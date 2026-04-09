# Karatsuba Multiplication Algorithm in Go

The Karatsuba algorithm is a fast multiplication algorithm that uses a divide-and-conquer approach to multiply two numbers more efficiently than the traditional O(n²) method.

```go
package main

import (
    "fmt"
    "math"
)

// Karatsuba multiplication algorithm
func karatsuba(x, y int64) int64 {
    // Base case: if numbers are single digits, multiply directly
    if x < 10 && y < 10 {
        return x * y
    }
    
    // Convert to strings to find number of digits
    xStr := fmt.Sprintf("%d", x)
    yStr := fmt.Sprintf("%d", y)
    
    n := max(len(xStr), len(yStr))
    
    // Make sure both numbers have the same number of digits
    // by padding with zeros
    if len(xStr) < n {
        xStr = fmt.Sprintf("%0*d", n, x)
    }
    if len(yStr) < n {
        yStr = fmt.Sprintf("%0*d", n, y)
    }
    
    // Split the numbers into two halves
    mid := n / 2
    xHigh := mustParseInt(xStr[:n-mid])
    xLow := mustParseInt(xStr[n-mid:])
    yHigh := mustParseInt(yStr[:n-mid])
    yLow := mustParseInt(yStr[n-mid:])
    
    // Recursive calls
    z0 := karatsuba(xLow, yLow)
    z2 := karatsuba(xHigh, yHigh)
    z1 := karatsuba(xLow+xHigh, yLow+yHigh) - z2 - z0
    
    // Combine results
    return z2*int64(math.Pow10(2*mid)) + z1*int64(math.Pow10(mid)) + z0
}

// Helper function to parse string to int64 (with error handling)
func mustParseInt(s string) int64 {
    var result int64
    fmt.Sscanf(s, "%d", &result)
    return result
}

// Helper function to find maximum of two integers
func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

// Traditional multiplication for comparison
func traditionalMultiply(x, y int64) int64 {
    return x * y
}

func main() {
    // Test cases
    testCases := [][2]int64{
        {1234, 5678},
        {123, 456},
        {12, 34},
        {5, 7},
        {12345, 67890},
    }
    
    fmt.Println("Karatsuba vs Traditional Multiplication")
    fmt.Println("======================================")
    
    for _, testCase := range testCases {
        x, y := testCase[0], testCase[1]
        
        // Using Karatsuba
        resultKaratsuba := karatsuba(x, y)
        
        // Using traditional method
        resultTraditional := traditionalMultiply(x, y)
        
        fmt.Printf("Numbers: %d × %d\n", x, y)
        fmt.Printf("Karatsuba result:   %d\n", resultKaratsuba)
        fmt.Printf("Traditional result: %d\n", resultTraditional)
        fmt.Printf("Match: %t\n\n", resultKaratsuba == resultTraditional)
    }
    
    // Performance comparison example
    fmt.Println("Performance Example:")
    fmt.Println("===================")
    
    largeX := int64(123456789)
    largeY := int64(987654321)
    
    fmt.Printf("Multiplying %d × %d\n", largeX, largeY)
    
    result := karatsuba(largeX, largeY)
    fmt.Printf("Result: %d\n", result)
    
    // Verify with traditional method
    traditional := traditionalMultiply(largeX, largeY)
    fmt.Printf("Verification: %t\n", result == traditional)
}
```

## How the Algorithm Works:

1. **Base Case**: If both numbers are single digits, multiply them directly
2. **Split**: Divide each number into two halves
3. **Recursive Calls**: 
   - Compute `z0 = low_x × low_y`
   - Compute `z2 = high_x × high_y` 
   - Compute `z1 = (low_x + high_x) × (low_y + high_y) - z2 - z0`
4. **Combine**: Result = `z2 × 10^(2n) + z1 × 10^n + z0`

## Time Complexity:
- **Traditional**: O(n²)
- **Karatsuba**: O(n^log₂3) ≈ O(n^1.585)

## Key Advantages:
- More efficient for large numbers
- Reduces the number of multiplications from 4 to 3
- Better performance for very large integers

## Output Example:
```
Karatsuba vs Traditional Multiplication
======================================
Numbers: 1234 × 5678
Karatsuba result:   7006652
Traditional result: 7006652
Match: true

Numbers: 123 × 456
Karatsuba result:   56088
Traditional result: 56088
Match: true
```

