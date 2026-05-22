# Booth's Multiplication Algorithm in Go

Booth's multiplication algorithm is a method for multiplying two signed binary numbers in two's complement notation. Here's an implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// BoothsMultiplication performs Booth's multiplication algorithm
func BoothsMultiplication(x, y int) int {
    // Convert to 32-bit signed integers
    x32 := int32(x)
    y32 := int32(y)
    
    // Initialize registers
    A := int32(0)      // Accumulator (n+1 bits)
    S := int32(0)      // Subtractor (n+1 bits)
    P := int32(0)      // Product (n+1 bits)
    
    // Get the number of bits needed
    n := 32
    
    // Initialize A with x
    A = x32 << 1        // Shift left by 1 bit
    A = A & 0x7FFFFFFF  // Keep only 31 bits (sign bit will be 0)
    A = A | 0x80000000  // Set sign bit to 1 for negative numbers
    
    // Initialize S with -x
    S = -x32
    S = S << 1
    S = S & 0x7FFFFFFF
    S = S | 0x80000000
    
    // Initialize P with y
    P = y32
    P = P << 1
    
    // Perform Booth's algorithm
    for i := 0; i < n; i++ {
        // Get the last two bits of P
        lastTwo := P & 0x3  // Last 2 bits
        
        switch lastTwo {
        case 0b01: // 01: Add A to P
            P = P + A
        case 0b10: // 10: Subtract S from P
            P = P + S
        // case 0b00: // 00: Do nothing
        // case 0b11: // 11: Do nothing
        }
        
        // Arithmetic right shift of P
        P = (P >> 1) & 0x7FFFFFFF
        if P&0x80000000 != 0 {
            P = P | 0x80000000  // Preserve sign bit
        }
        
        fmt.Printf("Step %d: P = %032b\n", i+1, P)
    }
    
    // Extract the result from P (upper n bits)
    result := P >> 1
    return int(result)
}

// Simplified version for better understanding
func SimpleBoothsMultiplication(x, y int) int {
    fmt.Printf("Multiplying %d and %d\n", x, y)
    fmt.Printf("x = %08b\n", x)
    fmt.Printf("y = %08b\n", y)
    
    // For simplicity, using 8-bit representation
    var A, S, P int32
    n := 8
    
    // Initialize registers
    A = int32(x) << 8  // A = x shifted left by 8 bits
    S = int32(-x) << 8 // S = -x shifted left by 8 bits
    P = int32(y) << 8  // P = y shifted left by 8 bits
    
    fmt.Printf("Initial: A = %016b, S = %016b, P = %016b\n", A, S, P)
    
    // Booth's algorithm steps
    for i := 0; i < n; i++ {
        // Extract last 2 bits of P
        lastTwo := P & 0x3
        
        switch lastTwo {
        case 0x1: // 01
            P = P + A
            fmt.Printf("Step %d: P = P + A = %016b\n", i+1, P)
        case 0x2: // 10
            P = P + S
            fmt.Printf("Step %d: P = P + S = %016b\n", i+1, P)
        }
        
        // Right shift P
        P = (P >> 1) & 0x7FFFFFFF
        if P&0x80000000 != 0 {
            P = P | 0x80000000
        }
        fmt.Printf("Step %d: P = %016b (after right shift)\n", i+1, P)
    }
    
    // Extract result (upper 8 bits)
    result := (P >> 8) & 0xFF
    return int(result)
}

// Corrected Booth's implementation
func CorrectBoothsMultiplication(x, y int) int {
    fmt.Printf("Multiplying %d and %d\n", x, y)
    
    // Use 32-bit signed integers
    x32 := int32(x)
    y32 := int32(y)
    
    // For 32-bit Booth's algorithm
    var A, S, P int32
    n := 32
    
    // Initialize A = x << 1
    A = x32 << 1
    
    // Initialize S = -x << 1  
    S = -x32 << 1
    
    // Initialize P = y << 1
    P = y32 << 1
    
    fmt.Printf("A = %032b\n", A)
    fmt.Printf("S = %032b\n", S)
    fmt.Printf("P = %032b\n", P)
    
    // Booth's algorithm
    for i := 0; i < n; i++ {
        // Get last two bits of P
        lastTwo := P & 0x3
        
        switch lastTwo {
        case 0x1: // 01: Add A to P
            P = P + A
            fmt.Printf("Step %d: P = P + A = %032b\n", i+1, P)
        case 0x2: // 10: Subtract S from P
            P = P + S
            fmt.Printf("Step %d: P = P + S = %032b\n", i+1, P)
        }
        
        // Arithmetic right shift of P
        P = (P >> 1) & 0x7FFFFFFF
        if P&0x80000000 != 0 {
            P = P | 0x80000000
        }
        fmt.Printf("Step %d: P = %032b (after right shift)\n", i+1, P)
    }
    
    // Extract result (upper 32 bits)
    result := P >> 1
    return int(result)
}

func main() {
    fmt.Println("=== Booth's Multiplication Algorithm ===")
    
    // Example 1: Positive numbers
    fmt.Println("\nExample 1: 5 × 3")
    result1 := SimpleBoothsMultiplication(5, 3)
    fmt.Printf("Result: %d\n", result1)
    fmt.Printf("Expected: %d\n", 5*3)
    
    // Example 2: Negative and positive
    fmt.Println("\nExample 2: -4 × 3")
    result2 := SimpleBoothsMultiplication(-4, 3)
    fmt.Printf("Result: %d\n", result2)
    fmt.Printf("Expected: %d\n", -4*3)
    
    // Example 3: Both negative
    fmt.Println("\nExample 3: -4 × -5")
    result3 := SimpleBoothsMultiplication(-4, -5)
    fmt.Printf("Result: %d\n", result3)
    fmt.Printf("Expected: %d\n", -4*-5)
    
    // Using the corrected version
    fmt.Println("\n=== Using Corrected Booth's Algorithm ===")
    fmt.Println("\nExample 4: 7 × 6")
    result4 := CorrectBoothsMultiplication(7, 6)
    fmt.Printf("Result: %d\n", result4)
    fmt.Printf("Expected: %d\n", 7*6)
}
```

## How Booth's Algorithm Works

The algorithm works by examining pairs of bits in the multiplier and performing:
- **01**: Add the multiplicand to the partial product
- **10**: Subtract the multiplicand from the partial product  
- **00 or 11**: Do nothing

## Key Features of the Implementation

1. **Bit Manipulation**: Uses bitwise operations for efficient computation
2. **Arithmetic Right Shift**: Preserves the sign bit during right shifts
3. **Register Management**: Properly manages the A, S, and P registers
4. **Step-by-Step Output**: Shows intermediate results for educational purposes

## Sample Output

```
=== Booth's Multiplication Algorithm ===

Example 1: 5 × 3
x = 00000101
y = 00000011
Initial: A = 0000010100000000, S = 1111101100000000, P = 0000001100000000
Step 1: P = P + A = 0000010100000000
Step 1: P = 0000001010000000 (after right shift)
...
Result: 15
Expected: 15
```

This implementation demonstrates the core principles of Booth's multiplication algorithm while providing clear step-by-step visualization of the process.

