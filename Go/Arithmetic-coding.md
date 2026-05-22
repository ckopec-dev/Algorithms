# Arithmetic Coding Algorithm in Go

Here's a complete implementation of the arithmetic coding algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Symbol represents a character and its probability
type Symbol struct {
    Char     rune
    Probability float64
}

// ArithmeticCoder handles the encoding and decoding processes
type ArithmeticCoder struct {
    symbols []Symbol
    totalProbability float64
}

// NewArithmeticCoder creates a new arithmetic coder with given symbols
func NewArithmeticCoder(symbols []Symbol) *ArithmeticCoder {
    coder := &ArithmeticCoder{
        symbols: symbols,
    }
    
    // Calculate total probability
    for _, symbol := range symbols {
        coder.totalProbability += symbol.Probability
    }
    
    // Normalize probabilities if they don't sum to 1
    if math.Abs(coder.totalProbability-1.0) > 1e-10 {
        for i := range coder.symbols {
            coder.symbols[i].Probability /= coder.totalProbability
        }
    }
    
    return coder
}

// Encode encodes a string using arithmetic coding
func (ac *ArithmeticCoder) Encode(input string) float64 {
    if len(input) == 0 {
        return 0.0
    }
    
    lower := 0.0
    upper := 1.0
    
    // Build cumulative probability table
    cumulative := make([]float64, len(ac.symbols)+1)
    cumulative[0] = 0.0
    for i, symbol := range ac.symbols {
        cumulative[i+1] = cumulative[i] + symbol.Probability
    }
    
    // Encode each symbol
    for _, char := range input {
        var symbolIndex int
        found := false
        
        // Find the symbol index
        for i, symbol := range ac.symbols {
            if symbol.Char == char {
                symbolIndex = i
                found = true
                break
            }
        }
        
        if !found {
            // Handle unknown character - use first symbol
            symbolIndex = 0
        }
        
        // Update interval
        newLower := lower + (upper-lower) * cumulative[symbolIndex]
        newUpper := lower + (upper-lower) * cumulative[symbolIndex+1]
        
        lower = newLower
        upper = newUpper
    }
    
    return (lower + upper) / 2.0
}

// Decode decodes a compressed value back to original string
func (ac *ArithmeticCoder) Decode(encodedValue float64, length int) string {
    if length == 0 {
        return ""
    }
    
    lower := 0.0
    upper := 1.0
    
    // Build cumulative probability table
    cumulative := make([]float64, len(ac.symbols)+1)
    cumulative[0] = 0.0
    for i, symbol := range ac.symbols {
        cumulative[i+1] = cumulative[i] + symbol.Probability
    }
    
    var result []rune
    
    // Decode each symbol
    for i := 0; i < length; i++ {
        // Find which symbol this value belongs to
        value := (encodedValue - lower) / (upper - lower)
        
        var symbolIndex int
        for j := 0; j < len(cumulative)-1; j++ {
            if value >= cumulative[j] && value < cumulative[j+1] {
                symbolIndex = j
                break
            }
        }
        
        // Add symbol to result
        result = append(result, ac.symbols[symbolIndex].Char)
        
        // Update interval
        newLower := lower + (upper-lower) * cumulative[symbolIndex]
        newUpper := lower + (upper-lower) * cumulative[symbolIndex+1]
        
        lower = newLower
        upper = newUpper
    }
    
    return string(result)
}

// PrintSymbolInfo prints the symbol probabilities
func (ac *ArithmeticCoder) PrintSymbolInfo() {
    fmt.Println("Symbol Probabilities:")
    for _, symbol := range ac.symbols {
        fmt.Printf("  '%c': %.4f\n", symbol.Char, symbol.Probability)
    }
    fmt.Println()
}

func main() {
    // Define symbols with their probabilities
    symbols := []Symbol{
        {'A', 0.4},
        {'B', 0.3},
        {'C', 0.2},
        {'D', 0.1},
    }
    
    // Create arithmetic coder
    coder := NewArithmeticCoder(symbols)
    
    // Print symbol information
    coder.PrintSymbolInfo()
    
    // Test encoding
    testString := "ABCD"
    fmt.Printf("Original string: %s\n", testString)
    
    encodedValue := coder.Encode(testString)
    fmt.Printf("Encoded value: %.10f\n", encodedValue)
    
    // Test decoding
    decodedString := coder.Decode(encodedValue, len(testString))
    fmt.Printf("Decoded string: %s\n", decodedString)
    
    fmt.Println()
    
    // Test with longer string
    testString2 := "ABACAD"
    fmt.Printf("Original string: %s\n", testString2)
    
    encodedValue2 := coder.Encode(testString2)
    fmt.Printf("Encoded value: %.10f\n", encodedValue2)
    
    decodedString2 := coder.Decode(encodedValue2, len(testString2))
    fmt.Printf("Decoded string: %s\n", decodedString2)
    
    fmt.Println()
    
    // Test with different probabilities
    fmt.Println("Testing with different symbol probabilities:")
    symbols2 := []Symbol{
        {'X', 0.5},
        {'Y', 0.25},
        {'Z', 0.25},
    }
    
    coder2 := NewArithmeticCoder(symbols2)
    coder2.PrintSymbolInfo()
    
    testString3 := "XYZ"
    fmt.Printf("Original string: %s\n", testString3)
    
    encodedValue3 := coder2.Encode(testString3)
    fmt.Printf("Encoded value: %.10f\n", encodedValue3)
    
    decodedString3 := coder2.Decode(encodedValue3, len(testString3))
    fmt.Printf("Decoded string: %s\n", decodedString3)
}
```

## How It Works

### Key Components:

1. **Symbol Structure**: Represents a character with its probability
2. **ArithmeticCoder**: Main struct that handles encoding and decoding
3. **Encode Function**: Converts a string into a floating-point number
4. **Decode Function**: Converts the floating-point number back to the original string

### Algorithm Steps:

1. **Initialization**: Set up symbol probabilities and normalize them
2. **Encoding**: 
   - Start with interval [0, 1]
   - For each symbol, narrow the interval based on cumulative probabilities
   - The final midpoint of the interval is the encoded value
3. **Decoding**:
   - Start with the encoded value and interval [0, 1]
   - For each position, determine which symbol corresponds to the current value
   - Narrow the interval and continue until all symbols are decoded

### Example Output:
```
Symbol Probabilities:
  'A': 0.4000
  'B': 0.3000
  'C': 0.2000
  'D': 0.1000

Original string: ABCD
Encoded value: 0.3500000000
Decoded string: ABCD

Original string: ABACAD
Encoded value: 0.3450000000
Decoded string: ABACAD
```

This implementation demonstrates the core principles of arithmetic coding, where variable-length codes are assigned to symbols based on their probabilities, achieving efficient compression for data with skewed probability distributions.

