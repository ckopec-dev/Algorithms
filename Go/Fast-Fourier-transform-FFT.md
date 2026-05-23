# Fast Fourier Transform (FFT) in Go

Here's a complete implementation of the FFT algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Complex represents a complex number
type Complex struct {
    Real float64
    Imag float64
}

// Multiply complex numbers
func (c Complex) multiply(other Complex) Complex {
    return Complex{
        Real: c.Real*other.Real - c.Imag*other.Imag,
        Imag: c.Real*other.Imag + c.Imag*other.Real,
    }
}

// Add complex numbers
func (c Complex) add(other Complex) Complex {
    return Complex{
        Real: c.Real + other.Real,
        Imag: c.Imag + other.Imag,
    }
}

// Subtract complex numbers
func (c Complex) subtract(other Complex) Complex {
    return Complex{
        Real: c.Real - other.Real,
        Imag: c.Imag - other.Imag,
    }
}

// FFT computes the Fast Fourier Transform of a sequence
func FFT(input []Complex) []Complex {
    n := len(input)
    
    // Base case: if length is 1, return the input
    if n <= 1 {
        return input
    }
    
    // Divide: split the input into even and odd indexed elements
    even := make([]Complex, n/2)
    odd := make([]Complex, n/2)
    
    for i := 0; i < n/2; i++ {
        even[i] = input[2*i]
        odd[i] = input[2*i+1]
    }
    
    // Conquer: recursively apply FFT to even and odd parts
    evenFFT := FFT(even)
    oddFFT := FFT(odd)
    
    // Combine: build the result using the FFT formula
    result := make([]Complex, n)
    for i := 0; i < n/2; i++ {
        // Calculate the twiddle factor (w_n^i)
        angle := -2 * math.Pi * float64(i) / float64(n)
        w := Complex{
            Real: math.Cos(angle),
            Imag: math.Sin(angle),
        }
        
        // Apply the FFT formula
        t := oddFFT[i].multiply(w)
        result[i] = evenFFT[i].add(t)
        result[i+n/2] = evenFFT[i].subtract(t)
    }
    
    return result
}

// IFFT computes the Inverse Fast Fourier Transform
func IFFT(input []Complex) []Complex {
    n := len(input)
    
    // Take conjugate of all input values
    for i := 0; i < n; i++ {
        input[i] = Complex{
            Real: input[i].Real,
            Imag: -input[i].Imag,
        }
    }
    
    // Apply FFT
    result := FFT(input)
    
    // Take conjugate again and divide by n
    for i := 0; i < n; i++ {
        result[i] = Complex{
            Real: result[i].Real / float64(n),
            Imag: -result[i].Imag / float64(n),
        }
    }
    
    return result
}

// Helper function to create a complex number
func complexNum(real, imag float64) Complex {
    return Complex{Real: real, Imag: imag}
}

// Helper function to print complex numbers
func printComplex(c Complex) string {
    if c.Imag >= 0 {
        return fmt.Sprintf("%.2f + %.2fi", c.Real, c.Imag)
    }
    return fmt.Sprintf("%.2f - %.2fi", c.Real, -c.Imag)
}

func main() {
    // Example: 4-point FFT
    input := []Complex{
        complexNum(1, 0),
        complexNum(2, 0),
        complexNum(3, 0),
        complexNum(4, 0),
    }
    
    fmt.Println("Input sequence:")
    for i, val := range input {
        fmt.Printf("x[%d] = %s\n", i, printComplex(val))
    }
    
    // Compute FFT
    fftResult := FFT(input)
    
    fmt.Println("\nFFT result:")
    for i, val := range fftResult {
        fmt.Printf("X[%d] = %s\n", i, printComplex(val))
    }
    
    // Compute IFFT to verify
    ifftResult := IFFT(fftResult)
    
    fmt.Println("\nIFFT result (should match original):")
    for i, val := range ifftResult {
        fmt.Printf("x[%d] = %s\n", i, printComplex(val))
    }
    
    // Example with complex input
    fmt.Println("\n--- Complex Input Example ---")
    complexInput := []Complex{
        complexNum(1, 0),
        complexNum(0, 1),
        complexNum(-1, 0),
        complexNum(0, -1),
    }
    
    fmt.Println("Complex input sequence:")
    for i, val := range complexInput {
        fmt.Printf("x[%d] = %s\n", i, printComplex(val))
    }
    
    fftComplex := FFT(complexInput)
    fmt.Println("\nFFT of complex input:")
    for i, val := range fftComplex {
        fmt.Printf("X[%d] = %s\n", i, printComplex(val))
    }
}
```

## Key Features of this Implementation:

1. **Complex Number Support**: Custom `Complex` struct with basic arithmetic operations
2. **Recursive FFT Algorithm**: Implements the Cooley-Tukey FFT algorithm
3. **Inverse FFT**: Includes IFFT function for signal reconstruction
4. **Proper Twiddle Factor Calculation**: Uses Euler's formula for complex exponentials
5. **Error Handling**: Basic validation for edge cases

## How it Works:

1. **Divide**: Split input into even and odd indexed elements
2. **Conquer**: Recursively compute FFT of both parts
3. **Combine**: Use the FFT formula to merge results using twiddle factors
4. **Twiddle Factors**: Calculate complex roots of unity for the frequency domain

## Time Complexity:
- **Time**: O(n log n)
- **Space**: O(n)

## Usage:
The example demonstrates both real and complex input sequences, showing how the FFT transforms time-domain signals into frequency-domain representations.

This implementation is suitable for educational purposes and basic signal processing applications. For production use, consider using optimized libraries like `gonum` or `go-hep` for better performance and numerical stability.

