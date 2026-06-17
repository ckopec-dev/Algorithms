# Fast Fourier Transform (FFT) in C

Here's a complete implementation of the Cooley-Tukey FFT algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Complex number structure
typedef struct {
    double real;
    double imag;
} complex;

// Initialize complex number
complex complex_init(double real, double imag) {
    complex c;
    c.real = real;
    c.imag = imag;
    return c;
}

// Add two complex numbers
complex complex_add(complex a, complex b) {
    return complex_init(a.real + b.real, a.imag + b.imag);
}

// Subtract two complex numbers
complex complex_subtract(complex a, complex b) {
    return complex_init(a.real - b.real, a.imag - b.imag);
}

// Multiply two complex numbers
complex complex_multiply(complex a, complex b) {
    return complex_init(
        a.real * b.real - a.imag * b.imag,
        a.real * b.imag + a.imag * b.real
    );
}

// FFT algorithm implementation
void fft(complex *x, int n) {
    if (n <= 1) return;
    
    // Separate even and odd indexed elements
    complex *even = (complex*)malloc(n/2 * sizeof(complex));
    complex *odd = (complex*)malloc(n/2 * sizeof(complex));
    
    for (int i = 0; i < n/2; i++) {
        even[i] = x[2*i];
        odd[i] = x[2*i + 1];
    }
    
    // Recursive FFT calls
    fft(even, n/2);
    fft(odd, n/2);
    
    // Combine results
    for (int k = 0; k < n/2; k++) {
        double angle = -2 * M_PI * k / n;
        complex w = complex_init(cos(angle), sin(angle));
        complex t = complex_multiply(odd[k], w);
        
        x[k] = complex_add(even[k], t);
        x[k + n/2] = complex_subtract(even[k], t);
    }
    
    free(even);
    free(odd);
}

// Print complex array
void print_complex_array(complex *x, int n) {
    for (int i = 0; i < n; i++) {
        printf("X[%d] = %.4f + %.4fi\n", i, x[i].real, x[i].imag);
    }
}

// Example usage
int main() {
    // Example: 8-point FFT
    int N = 8;
    
    // Input signal (real values)
    double input[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    
    // Create complex array
    complex *x = (complex*)malloc(N * sizeof(complex));
    
    for (int i = 0; i < N; i++) {
        x[i] = complex_init(input[i], 0.0);
    }
    
    printf("Input signal:\n");
    print_complex_array(x, N);
    
    // Apply FFT
    fft(x, N);
    
    printf("\nFFT result:\n");
    print_complex_array(x, N);
    
    free(x);
    return 0;
}
```

## Output Example:
```
Input signal:
X[0] = 1.0000 + 0.0000i
X[1] = 2.0000 + 0.0000i
X[2] = 3.0000 + 0.0000i
X[3] = 4.0000 + 0.0000i
X[4] = 5.0000 + 0.0000i
X[5] = 6.0000 + 0.0000i
X[6] = 7.0000 + 0.0000i
X[7] = 8.0000 + 0.0000i

FFT result:
X[0] = 36.0000 + 0.0000i
X[1] = -4.0000 + 9.6569i
X[2] = -4.0000 + 4.0000i
X[3] = -4.0000 + 1.6569i
X[4] = -4.0000 + 0.0000i
X[5] = -4.0000 - 1.6569i
X[6] = -4.0000 - 4.0000i
X[7] = -4.0000 - 9.6569i
```

## Key Features:

1. **Complex Number Operations**: Basic arithmetic operations for complex numbers
2. **Recursive Implementation**: Uses the Cooley-Tukey algorithm
3. **In-place Processing**: Efficient memory usage
4. **Supports Any Size**: Works with power-of-2 lengths
5. **Complete Example**: Includes input/output demonstration

## Time Complexity:
- **O(n log n)** where n is the number of points
- Much faster than direct DFT computation which is O(n²)

This implementation provides a solid foundation for FFT computations in C and can be extended for more advanced applications like signal processing or spectral analysis.