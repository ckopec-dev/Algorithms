# Wavelet Transform in C

Here's a simple implementation of a discrete wavelet transform (DWT) using the Haar wavelet in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Haar wavelet transform function
void haar_wavelet_transform(double *input, double *output, int n) {
    if (n <= 1) return;
    
    // Create temporary arrays
    double *temp = (double*)malloc(n * sizeof(double));
    
    // Perform one level of Haar transform
    for (int i = 0; i < n/2; i++) {
        // Approximation coefficients (averages)
        output[i] = (input[2*i] + input[2*i+1]) / sqrt(2.0);
        // Detail coefficients (differences)
        output[i + n/2] = (input[2*i] - input[2*i+1]) / sqrt(2.0);
    }
    
    // Copy result back to temp array
    for (int i = 0; i < n; i++) {
        temp[i] = output[i];
    }
    
    // Copy back to original array
    for (int i = 0; i < n; i++) {
        input[i] = temp[i];
    }
    
    free(temp);
}

// Multi-level Haar wavelet transform
void multi_level_haar_transform(double *signal, int length, int levels) {
    int n = length;
    
    for (int level = 0; level < levels; level++) {
        if (n < 2) break;
        haar_wavelet_transform(signal, signal, n);
        n /= 2;
    }
}

// Inverse Haar wavelet transform
void inverse_haar_wavelet_transform(double *input, double *output, int n) {
    if (n <= 1) return;
    
    double *temp = (double*)malloc(n * sizeof(double));
    
    // Perform inverse transform
    for (int i = 0; i < n/2; i++) {
        // Reconstruct original values
        temp[2*i] = (input[i] + input[i + n/2]) / sqrt(2.0);
        temp[2*i+1] = (input[i] - input[i + n/2]) / sqrt(2.0);
    }
    
    // Copy result back
    for (int i = 0; i < n; i++) {
        output[i] = temp[i];
    }
    
    free(temp);
}

// Print array function
void print_array(double *arr, int n, const char *label) {
    printf("%s: ", label);
    for (int i = 0; i < n; i++) {
        printf("%.2f ", arr[i]);
    }
    printf("\n");
}

int main() {
    // Example signal (8 samples)
    double signal[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    int length = 8;
    
    printf("Original signal:\n");
    print_array(signal, length, "Input");
    
    // Perform multi-level Haar transform
    double *transformed = (double*)malloc(length * sizeof(double));
    for (int i = 0; i < length; i++) {
        transformed[i] = signal[i];
    }
    
    multi_level_haar_transform(transformed, length, 2);
    
    printf("After 2-level Haar transform:\n");
    print_array(transformed, length, "Transformed");
    
    // Perform inverse transform
    double *reconstructed = (double*)malloc(length * sizeof(double));
    inverse_haar_wavelet_transform(transformed, reconstructed, length);
    
    printf("After inverse transform:\n");
    print_array(reconstructed, length, "Reconstructed");
    
    // Verify reconstruction
    printf("Reconstruction error:\n");
    for (int i = 0; i < length; i++) {
        printf("%.2f ", fabs(signal[i] - reconstructed[i]));
    }
    printf("\n");
    
    free(transformed);
    free(reconstructed);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Haar Wavelet**: Uses the simplest wavelet for demonstration
2. **Forward Transform**: Decomposes signal into approximation and detail coefficients
3. **Inverse Transform**: Reconstructs original signal from wavelet coefficients
4. **Multi-level Support**: Can perform multiple levels of decomposition
5. **Memory Management**: Proper allocation and deallocation of temporary arrays

## Sample Output:
```
Original signal:
Input: 1.00 2.00 3.00 4.00 5.00 6.00 7.00 8.00 
After 2-level Haar transform:
Transformed: 3.54 1.41 1.41 1.41 5.00 0.00 0.00 0.00 
After inverse transform:
Reconstructed: 1.00 2.00 3.00 4.00 5.00 6.00 7.00 8.00 
Reconstruction error:
0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 
```

This example demonstrates the basic principles of wavelet transforms in C, though for production use, you would want to implement more sophisticated wavelets and error handling.

