# Wavelet Transform Algorithm in C

Here's a complete implementation of a 1D Discrete Wavelet Transform (DWT) using the Haar wavelet in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Haar wavelet transform function
void haar_dwt(double *input, double *output, int n) {
    if (n <= 1) return;
    
    int half = n / 2;
    
    // Perform the forward DWT
    for (int i = 0; i < half; i++) {
        // Approximation coefficients (averages)
        output[i] = (input[2*i] + input[2*i+1]) / 2.0;
        // Detail coefficients (differences)
        output[i + half] = (input[2*i] - input[2*i+1]) / 2.0;
    }
}

// Inverse Haar wavelet transform function
void haar_idwt(double *input, double *output, int n) {
    if (n <= 1) return;
    
    int half = n / 2;
    
    // Perform the inverse DWT
    for (int i = 0; i < half; i++) {
        // Reconstruct original values
        output[2*i] = input[i] + input[i + half];
        output[2*i+1] = input[i] - input[i + half];
    }
}

// Multi-level wavelet decomposition
void multilevel_dwt(double *signal, double *coeffs, int length, int levels) {
    double *temp1 = (double*)malloc(length * sizeof(double));
    double *temp2 = (double*)malloc(length * sizeof(double));
    
    // Copy input signal to temp1
    for (int i = 0; i < length; i++) {
        temp1[i] = signal[i];
    }
    
    int current_length = length;
    
    for (int level = 0; level < levels; level++) {
        if (current_length <= 1) break;
        
        haar_dwt(temp1, temp2, current_length);
        
        // Copy coefficients to output array
        for (int i = 0; i < current_length; i++) {
            coeffs[level * current_length + i] = temp2[i];
        }
        
        // Prepare for next level
        for (int i = 0; i < current_length; i++) {
            temp1[i] = temp2[i];
        }
        
        current_length /= 2;
    }
    
    free(temp1);
    free(temp2);
}

// Print array function
void print_array(double *array, int n, const char* label) {
    printf("%s: ", label);
    for (int i = 0; i < n; i++) {
        printf("%.2f ", array[i]);
    }
    printf("\n");
}

int main() {
    // Example signal (8 samples)
    double signal[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    int length = 8;
    
    printf("Original Signal:\n");
    print_array(signal, length, "Signal");
    
    // Forward DWT
    double *dwt_coeffs = (double*)malloc(length * sizeof(double));
    double *reconstructed = (double*)malloc(length * sizeof(double));
    
    // Single level DWT
    haar_dwt(signal, dwt_coeffs, length);
    printf("\nDWT Coefficients:\n");
    print_array(dwt_coeffs, length, "Coeffs");
    
    // Inverse DWT
    haar_idwt(dwt_coeffs, reconstructed, length);
    printf("\nReconstructed Signal:\n");
    print_array(reconstructed, length, "Recon");
    
    // Multi-level DWT example (2 levels)
    printf("\nMulti-level DWT (2 levels):\n");
    double *multilevel_coeffs = (double*)malloc(15 * sizeof(double)); // 8 + 4 + 2 + 1
    multilevel_dwt(signal, multilevel_coeffs, length, 2);
    
    printf("Level 0 (8 coeffs): ");
    for (int i = 0; i < 8; i++) {
        printf("%.2f ", multilevel_coeffs[i]);
    }
    printf("\n");
    
    printf("Level 1 (4 coeffs): ");
    for (int i = 8; i < 12; i++) {
        printf("%.2f ", multilevel_coeffs[i]);
    }
    printf("\n");
    
    // Cleanup
    free(dwt_coeffs);
    free(reconstructed);
    free(multilevel_coeffs);
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Functions:**
- `haar_dwt()`: Performs forward Haar wavelet transform
- `haar_idwt()`: Performs inverse Haar wavelet transform
- `multilevel_dwt()`: Performs multi-level wavelet decomposition

### 2. **Algorithm Steps:**
1. **Forward Transform**: 
   - Computes approximation coefficients (averages)
   - Computes detail coefficients (differences)
   - Uses simple averaging and differencing operations

2. **Inverse Transform**:
   - Reconstructs original signal from coefficients
   - Combines approximation and detail components

### 3. **Haar Wavelet Properties:**
- Simplest possible wavelet
- Uses 2-point averaging and differencing
- Orthogonal transform
- Perfect reconstruction

### 4. **Example Output:**
```
Original Signal:
Signal: 1.00 2.00 3.00 4.00 5.00 6.00 7.00 8.00 

DWT Coefficients:
Coeffs: 1.50 3.50 5.50 7.50 -0.50 -0.50 -0.50 -0.50 

Reconstructed Signal:
Recon: 1.00 2.00 3.00 4.00 5.00 6.00 7.00 8.00 
```

## Compilation and Execution:

```bash
gcc -o wavelet wavelet.c -lm
./wavelet
```

This implementation demonstrates the fundamental concepts of wavelet transforms with a practical, working example that can be easily extended for more complex wavelets or applications.