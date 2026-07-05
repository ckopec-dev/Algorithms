# Welch's Method Algorithm in C

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging multiple periodograms. Here's an implementation in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>

#define PI 3.14159265358979323846

// Function to compute FFT using Cooley-Tukey algorithm
void fft(double complex *x, int n) {
    if (n <= 1) return;
    
    // Bit-reversal permutation
    for (int i = 1, j = 0; i < n; i++) {
        int bit = n >> 1;
        for (j = 0; j < n; j >>= 1) {
            if (j & 1) break;
            bit >>= 1;
        }
        if (i < j) {
            double complex temp = x[i];
            x[i] = x[j];
            x[j] = temp;
        }
    }
    
    // Cooley-Tukey FFT
    for (int len = 2; len <= n; len <<= 1) {
        double complex wlen = cexp(-2 * PI * I / len);
        for (int i = 0; i < n; i += len) {
            double complex w = 1;
            for (int j = 0; j < len / 2; j++) {
                double complex u = x[i + j];
                double complex v = x[i + j + len / 2] * w;
                x[i + j] = u + v;
                x[i + j + len / 2] = u - v;
                w *= wlen;
            }
        }
    }
}

// Welch's method implementation
void welch_method(double *signal, int signal_length, double *psd, int psd_length,
                  int window_size, int overlap, int nfft) {
    
    // Allocate memory for window and temporary arrays
    double *window = (double*)malloc(window_size * sizeof(double));
    double complex *temp_fft = (double complex*)malloc(nfft * sizeof(double complex));
    double complex *windowed_signal = (double complex*)malloc(window_size * sizeof(double complex));
    
    // Create Hann window
    for (int i = 0; i < window_size; i++) {
        window[i] = 0.5 * (1 - cos(2 * PI * i / (window_size - 1)));
    }
    
    // Initialize PSD array to zero
    for (int i = 0; i < psd_length; i++) {
        psd[i] = 0;
    }
    
    int num_segments = (signal_length - overlap) / (window_size - overlap);
    int total_power = 0;
    
    // Process each segment
    for (int seg = 0; seg < num_segments; seg++) {
        int start_idx = seg * (window_size - overlap);
        
        // Apply window to signal segment
        for (int i = 0; i < window_size && (start_idx + i) < signal_length; i++) {
            windowed_signal[i] = signal[start_idx + i] * window[i];
        }
        
        // Zero-padding if necessary
        for (int i = window_size; i < nfft; i++) {
            windowed_signal[i] = 0;
        }
        
        // Compute FFT
        fft(windowed_signal, nfft);
        
        // Compute power spectrum (magnitude squared)
        for (int i = 0; i < psd_length; i++) {
            double mag = cabs(windowed_signal[i]);
            psd[i] += mag * mag;
        }
        
        total_power++;
    }
    
    // Average the results
    for (int i = 0; i < psd_length; i++) {
        psd[i] /= total_power;
    }
    
    // Free allocated memory
    free(window);
    free(temp_fft);
    free(windowed_signal);
}

// Example usage
int main() {
    // Generate test signal (sum of sinusoids)
    int signal_length = 1024;
    double *signal = (double*)malloc(signal_length * sizeof(double));
    
    for (int i = 0; i < signal_length; i++) {
        signal[i] = 0.5 * sin(2 * PI * 10 * i / 1024) + 
                   0.3 * sin(2 * PI * 25 * i / 1024) +
                   0.2 * sin(2 * PI * 50 * i / 1024);
    }
    
    // Parameters for Welch's method
    int window_size = 256;
    int overlap = 128;
    int nfft = 512;
    int psd_length = nfft / 2; // Only positive frequencies
    
    double *psd = (double*)malloc(psd_length * sizeof(double));
    
    // Apply Welch's method
    welch_method(signal, signal_length, psd, psd_length, window_size, overlap, nfft);
    
    // Print results
    printf("Power Spectral Density Estimates:\n");
    printf("Bin\tPSD\n");
    for (int i = 0; i < psd_length; i++) {
        printf("%d\t%.6f\n", i, psd[i]);
    }
    
    // Free memory
    free(signal);
    free(psd);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **FFT Implementation**: Uses Cooley-Tukey algorithm for efficient computation
2. **Hann Window**: Applies Hann windowing to reduce spectral leakage
3. **Overlapping Segments**: Processes overlapping signal segments
4. **Averaging**: Averages multiple periodograms for improved estimation
5. **Memory Management**: Proper allocation and deallocation of memory

## Parameters:
- `window_size`: Size of each signal segment
- `overlap`: Number of samples overlap between consecutive segments
- `nfft`: FFT size (usually power of 2)
- `psd_length`: Length of output PSD array

This implementation provides a practical approach to spectral density estimation using Welch's method, which is particularly useful for noisy signals where traditional periodogram methods may produce unreliable results.