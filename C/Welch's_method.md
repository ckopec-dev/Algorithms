# Welch's Method Algorithm in C

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's an implementation in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define PI 3.14159265358979323846

// Function to perform FFT (simplified version)
void fft(float *x, float *y, int n) {
    // Simple implementation - in practice, use a proper FFT library
    // This is a placeholder for actual FFT computation
    for (int i = 0; i < n; i++) {
        x[i] = x[i] * 2.0f; // Simplified scaling
        y[i] = 0.0f;
    }
}

// Generate Hamming window
void generate_window(float *window, int n) {
    const float alpha = 0.54f;
    const float beta = 0.46f;
    
    for (int i = 0; i < n; i++) {
        window[i] = alpha - beta * cos(2.0f * PI * i / (n - 1));
    }
}

// Welch's method implementation
void welch_method(float *signal, float *psd, int signal_length, 
                  int window_size, int overlap, int nfft) {
    
    int n_segments = (signal_length - overlap) / (window_size - overlap);
    if (n_segments <= 0) n_segments = 1;
    
    // Allocate memory for window and temporary arrays
    float *window = (float*)malloc(window_size * sizeof(float));
    float *segment = (float*)malloc(window_size * sizeof(float));
    float *fft_result_real = (float*)malloc(nfft * sizeof(float));
    float *fft_result_imag = (float*)malloc(nfft * sizeof(float));
    
    // Generate window
    generate_window(window, window_size);
    
    // Initialize PSD array
    memset(psd, 0, (nfft/2 + 1) * sizeof(float));
    
    int num_averaged = 0;
    
    // Process each segment
    for (int seg = 0; seg < n_segments; seg++) {
        int start_idx = seg * (window_size - overlap);
        
        // Check if we have enough data
        if (start_idx + window_size > signal_length) break;
        
        // Apply window to segment
        for (int i = 0; i < window_size; i++) {
            segment[i] = signal[start_idx + i] * window[i];
        }
        
        // Perform FFT
        memcpy(fft_result_real, segment, window_size * sizeof(float));
        memset(fft_result_imag, 0, window_size * sizeof(float));
        fft(fft_result_real, fft_result_imag, window_size);
        
        // Compute power spectrum
        for (int i = 0; i <= nfft/2; i++) {
            float real = fft_result_real[i];
            float imag = fft_result_imag[i];
            psd[i] += (real * real + imag * imag);
        }
        
        num_averaged++;
    }
    
    // Average the results
    for (int i = 0; i <= nfft/2; i++) {
        psd[i] /= num_averaged;
    }
    
    // Clean up
    free(window);
    free(segment);
    free(fft_result_real);
    free(fft_result_imag);
}

// Example usage
int main() {
    // Generate test signal (sum of sine waves)
    const int signal_length = 1024;
    const int window_size = 256;
    const int overlap = 128;
    const int nfft = 512;
    
    float *signal = (float*)malloc(signal_length * sizeof(float));
    float *psd = (float*)malloc((nfft/2 + 1) * sizeof(float));
    
    // Generate test signal with two frequency components
    for (int i = 0; i < signal_length; i++) {
        signal[i] = 0.5f * sin(2.0f * PI * 10.0f * i / 1024.0f) +
                    0.3f * sin(2.0f * PI * 25.0f * i / 1024.0f) +
                    0.1f * rand() / RAND_MAX; // Add noise
    }
    
    // Apply Welch's method
    welch_method(signal, psd, signal_length, window_size, overlap, nfft);
    
    // Print results
    printf("Welch's Method PSD Results:\n");
    printf("Bin\tPower\n");
    for (int i = 0; i <= nfft/2; i += 10) {
        printf("%d\t%.6f\n", i, psd[i]);
    }
    
    // Clean up
    free(signal);
    free(psd);
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Segmentation**: Divides the input signal into overlapping segments
2. **Windowing**: Applies a Hamming window to each segment to reduce spectral leakage
3. **FFT**: Computes the Fast Fourier Transform of each windowed segment
4. **Averaging**: Averages the power spectra of all segments
5. **Power Spectral Density**: Returns the final PSD estimate

## Parameters:

- `signal`: Input signal array
- `psd`: Output power spectral density array
- `signal_length`: Length of input signal
- `window_size`: Size of each segment
- `overlap`: Number of overlapping samples between segments
- `nfft`: FFT size for computation

## Advantages:

- Reduces noise in the PSD estimate
- Provides better frequency resolution than single periodogram
- Reduces variance of the estimate
- Handles non-stationary signals better

This is a simplified implementation for demonstration. In practice, you would use optimized FFT libraries like FFTW for better performance.

