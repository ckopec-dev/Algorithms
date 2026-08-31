# Welch's Method Algorithm in C++

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <complex>
#include <algorithm>

class WelchMethod {
private:
    int N;           // Signal length
    int M;           // Segment length
    int overlap;     // Overlap between segments
    int numSegments; // Number of segments
    
    // FFT function using Cooley-Tukey algorithm
    void fft(std::vector<std::complex<double>>& x) {
        int n = x.size();
        if (n <= 1) return;
        
        // Bit-reversal permutation
        for (int i = 1, j = 0; i < n; i++) {
            int bit = n >> 1;
            for (; j & bit; bit >>= 1) {
                j ^= bit;
            }
            j ^= bit;
            if (i < j) {
                std::swap(x[i], x[j]);
            }
        }
        
        // Cooley-Tukey FFT
        for (int len = 2; len <= n; len <<= 1) {
            double angle = -2 * M_PI / len;
            std::complex<double> wlen(cos(angle), sin(angle));
            for (int i = 0; i < n; i += len) {
                std::complex<double> w(1.0, 0.0);
                for (int j = 0; j < len / 2; j++) {
                    std::complex<double> u = x[i + j];
                    std::complex<double> v = x[i + j + len / 2] * w;
                    x[i + j] = u + v;
                    x[i + j + len / 2] = u - v;
                    w *= wlen;
                }
            }
        }
    }
    
public:
    WelchMethod(int signalLength, int segmentLength, int overlapRatio) 
        : N(signalLength), M(segmentLength), overlap(overlapRatio) {
        numSegments = (N - overlap) / (M - overlap);
        if (numSegments <= 0) numSegments = 1;
    }
    
    // Generate power spectral density estimate
    std::vector<double> estimatePSD(const std::vector<double>& signal) {
        std::vector<double> psd(M / 2); // Only positive frequencies
        
        // Window function (Hamming window)
        auto hammingWindow = [](int n, int M) -> double {
            const double alpha = 0.54;
            const double beta = 1.0 - alpha;
            return alpha - beta * cos(2 * M_PI * n / (M - 1));
        };
        
        std::vector<std::complex<double>> segment(M);
        std::vector<std::complex<double>> fftResult(M);
        
        for (int i = 0; i < numSegments; i++) {
            int startIdx = i * (M - overlap);
            if (startIdx + M > N) break;
            
            // Apply window and copy segment
            for (int j = 0; j < M; j++) {
                double windowValue = hammingWindow(j, M);
                segment[j] = std::complex<double>(signal[startIdx + j] * windowValue, 0.0);
            }
            
            // Compute FFT
            fftResult = segment;
            fft(fftResult);
            
            // Accumulate power spectral density
            for (int k = 0; k < M / 2; k++) {
                double magnitude = std::abs(fftResult[k]);
                psd[k] += magnitude * magnitude;
            }
        }
        
        // Average and normalize
        double normalization = 1.0 / numSegments;
        for (int i = 0; i < M / 2; i++) {
            psd[i] *= normalization;
        }
        
        return psd;
    }
    
    // Simple test function to generate a test signal
    static std::vector<double> generateTestSignal(int length) {
        std::vector<double> signal(length);
        for (int i = 0; i < length; i++) {
            // Generate a signal with two sinusoids plus noise
            signal[i] = 0.5 * sin(2 * M_PI * 0.1 * i) + 
                       0.3 * sin(2 * M_PI * 0.2 * i) +
                       0.1 * (rand() / (double)RAND_MAX - 0.5); // Add noise
        }
        return signal;
    }
};

// Example usage
int main() {
    // Parameters
    int signalLength = 1024;
    int segmentLength = 256;
    int overlapRatio = 128; // 50% overlap
    
    // Generate test signal
    std::vector<double> signal = WelchMethod::generateTestSignal(signalLength);
    
    // Create Welch method instance
    WelchMethod welch(signalLength, segmentLength, overlapRatio);
    
    // Estimate PSD
    std::vector<double> psd = welch.estimatePSD(signal);
    
    // Output results
    std::cout << "Welch's Method - Power Spectral Density Estimate\n";
    std::cout << "================================================\n";
    std::cout << "Segment length: " << segmentLength << "\n";
    std::cout << "Number of segments: " << (segmentLength - overlapRatio) / (segmentLength - overlapRatio) << "\n";
    std::cout << "Frequency bins: " << psd.size() << "\n\n";
    
    std::cout << "PSD values (first 20 bins):\n";
    for (int i = 0; i < std::min(20, (int)psd.size()); i++) {
        std::cout << "Bin " << i << ": " << psd[i] << "\n";
    }
    
    return 0;
}
```

## Key Features of this Implementation:

1. **FFT Implementation**: Uses Cooley-Tukey FFT algorithm for efficient computation
2. **Windowing**: Applies Hamming window to reduce spectral leakage
3. **Segmentation**: Divides signal into overlapping segments
4. **Averaging**: Averages the periodograms of all segments
5. **Normalization**: Properly normalizes the final PSD estimate

## Algorithm Steps:

1. Divide input signal into overlapping segments
2. Apply window function to each segment
3. Compute FFT of each segment
4. Calculate power spectrum (magnitude squared)
5. Average all power spectra
6. Normalize the result

## Usage Notes:

- The overlap parameter controls how much segments overlap (0 = no overlap, 0.5 = 50% overlap)
- Larger segment lengths provide better frequency resolution but worse time resolution
- More overlapping segments improve the PSD estimate quality
- The method reduces noise in the PSD estimate compared to single periodogram

This implementation provides a practical foundation for power spectral density estimation in signal processing applications.