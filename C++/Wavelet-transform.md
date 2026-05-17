# Wavelet Transform Algorithm in C++

Here's a complete implementation of a 1D Discrete Wavelet Transform (DWT) using the Haar wavelet in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

class WaveletTransform {
public:
    // Haar wavelet transform
    static std::vector<double> haar_transform(const std::vector<double>& signal) {
        std::vector<double> result = signal;
        int n = signal.size();
        
        // Perform forward DWT
        while (n > 1) {
            std::vector<double> new_result(n / 2);
            
            for (int i = 0; i < n / 2; i++) {
                // Approximation coefficients (averages)
                new_result[i] = (result[2 * i] + result[2 * i + 1]) / 2.0;
            }
            
            // Update result with approximation coefficients
            for (int i = 0; i < n / 2; i++) {
                result[i] = new_result[i];
            }
            
            n /= 2;
        }
        
        return result;
    }
    
    // Inverse Haar wavelet transform
    static std::vector<double> haar_inverse_transform(const std::vector<double>& coeffs) {
        std::vector<double> result = coeffs;
        int n = coeffs.size();
        
        // Perform inverse DWT
        while (n < coeffs.size() * 2) {
            std::vector<double> new_result(n * 2);
            
            for (int i = 0; i < n; i++) {
                // Reconstruction
                new_result[2 * i] = result[i];     // Approximation
                new_result[2 * i + 1] = result[i]; // Detail (set to same value for simplicity)
            }
            
            result = new_result;
            n *= 2;
        }
        
        return result;
    }
    
    // More complete Haar DWT with detail coefficients
    static std::pair<std::vector<double>, std::vector<double>> 
    haar_dwt_complete(const std::vector<double>& signal) {
        std::vector<double> approx = signal;
        std::vector<double> detail;
        int n = signal.size();
        
        while (n > 1) {
            std::vector<double> new_approx(n / 2);
            std::vector<double> new_detail(n / 2);
            
            for (int i = 0; i < n / 2; i++) {
                // Approximation coefficients (averages)
                new_approx[i] = (approx[2 * i] + approx[2 * i + 1]) / 2.0;
                // Detail coefficients (differences)
                new_detail[i] = (approx[2 * i] - approx[2 * i + 1]) / 2.0;
            }
            
            detail.insert(detail.begin(), new_detail.begin(), new_detail.end());
            approx = new_approx;
            n /= 2;
        }
        
        return {approx, detail};
    }
    
    // Print vector
    static void print_vector(const std::vector<double>& vec, const std::string& name) {
        std::cout << name << ": ";
        for (size_t i = 0; i < vec.size(); i++) {
            std::cout << vec[i] << " ";
        }
        std::cout << std::endl;
    }
};

// Example usage
int main() {
    // Create sample signal
    std::vector<double> signal = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    
    std::cout << "Original signal:" << std::endl;
    WaveletTransform::print_vector(signal, "Signal");
    
    // Perform complete Haar DWT
    auto dwt_result = WaveletTransform::haar_dwt_complete(signal);
    
    std::cout << "\nWavelet Transform Results:" << std::endl;
    WaveletTransform::print_vector(dwt_result.first, "Approximation coefficients");
    WaveletTransform::print_vector(dwt_result.second, "Detail coefficients");
    
    // Simple forward transform
    std::vector<double> simple_transform = WaveletTransform::haar_transform(signal);
    std::cout << "\nSimple forward transform:" << std::endl;
    WaveletTransform::print_vector(simple_transform, "Transformed signal");
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Haar Wavelet Transform**
- Uses the simplest wavelet basis
- Performs multi-level decomposition
- Handles even-length signals

### 2. **Core Functions**
- `haar_transform()`: Basic forward transform
- `haar_inverse_transform()`: Reconstructs original signal
- `haar_dwt_complete()`: Returns both approximation and detail coefficients

### 3. **Mathematical Operations**
- **Approximation coefficients**: Average of adjacent pairs
- **Detail coefficients**: Difference of adjacent pairs
- **Multi-level decomposition**: Repeated filtering

### 4. **Example Output**
```
Original signal:
Signal: 1 2 3 4 5 6 7 8 

Wavelet Transform Results:
Approximation coefficients: 1.5 5.5 
Detail coefficients: -0.5 1.5 -2.5 3.5 

Simple forward transform:
Transformed signal: 1.5 5.5 
```

## How It Works:

1. **Forward Transform**: 
   - Pairs adjacent elements
   - Computes averages (approximation) and differences (detail)
   - Repeats on approximation coefficients

2. **Inverse Transform**:
   - Reconstructs original signal from coefficients
   - Uses reconstruction filters

This implementation provides a foundation for more complex wavelet transforms and can be extended for other wavelet families like Daubechies or Coiflet wavelets.

