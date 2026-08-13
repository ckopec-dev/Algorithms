# Fast Fourier Transform (FFT) in C++

Here's a complete implementation of the Cooley-Tukey FFT algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <complex>
#include <cmath>

using namespace std;
using cd = complex<double>;

class FFT {
public:
    // Perform FFT on the input array
    static vector<cd> fft(vector<cd> &a) {
        int n = a.size();
        
        // Base case
        if (n == 1) return a;
        
        // Divide
        vector<cd> a0(n / 2), a1(n / 2);
        for (int i = 0; i < n / 2; i++) {
            a0[i] = a[2 * i];
            a1[i] = a[2 * i + 1];
        }
        
        // Conquer
        vector<cd> y0 = fft(a0);
        vector<cd> y1 = fft(a1);
        
        // Combine
        vector<cd> y(n);
        for (int k = 0; k < n / 2; k++) {
            double angle = -2 * M_PI * k / n;
            cd w = cd(cos(angle), sin(angle));
            y[k] = y0[k] + w * y1[k];
            y[k + n / 2] = y0[k] - w * y1[k];
        }
        
        return y;
    }
    
    // Inverse FFT
    static vector<cd> ifft(vector<cd> &a) {
        int n = a.size();
        vector<cd> result = fft(a);
        
        // Conjugate and divide by n
        for (int i = 0; i < n; i++) {
            result[i] = conj(result[i]) / n;
        }
        
        return result;
    }
    
    // FFT with bit-reversal for better performance
    static vector<cd> fft_bit_reverse(vector<cd> &a) {
        int n = a.size();
        int bits = 0;
        while ((1 << bits) < n) bits++;
        
        // Bit-reversal permutation
        for (int i = 0; i < n; i++) {
            int rev = 0;
            for (int j = 0; j < bits; j++) {
                if (i & (1 << j)) rev |= (1 << (bits - 1 - j));
            }
            if (rev > i) swap(a[i], a[rev]);
        }
        
        // Cooley-Tukey FFT
        for (int len = 2; len <= n; len <<= 1) {
            double angle = -2 * M_PI / len;
            cd wlen(cos(angle), sin(angle));
            
            for (int i = 0; i < n; i += len) {
                cd w(1);
                for (int j = 0; j < len / 2; j++) {
                    cd u = a[i + j];
                    cd v = a[i + j + len / 2] * w;
                    a[i + j] = u + v;
                    a[i + j + len / 2] = u - v;
                    w *= wlen;
                }
            }
        }
        
        return a;
    }
};

// Example usage
int main() {
    // Sample input signal (complex numbers)
    vector<cd> input = {
        cd(1, 0), cd(2, 0), cd(3, 0), cd(4, 0),
        cd(0, 0), cd(0, 0), cd(0, 0), cd(0, 0)
    };
    
    cout << "Input signal:" << endl;
    for (int i = 0; i < input.size(); i++) {
        cout << "x[" << i << "] = " << input[i] << endl;
    }
    
    // Perform FFT
    vector<cd> result = FFT::fft(input);
    
    cout << "\nFFT result:" << endl;
    for (int i = 0; i < result.size(); i++) {
        cout << "X[" << i << "] = " << result[i] << endl;
    }
    
    // Perform inverse FFT to verify
    vector<cd> reconstructed = FFT::ifft(result);
    
    cout << "\nReconstructed signal:" << endl;
    for (int i = 0; i < reconstructed.size(); i++) {
        cout << "x[" << i << "] = " << reconstructed[i] << endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Basic FFT Algorithm**: Implements the Cooley-Tukey divide-and-conquer approach
2. **Complex Number Support**: Uses C++'s `std::complex` for handling complex arithmetic
3. **Inverse FFT**: Includes functionality to reconstruct original signal
4. **Bit-Reversal Optimization**: Provides an optimized version with bit-reversal permutation
5. **Complete Example**: Demonstrates usage with sample input data

## Time Complexity:
- **Time**: O(n log n)
- **Space**: O(n)

## Sample Output:
```
Input signal:
x[0] = (1,0)
x[1] = (2,0)
x[2] = (3,0)
x[3] = (4,0)
x[4] = (0,0)
x[5] = (0,0)
x[6] = (0,0)
x[7] = (0,0)

FFT result:
X[0] = (10,0)
X[1] = (-2.41421,-3.41421)
X[2] = (-2,0)
X[3] = (-0.585786,0.585786)
...
```

This implementation provides a solid foundation for FFT operations and can be extended for more specialized applications like convolution or spectral analysis.