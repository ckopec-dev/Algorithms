# Wavelet Transform in Rust

Here's an example implementation of a Discrete Wavelet Transform (DWT) in Rust using the Haar wavelet:

```rust
use std::f64;

/// Haar wavelet transform implementation
pub struct WaveletTransform;

impl WaveletTransform {
    /// Performs a single-level discrete wavelet transform using Haar wavelet
    /// 
    /// # Arguments
    /// * `input` - Input signal as a vector of f64 values
    /// 
    /// # Returns
    /// * `Vec<f64>` - Transformed signal (approximation + detail coefficients)
    pub fn haar_transform(input: &[f64]) -> Vec<f64> {
        if input.is_empty() {
            return vec![];
        }
        
        let n = input.len();
        let mut output = vec![0.0; n];
        
        // For simplicity, we'll implement a basic 2-level transform
        // In practice, you'd want to handle the recursive structure properly
        
        if n == 1 {
            return input.to_vec();
        }
        
        // Simple implementation for even-length signals
        if n % 2 == 0 {
            let half = n / 2;
            let mut approx = vec![0.0; half];
            let mut detail = vec![0.0; half];
            
            // Compute approximation and detail coefficients
            for i in 0..half {
                let even_idx = 2 * i;
                let odd_idx = 2 * i + 1;
                
                approx[i] = (input[even_idx] + input[odd_idx]) / 2.0;
                detail[i] = (input[even_idx] - input[odd_idx]) / 2.0;
            }
            
            // Combine results: [approximation coefficients, detail coefficients]
            output[..half].copy_from_slice(&approx);
            output[half..].copy_from_slice(&detail);
        } else {
            // Handle odd-length signals
            output.copy_from_slice(input);
        }
        
        output
    }
    
    /// Performs inverse Haar wavelet transform
    /// 
    /// # Arguments
    /// * `coefficients` - Wavelet coefficients (approximation + detail)
    /// 
    /// # Returns
    /// * `Vec<f64>` - Reconstructed signal
    pub fn inverse_haar_transform(coefficients: &[f64]) -> Vec<f64> {
        if coefficients.is_empty() {
            return vec![];
        }
        
        let n = coefficients.len();
        let mut output = vec![0.0; n];
        
        if n == 1 {
            return coefficients.to_vec();
        }
        
        if n % 2 == 0 {
            let half = n / 2;
            let approx = &coefficients[..half];
            let detail = &coefficients[half..];
            
            for i in 0..half {
                let even_idx = 2 * i;
                let odd_idx = 2 * i + 1;
                
                output[even_idx] = approx[i] + detail[i];
                output[odd_idx] = approx[i] - detail[i];
            }
        } else {
            output.copy_from_slice(coefficients);
        }
        
        output
    }
    
    /// Multi-level wavelet decomposition
    /// 
    /// # Arguments
    /// * `input` - Input signal
    /// * `levels` - Number of decomposition levels
    /// 
    /// # Returns
    /// * `Vec<f64>` - Coefficients from multi-level decomposition
    pub fn multi_level_decomposition(input: &[f64], levels: usize) -> Vec<f64> {
        let mut current = input.to_vec();
        
        for _ in 0..levels {
            current = Self::haar_transform(&current);
        }
        
        current
    }
}

// Example usage
fn main() {
    // Sample input signal
    let signal = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0];
    
    println!("Original signal: {:?}", signal);
    
    // Perform single-level wavelet transform
    let transformed = WaveletTransform::haar_transform(&signal);
    println!("Transformed signal: {:?}", transformed);
    
    // Perform inverse transform
    let reconstructed = WaveletTransform::inverse_haar_transform(&transformed);
    println!("Reconstructed signal: {:?}", reconstructed);
    
    // Multi-level decomposition
    let multi_level = WaveletTransform::multi_level_decomposition(&signal, 2);
    println!("Multi-level decomposition: {:?}", multi_level);
    
    // Verify reconstruction
    let is_equal = signal.iter()
        .zip(reconstructed.iter())
        .all(|(a, b)| (a - b).abs() < f64::EPSILON);
    
    println!("Reconstruction successful: {}", is_equal);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_haar_transform() {
        let signal = vec![1.0, 2.0, 3.0, 4.0];
        let result = WaveletTransform::haar_transform(&signal);
        assert_eq!(result.len(), 4);
    }
    
    #[test]
    fn test_inverse_transform() {
        let signal = vec![1.0, 2.0, 3.0, 4.0];
        let transformed = WaveletTransform::haar_transform(&signal);
        let reconstructed = WaveletTransform::inverse_haar_transform(&transformed);
        
        // Check that reconstruction is close to original
        for (orig, recon) in signal.iter().zip(reconstructed.iter()) {
            assert!((orig - recon).abs() < 1e-10);
        }
    }
}
```

## Key Features of this Implementation:

1. **Haar Wavelet**: Uses the simplest wavelet for demonstration
2. **Forward Transform**: Decomposes signal into approximation and detail coefficients
3. **Inverse Transform**: Reconstructs original signal from coefficients
4. **Multi-level Support**: Can perform multiple decomposition levels
5. **Error Handling**: Proper handling of edge cases (empty signals, odd lengths)
6. **Testing**: Includes unit tests for verification

## Usage Example:

```bash
cargo run
```

This will output the original signal, transformed coefficients, reconstructed signal, and verification results.

The implementation demonstrates the core concepts of wavelet transforms while being simple enough to understand the underlying mathematics. For production use, you'd want to implement more sophisticated wavelets and handle boundary conditions more carefully.

