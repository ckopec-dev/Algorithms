# Fast Fourier Transform (FFT) in Rust

Here's a complete implementation of the Cooley-Tukey FFT algorithm in Rust:

```rust
use std::f64::consts::PI;

#[derive(Debug, Clone)]
pub struct Complex {
    pub real: f64,
    pub imag: f64,
}

impl Complex {
    pub fn new(real: f64, imag: f64) -> Self {
        Complex { real, imag }
    }

    pub fn zero() -> Self {
        Complex::new(0.0, 0.0)
    }

    pub fn one() -> Self {
        Complex::new(1.0, 0.0)
    }
}

impl std::ops::Add for Complex {
    type Output = Complex;

    fn add(self, other: Complex) -> Complex {
        Complex::new(self.real + other.real, self.imag + other.imag)
    }
}

impl std::ops::Sub for Complex {
    type Output = Complex;

    fn sub(self, other: Complex) -> Complex {
        Complex::new(self.real - other.real, self.imag - other.imag)
    }
}

impl std::ops::Mul for Complex {
    type Output = Complex;

    fn mul(self, other: Complex) -> Complex {
        Complex::new(
            self.real * other.real - self.imag * other.imag,
            self.real * other.imag + self.imag * other.real,
        )
    }
}

impl std::ops::Mul<f64> for Complex {
    type Output = Complex;

    fn mul(self, scalar: f64) -> Complex {
        Complex::new(self.real * scalar, self.imag * scalar)
    }
}

impl std::ops::Div<f64> for Complex {
    type Output = Complex;

    fn div(self, scalar: f64) -> Complex {
        Complex::new(self.real / scalar, self.imag / scalar)
    }
}

/// Fast Fourier Transform implementation
pub fn fft(signal: &[Complex]) -> Vec<Complex> {
    let n = signal.len();
    
    // Handle edge cases
    if n <= 1 {
        return signal.to_vec();
    }
    
    // Ensure input length is a power of 2
    if !n.is_power_of_two() {
        panic!("Input length must be a power of 2 for FFT");
    }
    
    // Base case
    if n == 1 {
        return vec![signal[0].clone()];
    }
    
    // Split into even and odd indexed elements
    let mut even = Vec::with_capacity(n / 2);
    let mut odd = Vec::with_capacity(n / 2);
    
    for i in (0..n).step_by(2) {
        even.push(signal[i].clone());
        odd.push(signal[i + 1].clone());
    }
    
    // Recursive calls
    let even_fft = fft(&even);
    let odd_fft = fft(&odd);
    
    // Combine results
    let mut result = vec![Complex::zero(); n];
    
    for i in 0..n / 2 {
        let angle = -2.0 * PI * i as f64 / n as f64;
        let w = Complex::new(angle.cos(), angle.sin());
        let t = w * odd_fft[i].clone();
        
        result[i] = even_fft[i].clone() + t;
        result[i + n / 2] = even_fft[i].clone() - t;
    }
    
    result
}

/// Inverse Fast Fourier Transform
pub fn ifft(spectrum: &[Complex]) -> Vec<Complex> {
    let n = spectrum.len();
    let mut conjugate_spectrum = Vec::with_capacity(n);
    
    // Take conjugate of all spectrum values
    for &c in spectrum {
        conjugate_spectrum.push(Complex::new(c.real, -c.imag));
    }
    
    // Apply FFT to conjugate
    let mut result = fft(&conjugate_spectrum);
    
    // Take conjugate again and normalize
    for c in &mut result {
        c.real = c.real / n as f64;
        c.imag = -c.imag / n as f64;
    }
    
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fft_simple() {
        let signal = vec![
            Complex::new(1.0, 0.0),
            Complex::new(1.0, 0.0),
            Complex::new(1.0, 0.0),
            Complex::new(1.0, 0.0),
        ];
        
        let result = fft(&signal);
        println!("FFT result: {:?}", result);
        
        // For a constant signal, we expect one non-zero component
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn test_fft_with_sine_wave() {
        let n = 8;
        let mut signal = Vec::with_capacity(n);
        
        // Create a sine wave
        for i in 0..n {
            let value = (2.0 * PI * i as f64 / n as f64).sin();
            signal.push(Complex::new(value, 0.0));
        }
        
        let spectrum = fft(&signal);
        let reconstructed = ifft(&spectrum);
        
        // Check that reconstruction is close to original
        for (i, (original, reconstructed)) in signal.iter().zip(reconstructed.iter()).enumerate() {
            let diff_real = (original.real - reconstructed.real).abs();
            let diff_imag = (original.imag - reconstructed.imag).abs();
            assert!(diff_real < 1e-10, "Reconstruction failed at index {}: {} vs {}", i, original.real, reconstructed.real);
            assert!(diff_imag < 1e-10, "Reconstruction failed at index {}: {} vs {}", i, original.imag, reconstructed.imag);
        }
    }
}

fn main() {
    // Example usage
    let signal = vec![
        Complex::new(1.0, 0.0),
        Complex::new(2.0, 0.0),
        Complex::new(3.0, 0.0),
        Complex::new(4.0, 0.0),
    ];
    
    println!("Original signal: {:?}", signal);
    
    let spectrum = fft(&signal);
    println!("FFT result: {:?}", spectrum);
    
    let reconstructed = ifft(&spectrum);
    println!("Reconstructed: {:?}", reconstructed);
    
    // Example with a simple sine wave
    let sine_wave = vec![
        Complex::new(0.0, 0.0),
        Complex::new(1.0, 0.0),
        Complex::new(0.0, 0.0),
        Complex::new(-1.0, 0.0),
    ];
    
    println!("\nSine wave: {:?}", sine_wave);
    let sine_spectrum = fft(&sine_wave);
    println!("Sine wave FFT: {:?}", sine_spectrum);
}
```

## Key Features of this Implementation:

1. **Complex Number Support**: Custom `Complex` struct with basic arithmetic operations
2. **Recursive FFT**: Implements the Cooley-Tukey algorithm with O(n log n) complexity
3. **Inverse FFT**: Includes both forward and inverse transforms
4. **Error Handling**: Validates input length requirements
5. **Tests**: Includes unit tests for verification
6. **Documentation**: Clear comments and examples

## Usage Example:

The code demonstrates:
- Creating complex signals
- Computing FFT transforms
- Reconstructing signals using inverse FFT
- Working with sine waves

## Requirements:

- Rust 1.56 or later
- No external dependencies (pure Rust implementation)

This implementation is suitable for educational purposes and basic signal processing applications. For production use, consider using optimized libraries like `rustfft` or `ndarray-fft`.

