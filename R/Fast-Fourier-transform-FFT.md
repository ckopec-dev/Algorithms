# Fast Fourier Transform (FFT) in R

Here's an example demonstrating the FFT algorithm in R:

```r
# Create a sample signal with multiple frequency components
# Generate time vector
t <- seq(0, 1, length.out = 1024)
# Create a signal with 50Hz and 120Hz components
signal <- 2*sin(2*pi*50*t) + 1.5*sin(2*pi*120*t) + 0.5*rnorm(length(t))

# Apply FFT
fft_result <- fft(signal)

# Calculate the frequency vector
n <- length(signal)
fs <- 1000  # Sampling frequency (assumed)
freq <- (0:(n-1)) * fs / n

# Plot the results
par(mfrow = c(2, 2))

# Original signal
plot(t, signal, type = "l", main = "Original Signal", 
     xlab = "Time", ylab = "Amplitude")

# Magnitude of FFT
plot(freq[1:n/2], Mod(fft_result)[1:n/2], type = "l", 
     main = "FFT Magnitude", xlab = "Frequency (Hz)", ylab = "Magnitude")

# Phase of FFT
plot(freq[1:n/2], Arg(fft_result)[1:n/2], type = "l", 
     main = "FFT Phase", xlab = "Frequency (Hz)", ylab = "Phase (radians)")

# Reconstruct signal using inverse FFT
reconstructed <- Re(fft(fft_result, inverse = TRUE) / n)
plot(t, reconstructed, type = "l", main = "Reconstructed Signal", 
     xlab = "Time", ylab = "Amplitude")

# Compare original and reconstructed
plot(t, signal, type = "l", col = "blue", main = "Original vs Reconstructed",
     xlab = "Time", ylab = "Amplitude")
lines(t, reconstructed, col = "red", lty = 2)
legend("topright", c("Original", "Reconstructed"), col = c("blue", "red"), 
       lty = c(1, 2))
```

## Key Functions Used:

- `fft()`: Computes the Fast Fourier Transform
- `Mod()`: Returns the modulus (magnitude) of complex numbers
- `Arg()`: Returns the argument (phase) of complex numbers
- `Re()`: Returns the real part of complex numbers
- `fft(inverse = TRUE)`: Computes the inverse FFT

## Output Description:

1. **Original Signal**: Shows the time-domain signal with multiple frequency components
2. **FFT Magnitude**: Displays the frequency spectrum showing peaks at 50Hz and 120Hz
3. **FFT Phase**: Shows the phase information across frequencies
4. **Reconstructed Signal**: Demonstrates that the original signal can be perfectly reconstructed
5. **Comparison**: Shows that the original and reconstructed signals are identical

## Important Notes:

- The FFT function in R returns complex numbers
- For real signals, the FFT is symmetric, so we typically only plot the first half
- The `inverse = TRUE` parameter is used for inverse FFT
- The magnitude spectrum shows the amplitude of each frequency component
- The phase spectrum shows the phase shift of each frequency component

This example demonstrates how FFT can be used to analyze the frequency content of signals and how the original signal can be perfectly reconstructed from its frequency domain representation.

