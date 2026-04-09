# Wavelet Transform in R

Here's an example of implementing a Wavelet Transform using the `waveslim` package in R:

```r
# Install and load required packages
install.packages(c("waveslim", "ggplot2"))
library(waveslim)
library(ggplot2)

# Create sample signal (combination of sine waves)
t <- seq(0, 4, length.out = 1024)
signal <- sin(2 * pi * 2 * t) + 0.5 * sin(2 * pi * 8 * t) + 
          0.3 * sin(2 * pi * 16 * t) + 0.1 * rnorm(1024)

# Perform Continuous Wavelet Transform (CWT)
# Using Morlet wavelet
cwt_result <- cwt(signal, wavelet = "morl", scales = 1:64)

# Perform Discrete Wavelet Transform (DWT)
# Using Haar wavelet
dwt_result <- dwt(signal, filter = "haar", nlevels = 5)

# Plot the original signal
plot(t, signal, type = "l", main = "Original Signal", 
     xlab = "Time", ylab = "Amplitude", col = "blue", lwd = 2)

# Plot the Continuous Wavelet Transform
# Create a heatmap of the CWT coefficients
image.plot(t, 1:64, abs(cwt_result), 
           main = "Continuous Wavelet Transform (Magnitude)", 
           xlab = "Time", ylab = "Scale")

# Plot DWT coefficients
plot(dwt_result, main = "Discrete Wavelet Transform Coefficients")

# Extract approximation and detail coefficients
approx_coeffs <- dwt_result$W[[1]]  # Approximation coefficients
detail_coeffs <- dwt_result$W[[2]]  # Detail coefficients

# Reconstruct signal from DWT
reconstructed <- idwt(dwt_result)

# Plot original vs reconstructed signal
plot(t, signal, type = "l", main = "Original vs Reconstructed Signal",
     xlab = "Time", ylab = "Amplitude", col = "blue", lwd = 2)
lines(t, reconstructed, col = "red", lwd = 2)
legend("topright", c("Original", "Reconstructed"), 
       col = c("blue", "red"), lwd = 2)

# Print information about the wavelet transform
cat("Wavelet Transform Analysis:\n")
cat("Original signal length:", length(signal), "\n")
cat("DWT levels:", dwt_result$nlevels, "\n")
cat("Approximation coefficients length:", length(approx_coeffs), "\n")
cat("Detail coefficients length:", length(detail_coeffs), "\n")
```

## Key Functions Used:

- **`cwt()`**: Continuous Wavelet Transform
- **`dwt()`**: Discrete Wavelet Transform  
- **`idwt()`**: Inverse Discrete Wavelet Transform
- **`image.plot()`**: For visualizing wavelet coefficients

## Wavelet Types Available:

- `"haar"` - Haar wavelet
- `"db4"` - Daubechies wavelet (4 coefficients)
- `"sym4"` - Symlets wavelet (4 coefficients)
- `"morl"` - Morlet wavelet
- `"cgau4"` - Complex Gaussian wavelet

## Key Features:

1. **Time-Frequency Analysis**: Shows how signal frequency content changes over time
2. **Multi-resolution Analysis**: Captures both high and low frequency components
3. **Signal Denoising**: Can be used to remove noise from signals
4. **Compression**: Useful for data compression applications

This example demonstrates both continuous and discrete wavelet transforms, showing how they can be used to analyze signals in both time and frequency domains.

