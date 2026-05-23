# Welch's Method Algorithm in Python

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's a complete implementation:

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

def welch_method(x, fs=1.0, nperseg=256, noverlap=None, nfft=None, 
                 window='hann', scaling='density', return_onesided=True):
    """
    Estimate power spectral density using Welch's method.
    
    Parameters:
    -----------
    x : array_like
        Input signal
    fs : float, optional
        Sampling frequency (default: 1.0)
    nperseg : int, optional
        Length of each segment (default: 256)
    noverlap : int, optional
        Number of points of overlap between segments (default: nperseg//2)
    nfft : int, optional
        Length of FFT (default: nperseg)
    window : str or tuple, optional
        Window to apply to each segment (default: 'hann')
    scaling : str, optional
        Scaling type ('density' or 'spectrum') (default: 'density')
    return_onesided : bool, optional
        Return only one-sided spectrum (default: True)
    
    Returns:
    --------
    f : array
        Array of sample frequencies
    Pxx : array
        Power spectral density estimates
    """
    
    # Handle default overlap
    if noverlap is None:
        noverlap = nperseg // 2
    
    # Handle default FFT length
    if nfft is None:
        nfft = nperseg
    
    # Calculate number of segments
    nseg = int((len(x) - noverlap) / (nperseg - noverlap))
    
    # Initialize arrays
    Pxx = np.zeros(nfft // 2 + 1 if return_onesided else nfft)
    
    # Process each segment
    for i in range(nseg):
        # Extract segment
        start_idx = i * (nperseg - noverlap)
        end_idx = start_idx + nperseg
        
        if end_idx > len(x):
            break
            
        segment = x[start_idx:end_idx]
        
        # Apply window
        windowed_segment = segment * window
        windowed_segment = windowed_segment.astype(np.complex128)
        
        # Compute FFT
        fft_result = np.fft.fft(windowed_segment, n=nfft)
        
        # Compute power
        if scaling == 'density':
            power = np.abs(fft_result)**2 / (fs * np.sum(windowed_segment**2))
        else:
            power = np.abs(fft_result)**2
            
        # Accumulate
        if return_onesided:
            Pxx += power[:nfft//2 + 1]
        else:
            Pxx += power
    
    # Average
    Pxx = Pxx / nseg
    
    # Generate frequency array
    if return_onesided:
        f = np.linspace(0, fs/2, nfft//2 + 1)
    else:
        f = np.linspace(0, fs, nfft)
    
    return f, Pxx

# Example usage
def example_welch():
    # Generate test signal
    fs = 100  # Sampling frequency
    T = 2.0   # Duration
    t = np.linspace(0, T, int(fs * T), endpoint=False)
    
    # Create signal with noise and sinusoids
    x = (np.sin(2 * np.pi * 10 * t) + 
         0.5 * np.sin(2 * np.pi * 25 * t) + 
         0.3 * np.random.randn(len(t)))
    
    # Apply Welch's method
    f, Pxx = welch_method(x, fs=fs, nperseg=64, noverlap=32)
    
    # Compare with scipy's built-in welch
    f_scipy, Pxx_scipy = signal.welch(x, fs=fs, nperseg=64, noverlap=32)
    
    # Plot results
    plt.figure(figsize=(12, 8))
    
    plt.subplot(2, 1, 1)
    plt.plot(t[:100], x[:100])
    plt.title('Original Signal')
    plt.xlabel('Time [s]')
    plt.ylabel('Amplitude')
    plt.grid(True)
    
    plt.subplot(2, 1, 2)
    plt.semilogy(f, Pxx, 'b-', label='Custom Welch')
    plt.semilogy(f_scipy, Pxx_scipy, 'r--', label='Scipy Welch')
    plt.title('Power Spectral Density')
    plt.xlabel('Frequency [Hz]')
    plt.ylabel('Power Spectral Density')
    plt.legend()
    plt.grid(True)
    
    plt.tight_layout()
    plt.show()
    
    return f, Pxx

# Run the example
if __name__ == "__main__":
    frequencies, psd = example_welch()
    print(f"Frequency range: {frequencies[0]:.1f} to {frequencies[-1]:.1f} Hz")
    print(f"PSD shape: {psd.shape}")
```

## Key Features of This Implementation:

1. **Segmentation**: Divides the signal into overlapping segments
2. **Windowing**: Applies a window function (default Hann) to each segment
3. **FFT**: Computes Fast Fourier Transform of each segment
4. **Averaging**: Averages the modified periodograms
5. **Flexible Parameters**: Supports various window types and scaling options

## Parameters Explained:

- **nperseg**: Length of each segment (higher = better frequency resolution)
- **noverlap**: Number of overlapping points between segments (higher = better time resolution)
- **window**: Type of window function to apply
- **scaling**: Whether to return power spectral density or power spectrum

## Advantages of Welch's Method:

- Reduces noise in PSD estimates
- Provides better statistical properties than single periodogram
- Handles non-stationary signals better
- Computationally efficient

This implementation demonstrates the core principles of Welch's method while providing a practical tool for spectral analysis.

