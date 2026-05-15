# Fast Fourier Transform (FFT) in Python

Here's an example of implementing FFT using Python with the NumPy library:

```python
import numpy as np
import matplotlib.pyplot as plt

# Generate a sample signal
# Create a signal with two sine waves at different frequencies
t = np.linspace(0, 1, 1000)  # Time vector
f1 = 5   # First frequency (Hz)
f2 = 12  # Second frequency (Hz)
signal = 0.5 * np.sin(2 * np.pi * f1 * t) + 0.3 * np.sin(2 * np.pi * f2 * t)

# Apply FFT
fft_result = np.fft.fft(signal)
fft_freq = np.fft.fftfreq(len(t), t[1] - t[0])

# Calculate the magnitude of the FFT
magnitude = np.abs(fft_result)

# Plot the results
plt.figure(figsize=(12, 8))

# Plot original signal
plt.subplot(2, 2, 1)
plt.plot(t, signal)
plt.title('Original Signal')
plt.xlabel('Time (s)')
plt.ylabel('Amplitude')

# Plot FFT magnitude
plt.subplot(2, 2, 2)
plt.plot(fft_freq[:len(fft_freq)//2], magnitude[:len(fft_freq)//2])
plt.title('FFT Magnitude')
plt.xlabel('Frequency (Hz)')
plt.ylabel('Magnitude')

# Plot FFT magnitude (zoomed in)
plt.subplot(2, 2, 3)
plt.plot(fft_freq[:len(fft_freq)//4], magnitude[:len(fft_freq)//4])
plt.title('FFT Magnitude (Zoomed)')
plt.xlabel('Frequency (Hz)')
plt.ylabel('Magnitude')

# Show the frequency components
plt.subplot(2, 2, 4)
plt.stem(fft_freq[:len(fft_freq)//2], magnitude[:len(fft_freq)//2])
plt.title('FFT Stem Plot')
plt.xlabel('Frequency (Hz)')
plt.ylabel('Magnitude')

plt.tight_layout()
plt.show()

# Print some key information
print(f"Signal length: {len(signal)}")
print(f"Sampling frequency: {1/(t[1]-t[0]):.2f} Hz")
print(f"Frequency resolution: {fft_freq[1]:.2f} Hz")
print(f"Peak frequencies found at: {fft_freq[np.argmax(magnitude[1:len(fft_freq)//2])]:.2f} Hz")
```

## Alternative Implementation using scipy

```python
from scipy import fftpack
import numpy as np

# Create a simple signal
x = np.linspace(0, 4*np.pi, 100)
signal = np.sin(x) + 0.5 * np.sin(3*x) + 0.3 * np.sin(7*x)

# Using scipy's FFT
fft_result = fftpack.fft(signal)
frequencies = fftpack.fftfreq(len(signal), d=x[1]-x[0])

# Plot the results
plt.figure(figsize=(10, 6))
plt.plot(frequencies[:len(frequencies)//2], 
         np.abs(fft_result[:len(fft_result)//2]))
plt.title('FFT using SciPy')
plt.xlabel('Frequency')
plt.ylabel('Magnitude')
plt.grid(True)
plt.show()
```

## Key Points:

- **FFT** transforms a time-domain signal into its frequency-domain representation
- **np.fft.fft()** is the main function for computing FFT in NumPy
- The result contains complex numbers; use **np.abs()** to get magnitude
- **np.fft.fftfreq()** generates the corresponding frequency vector
- FFT is much faster than direct DFT computation, with O(n log n) complexity instead of O(n²)

