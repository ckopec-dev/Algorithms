# Welch's Method Algorithm in C#

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's an implementation in C#:

```csharp
using System;
using System.Linq;
using System.Numerics;

public class WelchMethod
{
    /// <summary>
    /// Estimates power spectral density using Welch's method
    /// </summary>
    /// <param name="signal">Input signal array</param>
    /// <param name="windowSize">Size of each segment</param>
    /// <param name="overlap">Overlap between segments (0-1)</param>
    /// <param name="windowType">Type of window function to apply</param>
    /// <returns>Array of PSD estimates</returns>
    public static double[] EstimatePSD(double[] signal, int windowSize = 256, 
        double overlap = 0.5, WindowType windowType = WindowType.Hanning)
    {
        if (signal == null || signal.Length == 0)
            throw new ArgumentException("Signal cannot be null or empty");

        if (windowSize > signal.Length)
            throw new ArgumentException("Window size cannot be larger than signal length");

        // Calculate number of overlapping segments
        int stepSize = (int)(windowSize * (1 - overlap));
        int numSegments = (signal.Length - windowSize) / stepSize + 1;

        // Initialize PSD array
        double[] psd = new double[windowSize / 2];
        
        // Create window function
        double[] window = CreateWindow(windowSize, windowType);

        // Process each segment
        for (int i = 0; i < numSegments; i++)
        {
            int startIndex = i * stepSize;
            
            // Check if we have enough data for this segment
            if (startIndex + windowSize > signal.Length)
                break;

            // Extract segment
            double[] segment = new double[windowSize];
            Array.Copy(signal, startIndex, segment, 0, windowSize);

            // Apply window function
            for (int j = 0; j < windowSize; j++)
            {
                segment[j] *= window[j];
            }

            // Compute FFT
            Complex[] fftResult = FFT(segment);

            // Compute power spectrum (magnitude squared)
            for (int j = 0; j < windowSize / 2; j++)
            {
                double magnitude = Math.Abs(fftResult[j]);
                psd[j] += magnitude * magnitude;
            }
        }

        // Average the results and normalize
        double normalizationFactor = numSegments * GetWindowPower(window);
        for (int i = 0; i < psd.Length; i++)
        {
            psd[i] /= normalizationFactor;
        }

        return psd;
    }

    /// <summary>
    /// Creates a window function of specified size and type
    /// </summary>
    private static double[] CreateWindow(int size, WindowType windowType)
    {
        double[] window = new double[size];

        switch (windowType)
        {
            case WindowType.Rectangular:
                for (int i = 0; i < size; i++)
                    window[i] = 1.0;
                break;

            case WindowType.Hanning:
                for (int i = 0; i < size; i++)
                    window[i] = 0.5 * (1 - Math.Cos(2 * Math.PI * i / (size - 1)));
                break;

            case WindowType.Hamming:
                const double alpha = 0.54;
                const double beta = 0.46;
                for (int i = 0; i < size; i++)
                    window[i] = alpha - beta * Math.Cos(2 * Math.PI * i / (size - 1));
                break;

            case WindowType.Blackman:
                const double a0 = 0.42;
                const double a1 = 0.5;
                const double a2 = 0.08;
                for (int i = 0; i < size; i++)
                    window[i] = a0 - a1 * Math.Cos(2 * Math.PI * i / (size - 1)) 
                              + a2 * Math.Cos(4 * Math.PI * i / (size - 1));
                break;
        }

        return window;
    }

    /// <summary>
    /// Calculates the total power of a window function
    /// </summary>
    private static double GetWindowPower(double[] window)
    {
        return window.Sum(x => x * x);
    }

    /// <summary>
    /// Fast Fourier Transform implementation (Cooley-Tukey algorithm)
    /// </summary>
    private static Complex[] FFT(double[] input)
    {
        int n = input.Length;
        if (n <= 1) return input.Select(x => new Complex(x, 0)).ToArray();

        // Bit-reversal permutation
        Complex[] fft = new Complex[n];
        for (int i = 0; i < n; i++)
        {
            fft[i] = new Complex(input[i], 0);
        }

        // Cooley-Tukey FFT
        int m = 1;
        while (m < n)
        {
            m *= 2;
        }
        
        int k = 0;
        for (int i = 0; i < n - 1; i++)
        {
            if (i < k)
            {
                Complex temp = fft[k];
                fft[k] = fft[i];
                fft[i] = temp;
            }
            
            int j = n / 2;
            while (j <= k)
            {
                k -= j;
                j /= 2;
            }
            k += j;
        }

        // FFT computation
        for (int i = 1; i < n; i *= 2)
        {
            double angle = -Math.PI / i;
            Complex w = new Complex(Math.Cos(angle), Math.Sin(angle));
            
            for (int j = 0; j < i; j++)
            {
                Complex wj = new Complex(1, 0);
                for (int k = j; k < n; k += 2 * i)
                {
                    Complex u = fft[k];
                    Complex v = fft[k + i] * wj;
                    fft[k] = u + v;
                    fft[k + i] = u - v;
                    wj *= w;
                }
            }
        }

        return fft;
    }
}

/// <summary>
/// Window function types for spectral analysis
/// </summary>
public enum WindowType
{
    Rectangular,
    Hanning,
    Hamming,
    Blackman
}

// Example usage
class Program
{
    static void Main()
    {
        // Generate test signal (sum of sine waves)
        int sampleRate = 100;
        int duration = 5; // seconds
        int numSamples = sampleRate * duration;
        
        double[] signal = new double[numSamples];
        for (int i = 0; i < numSamples; i++)
        {
            double t = i / (double)sampleRate;
            // Signal with two frequency components
            signal[i] = Math.Sin(2 * Math.PI * 10 * t) + 0.5 * Math.Sin(2 * Math.PI * 25 * t);
        }

        // Apply Welch's method
        double[] psd = WelchMethod.EstimatePSD(signal, windowSize: 128, overlap: 0.75);

        // Display results
        Console.WriteLine("Power Spectral Density Estimates:");
        Console.WriteLine("Frequency (Hz)\tPSD Value");
        Console.WriteLine("------------------------");
        
        for (int i = 0; i < Math.Min(32, psd.Length); i++)
        {
            double frequency = i * sampleRate / (double)(psd.Length * 2);
            Console.WriteLine($"{frequency:F1}\t\t{psd[i]:E2}");
        }
    }
}
```

## Key Features of this Implementation:

1. **Segmentation**: Divides the input signal into overlapping segments
2. **Windowing**: Applies window functions to reduce spectral leakage
3. **FFT Processing**: Computes Fast Fourier Transform for each segment
4. **Averaging**: Averages the power spectra from all segments
5. **Normalization**: Properly normalizes the final PSD estimate

## Parameters:
- `windowSize`: Size of each signal segment (typically 128, 256, or 512)
- `overlap`: Fraction of overlap between consecutive segments (0 to 1)
- `windowType`: Type of window function to apply

## Usage:
The method returns an array of PSD estimates corresponding to frequency bins. The actual frequencies depend on the sampling rate and window size used in the analysis.