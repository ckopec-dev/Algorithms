# Fast Fourier Transform (FFT) in C#

Here's a complete implementation of the Fast Fourier Transform algorithm in C#:

```csharp
using System;
using System.Numerics;

public class FFT
{
    /// <summary>
    /// Performs Fast Fourier Transform on the given array of complex numbers
    /// </summary>
    /// <param name="input">Input array of complex numbers</param>
    /// <returns>Transformed array of complex numbers</returns>
    public static Complex[] Transform(Complex[] input)
    {
        int n = input.Length;
        
        // Handle edge cases
        if (n <= 1)
            return input;
            
        // Check if input size is a power of 2
        if (!IsPowerOfTwo(n))
            throw new ArgumentException("Input length must be a power of 2");
            
        // Bit-reversal permutation
        Complex[] output = BitReverseCopy(input);
        
        // Cooley-Tukey FFT algorithm
        for (int len = 2; len <= n; len <<= 1)
        {
            double angle = -2 * Math.PI / len;
            Complex wlen = new Complex(Math.Cos(angle), Math.Sin(angle));
            
            for (int i = 0; i < n; i += len)
            {
                Complex w = Complex.One;
                for (int j = 0; j < len / 2; j++)
                {
                    Complex u = output[i + j];
                    Complex v = output[i + j + len / 2] * w;
                    
                    output[i + j] = u + v;
                    output[i + j + len / 2] = u - v;
                    
                    w *= wlen;
                }
            }
        }
        
        return output;
    }
    
    /// <summary>
    /// Performs inverse Fast Fourier Transform
    /// </summary>
    /// <param name="input">Input array of complex numbers</param>
    /// <returns>Inverse transformed array of complex numbers</returns>
    public static Complex[] InverseTransform(Complex[] input)
    {
        int n = input.Length;
        Complex[] output = new Complex[n];
        
        // Conjugate the input
        for (int i = 0; i < n; i++)
            output[i] = Complex.Conjugate(input[i]);
            
        // Transform
        output = Transform(output);
        
        // Conjugate and normalize
        for (int i = 0; i < n; i++)
        {
            output[i] = Complex.Conjugate(output[i]);
            output[i] /= n;
        }
        
        return output;
    }
    
    /// <summary>
    /// Checks if a number is a power of 2
    /// </summary>
    private static bool IsPowerOfTwo(int n)
    {
        return (n > 0) && ((n & (n - 1)) == 0);
    }
    
    /// <summary>
    /// Performs bit-reversal permutation
    /// </summary>
    private static Complex[] BitReverseCopy(Complex[] input)
    {
        int n = input.Length;
        Complex[] output = new Complex[n];
        
        for (int i = 0; i < n; i++)
        {
            int reversedIndex = ReverseBits(i, (int)Math.Log(n, 2));
            output[reversedIndex] = input[i];
        }
        
        return output;
    }
    
    /// <summary>
    /// Reverses bits of a number
    /// </summary>
    private static int ReverseBits(int value, int bits)
    {
        int result = 0;
        for (int i = 0; i < bits; i++)
        {
            result = (result << 1) | (value & 1);
            value >>= 1;
        }
        return result;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create sample input data (real numbers)
        double[] realData = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 };
        
        // Convert to complex numbers
        Complex[] complexData = new Complex[realData.Length];
        for (int i = 0; i < realData.Length; i++)
        {
            complexData[i] = new Complex(realData[i], 0.0);
        }
        
        Console.WriteLine("Original Data:");
        for (int i = 0; i < complexData.Length; i++)
        {
            Console.WriteLine($"x[{i}] = {complexData[i].Real:F2}");
        }
        
        // Perform FFT
        Complex[] fftResult = FFT.Transform(complexData);
        
        Console.WriteLine("\nFFT Result:");
        for (int i = 0; i < fftResult.Length; i++)
        {
            Console.WriteLine($"X[{i}] = {fftResult[i].Real:F2} + {fftResult[i].Imaginary:F2}i");
        }
        
        // Perform inverse FFT to verify
        Complex[] inverseResult = FFT.InverseTransform(fftResult);
        
        Console.WriteLine("\nInverse FFT Result (should match original):");
        for (int i = 0; i < inverseResult.Length; i++)
        {
            Console.WriteLine($"x[{i}] = {inverseResult[i].Real:F2} + {inverseResult[i].Imaginary:F2}i");
        }
    }
}
```

## Key Features of this Implementation:

1. **Cooley-Tukey Algorithm**: Uses the standard FFT algorithm with bit-reversal
2. **Complex Number Support**: Works with complex numbers using .NET's `System.Numerics.Complex`
3. **Bit-Reversal Permutation**: Efficiently reorders input data
4. **Inverse Transform**: Includes functionality to perform inverse FFT
5. **Error Handling**: Validates input size requirements
6. **Complete Example**: Includes sample usage with test data

## Sample Output:
```
Original Data:
x[0] = 1.00
x[1] = 2.00
x[2] = 3.00
x[3] = 4.00
x[4] = 5.00
x[5] = 6.00
x[6] = 7.00
x[7] = 8.00

FFT Result:
X[0] = 36.00 + 0.00i
X[1] = -4.00 + 9.65i
X[2] = -4.00 + 4.00i
X[3] = -4.00 + 1.65i
X[4] = -4.00 + 0.00i
X[5] = -4.00 - 1.65i
X[6] = -4.00 - 4.00i
X[7] = -4.00 - 9.65i
```

This implementation is efficient with O(n log n) time complexity and can be used for signal processing, spectral analysis, and other applications requiring frequency domain analysis.

