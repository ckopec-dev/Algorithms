# Wavelet Transform Algorithm in C#

Here's a complete implementation of a Discrete Wavelet Transform (DWT) algorithm in C#:

```csharp
using System;
using System.Linq;

public class WaveletTransform
{
    /// <summary>
    /// Performs a 1D Discrete Wavelet Transform using Haar wavelet
    /// </summary>
    /// <param name="input">Input signal array</param>
    /// <returns>Transformed coefficients</returns>
    public static double[] DWT_Haar(double[] input)
    {
        if (input == null || input.Length == 0)
            throw new ArgumentException("Input array cannot be null or empty");

        int n = input.Length;
        int level = (int)Math.Floor(Math.Log2(n));
        int length = (int)Math.Pow(2, level);

        // Ensure input length is a power of 2
        double[] signal = new double[length];
        Array.Copy(input, 0, signal, 0, Math.Min(n, length));

        double[] output = new double[length];
        Array.Copy(signal, output, length);

        // Perform multi-level DWT
        for (int i = 0; i < level; i++)
        {
            int step = (int)Math.Pow(2, i + 1);
            int halfLength = length / step;

            for (int j = 0; j < halfLength; j++)
            {
                int index = j * step;
                double average = (output[index] + output[index + step / 2]) / 2.0;
                double difference = (output[index] - output[index + step / 2]) / 2.0;
                
                output[index] = average;
                output[index + step / 2] = difference;
            }
        }

        return output;
    }

    /// <summary>
    /// Performs inverse 1D Discrete Wavelet Transform using Haar wavelet
    /// </summary>
    /// <param name="coefficients">Wavelet coefficients</param>
    /// <returns>Reconstructed signal</returns>
    public static double[] IDWT_Haar(double[] coefficients)
    {
        if (coefficients == null || coefficients.Length == 0)
            throw new ArgumentException("Coefficients array cannot be null or empty");

        int n = coefficients.Length;
        int level = (int)Math.Floor(Math.Log2(n));
        int length = (int)Math.Pow(2, level);

        double[] signal = new double[length];
        Array.Copy(coefficients, 0, signal, 0, Math.Min(n, length));

        // Perform inverse multi-level DWT
        for (int i = level - 1; i >= 0; i--)
        {
            int step = (int)Math.Pow(2, i + 1);
            int halfLength = length / step;

            for (int j = 0; j < halfLength; j++)
            {
                int index = j * step;
                double average = signal[index];
                double difference = signal[index + step / 2];
                
                signal[index] = average + difference;
                signal[index + step / 2] = average - difference;
            }
        }

        return signal;
    }

    /// <summary>
    /// Performs 2D Discrete Wavelet Transform using Haar wavelet
    /// </summary>
    /// <param name="input">2D input matrix</param>
    /// <returns>Transformed 2D coefficients</returns>
    public static double[,] DWT_2D(double[,] input)
    {
        int rows = input.GetLength(0);
        int cols = input.GetLength(1);

        // Ensure dimensions are powers of 2
        int newRows = (int)Math.Pow(2, Math.Ceiling(Math.Log2(rows)));
        int newCols = (int)Math.Pow(2, Math.Ceiling(Math.Log2(cols)));

        double[,] matrix = new double[newRows, newCols];
        
        // Copy input to new matrix
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                matrix[i, j] = input[i, j];
            }
        }

        // Apply 1D DWT to rows
        for (int i = 0; i < newRows; i++)
        {
            double[] row = new double[newCols];
            for (int j = 0; j < newCols; j++)
            {
                row[j] = matrix[i, j];
            }
            double[] transformedRow = DWT_Haar(row);
            for (int j = 0; j < newCols; j++)
            {
                matrix[i, j] = transformedRow[j];
            }
        }

        // Apply 1D DWT to columns
        for (int j = 0; j < newCols; j++)
        {
            double[] col = new double[newRows];
            for (int i = 0; i < newRows; i++)
            {
                col[i] = matrix[i, j];
            }
            double[] transformedCol = DWT_Haar(col);
            for (int i = 0; i < newRows; i++)
            {
                matrix[i, j] = transformedCol[i];
            }
        }

        return matrix;
    }

    /// <summary>
    /// Demonstrates the usage of wavelet transform
    /// </summary>
    public static void DemonstrateWaveletTransform()
    {
        Console.WriteLine("=== Wavelet Transform Demo ===\n");

        // Example 1: 1D DWT
        double[] signal = { 1, 2, 3, 4, 5, 6, 7, 8 };
        Console.WriteLine("Original signal: [" + string.Join(", ", signal) + "]");

        double[] dwtResult = DWT_Haar(signal);
        Console.WriteLine("DWT coefficients: [" + string.Join(", ", dwtResult.Select(x => Math.Round(x, 3))) + "]");

        double[] reconstructed = IDWT_Haar(dwtResult);
        Console.WriteLine("Reconstructed signal: [" + string.Join(", ", reconstructed.Select(x => Math.Round(x, 3))) + "]");

        Console.WriteLine("\nReconstruction error: " + 
            string.Join(", ", signal.Zip(reconstructed, (a, b) => Math.Abs(a - b)).Select(x => Math.Round(x, 6))));

        Console.WriteLine("\n=== 2D Wavelet Transform Demo ===");

        // Example 2: 2D DWT
        double[,] image = {
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12},
            {13, 14, 15, 16}
        };

        Console.WriteLine("Original 2D signal:");
        PrintMatrix(image);

        double[,] dwt2D = DWT_2D(image);
        Console.WriteLine("\n2D DWT coefficients:");
        PrintMatrix(dwt2D);
    }

    /// <summary>
    /// Helper method to print 2D matrix
    /// </summary>
    private static void PrintMatrix(double[,] matrix)
    {
        int rows = matrix.GetLength(0);
        int cols = matrix.GetLength(1);

        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                Console.Write($"{matrix[i, j]:F2} ");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
class Program
{
    static void Main(string[] args)
    {
        WaveletTransform.DemonstrateWaveletTransform();
        
        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}
```

## Key Features of this Implementation:

### 1. **Haar Wavelet Transform**
- Uses the simplest wavelet basis
- Efficient implementation using the lifting scheme
- Supports both 1D and 2D transforms

### 2. **Core Functions**
- `DWT_Haar()`: Forward 1D discrete wavelet transform
- `IDWT_Haar()`: Inverse 1D discrete wavelet transform
- `DWT_2D()`: 2D discrete wavelet transform

### 3. **Key Characteristics**
- **Input validation**: Checks for null and empty arrays
- **Power-of-2 handling**: Automatically adjusts input sizes
- **Multi-level decomposition**: Supports multiple decomposition levels
- **Perfect reconstruction**: IDWT can perfectly reconstruct original signal

### 4. **Usage Example**
The code demonstrates both 1D and 2D wavelet transforms with:
- Signal reconstruction verification
- Error calculation
- Matrix printing utilities

This implementation provides a solid foundation that can be extended with other wavelet families (Daubechies, Coiflet, etc.) and more advanced features like wavelet packet transforms or thresholding.

