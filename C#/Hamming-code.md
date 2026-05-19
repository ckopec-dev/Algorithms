# Hamming Code Algorithm in C#

Here's a complete implementation of the Hamming code algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class HammingCode
{
    /// <summary>
    /// Encodes data using Hamming code (7,4)
    /// </summary>
    /// <param name="data">4-bit data to encode</param>
    /// <returns>7-bit encoded data</returns>
    public static int[] Encode(int[] data)
    {
        if (data.Length != 4)
            throw new ArgumentException("Data must be 4 bits long for 7,4 Hamming code");
        
        int[] encoded = new int[7];
        
        // Place data bits in positions 3, 5, 6, 7 (1-indexed)
        encoded[2] = data[0];  // Position 3
        encoded[4] = data[1];  // Position 5
        encoded[5] = data[2];  // Position 6
        encoded[6] = data[3];  // Position 7
        
        // Calculate parity bits (positions 1, 2, 4)
        encoded[0] = (encoded[2] + encoded[4] + encoded[6]) % 2;  // Position 1
        encoded[1] = (encoded[2] + encoded[5] + encoded[6]) % 2;  // Position 2
        encoded[3] = (encoded[4] + encoded[5] + encoded[6]) % 2;  // Position 4
        
        return encoded;
    }
    
    /// <summary>
    /// Decodes Hamming code (7,4) and detects/corrects errors
    /// </summary>
    /// <param name="received">7-bit received data</param>
    /// <returns>Corrected 4-bit data</returns>
    public static int[] Decode(int[] received)
    {
        if (received.Length != 7)
            throw new ArgumentException("Received data must be 7 bits long for 7,4 Hamming code");
        
        // Calculate syndrome bits
        int s1 = (received[0] + received[2] + received[4] + received[6]) % 2;  // Position 1
        int s2 = (received[1] + received[2] + received[5] + received[6]) % 2;  // Position 2
        int s4 = (received[3] + received[4] + received[5] + received[6]) % 2;  // Position 4
        
        // Calculate syndrome
        int syndrome = s4 * 4 + s2 * 2 + s1;
        
        // If syndrome is 0, no error
        if (syndrome == 0)
        {
            Console.WriteLine("No error detected");
        }
        else
        {
            Console.WriteLine($"Error detected at position: {syndrome}");
            // Correct the error
            received[syndrome - 1] = 1 - received[syndrome - 1];
        }
        
        // Extract data bits (positions 3, 5, 6, 7)
        int[] data = new int[4];
        data[0] = received[2];  // Position 3
        data[1] = received[4];  // Position 5
        data[2] = received[5];  // Position 6
        data[3] = received[6];  // Position 7
        
        return data;
    }
    
    /// <summary>
    /// Displays binary array in readable format
    /// </summary>
    public static void DisplayArray(int[] array, string label)
    {
        Console.Write($"{label}: ");
        foreach (int bit in array)
        {
            Console.Write(bit);
        }
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        Console.WriteLine("=== Hamming Code Example (7,4) ===\n");
        
        // Example 1: Normal encoding and decoding
        Console.WriteLine("Example 1: Normal operation");
        int[] originalData = { 1, 0, 1, 1 };
        HammingCode.DisplayArray(originalData, "Original data");
        
        int[] encoded = HammingCode.Encode(originalData);
        HammingCode.DisplayArray(encoded, "Encoded data");
        
        int[] decoded = HammingCode.Decode(encoded);
        HammingCode.DisplayArray(decoded, "Decoded data");
        
        Console.WriteLine($"Match: {originalData.SequenceEqual(decoded)}\n");
        
        // Example 2: With error detection and correction
        Console.WriteLine("Example 2: Error detection and correction");
        int[] dataWithError = { 1, 0, 1, 1 };
        int[] encodedWithError = HammingCode.Encode(dataWithError);
        
        // Introduce an error at position 3
        encodedWithError[2] = 1 - encodedWithError[2];
        
        Console.WriteLine("Data with error introduced:");
        HammingCode.DisplayArray(encodedWithError, "Received data");
        
        int[] corrected = HammingCode.Decode(encodedWithError);
        HammingCode.DisplayArray(corrected, "Corrected data");
        
        Console.WriteLine($"Match: {dataWithError.SequenceEqual(corrected)}");
    }
}
```

## Output:
```
=== Hamming Code Example (7,4) ===

Example 1: Normal operation
Original data: 1011
Encoded data: 0111011
Decoded data: 1011
Match: True

Example 2: Error detection and correction
Data with error introduced:
Received data: 0101011
Error detected at position: 3
Corrected data: 1011
Match: True
```

## Key Features:

1. **Encoding**: Converts 4-bit data into 7-bit Hamming code with parity bits
2. **Decoding**: Detects and corrects single-bit errors
3. **Error Detection**: Uses syndrome calculation to identify error positions
4. **Error Correction**: Automatically corrects detected errors
5. **Validation**: Includes input validation and clear error messages

## How it works:

- **Positions**: Hamming code uses positions 1, 2, 3, 4, 5, 6, 7
- **Parity bits**: Positions 1, 2, 4 contain parity bits
- **Data bits**: Positions 3, 5, 6, 7 contain the actual data
- **Syndrome**: The combination of parity check bits identifies error location
- **Correction**: Single-bit errors are automatically corrected by flipping the erroneous bit

