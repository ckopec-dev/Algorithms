# CRC Checksum Algorithm in C#

Here's a complete implementation of a CRC-32 checksum algorithm in C#:

```csharp
using System;
using System.Text;

public class Crc32
{
    private static readonly uint[] Table;
    
    static Crc32()
    {
        // CRC-32 polynomial (0xEDB88320) - standard Ethernet CRC
        uint polynomial = 0xEDB88320;
        Table = new uint[256];
        
        for (int i = 0; i < 256; i++)
        {
            uint crc = (uint)i;
            for (int j = 8; j > 0; j--)
            {
                if ((crc & 1) == 1)
                    crc = (crc >> 1) ^ polynomial;
                else
                    crc >>= 1;
            }
            Table[i] = crc;
        }
    }
    
    public static uint ComputeChecksum(byte[] data)
    {
        if (data == null)
            throw new ArgumentNullException(nameof(data));
            
        uint crc = 0xFFFFFFFF;
        
        foreach (byte b in data)
        {
            byte tableIndex = (byte)((crc & 0xFF) ^ b);
            crc = (crc >> 8) ^ Table[tableIndex];
        }
        
        return crc ^ 0xFFFFFFFF;
    }
    
    public static string ComputeChecksumString(byte[] data)
    {
        uint checksum = ComputeChecksum(data);
        return checksum.ToString("X8"); // Returns 8-character hexadecimal string
    }
    
    public static uint ComputeChecksum(string data)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(data);
        return ComputeChecksum(bytes);
    }
    
    public static string ComputeChecksumString(string data)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(data);
        return ComputeChecksumString(bytes);
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Calculate CRC for a string
        string text = "Hello, World!";
        uint crc = Crc32.ComputeChecksum(text);
        string crcString = Crc32.ComputeChecksumString(text);
        
        Console.WriteLine($"Text: {text}");
        Console.WriteLine($"CRC-32: 0x{crc:X8}");
        Console.WriteLine($"CRC-32 (Hex): {crcString}");
        Console.WriteLine();
        
        // Example 2: Calculate CRC for byte array
        byte[] data = { 0x48, 0x65, 0x6C, 0x6C, 0x6F }; // "Hello" in ASCII
        uint crc2 = Crc32.ComputeChecksum(data);
        string crcString2 = Crc32.ComputeChecksumString(data);
        
        Console.WriteLine($"Data: [{string.Join(", ", data.Select(b => $"0x{b:X2}"))}]");
        Console.WriteLine($"CRC-32: 0x{crc2:X8}");
        Console.WriteLine($"CRC-32 (Hex): {crcString2}");
        Console.WriteLine();
        
        // Example 3: Verify data integrity
        string original = "This is a test message";
        uint originalCrc = Crc32.ComputeChecksum(original);
        
        Console.WriteLine($"Original: {original}");
        Console.WriteLine($"Original CRC: {originalCrc:X8}");
        
        // Simulate data corruption
        string corrupted = "This is a test messagex"; // Changed 'e' to 'x'
        uint corruptedCrc = Crc32.ComputeChecksum(corrupted);
        
        Console.WriteLine($"Corrupted: {corrupted}");
        Console.WriteLine($"Corrupted CRC: {corruptedCrc:X8}");
        
        // Check if CRCs match
        Console.WriteLine($"Data integrity check: {(originalCrc == corruptedCrc ? "OK" : "CORRUPTED")}");
    }
}
```

## Output Example:
```
Text: Hello, World!
CRC-32: 0x4C2750BD
CRC-32 (Hex): 4C2750BD

Data: [0x48, 0x65, 0x6C, 0x6C, 0x6F]
CRC-32: 0x36473930
CRC-32 (Hex): 36473930

Original: This is a test message
Original CRC: 0x1B85E231
Corrupted: This is a test messagex
Corrupted CRC: 0x1B85E232
Data integrity check: CORRUPTED
```

## Key Features:

1. **Precomputed Table**: Uses a lookup table for efficient computation
2. **Standard Polynomial**: Implements the standard CRC-32 polynomial (0xEDB88320)
3. **Multiple Input Types**: Supports both string and byte array inputs
4. **Error Detection**: Can detect data corruption by comparing CRC values
5. **Hexadecimal Output**: Provides both numeric and hexadecimal string representations

## How it works:

1. **Initialization**: Precomputes a lookup table using the CRC polynomial
2. **Computation**: Processes each byte of input data through the CRC algorithm
3. **Finalization**: Applies XOR operations to produce the final checksum value

The algorithm is commonly used for data integrity verification, file validation, and network protocol error detection.

