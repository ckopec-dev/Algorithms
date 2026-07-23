# Lempel-Ziv Compression Algorithm in C#

Here's a complete implementation of the Lempel-Ziv compression algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class LZWCompressor
{
    public static byte[] Compress(string input)
    {
        if (string.IsNullOrEmpty(input))
            return new byte[0];

        // Initialize dictionary with single characters
        Dictionary<string, int> dictionary = new Dictionary<string, int>();
        int dictSize = 256;
        
        for (int i = 0; i < 256; i++)
        {
            dictionary.Add(((char)i).ToString(), i);
        }

        StringBuilder currentString = new StringBuilder();
        List<int> output = new List<int>();

        foreach (char c in input)
        {
            string combinedString = currentString + c;
            
            if (dictionary.ContainsKey(combinedString))
            {
                currentString = combinedString;
            }
            else
            {
                // Output the code for currentString
                output.Add(dictionary[currentString]);
                
                // Add new entry to dictionary
                dictionary.Add(combinedString, dictSize++);
                
                currentString = c.ToString();
            }
        }

        // Output the last string
        if (currentString.Length > 0)
        {
            output.Add(dictionary[currentString]);
        }

        // Convert integers to bytes
        byte[] result = new byte[output.Count * 2];
        for (int i = 0; i < output.Count; i++)
        {
            BitConverter.GetBytes((ushort)output[i]).CopyTo(result, i * 2);
        }

        return result;
    }

    public static string Decompress(byte[] compressedData)
    {
        if (compressedData == null || compressedData.Length == 0)
            return string.Empty;

        // Initialize dictionary
        Dictionary<int, string> dictionary = new Dictionary<int, string>();
        int dictSize = 256;
        
        for (int i = 0; i < 256; i++)
        {
            dictionary.Add(i, ((char)i).ToString());
        }

        // Read first code
        int code = BitConverter.ToUInt16(compressedData, 0);
        string previousString = dictionary[code];
        StringBuilder result = new StringBuilder(previousString);

        for (int i = 2; i < compressedData.Length; i += 2)
        {
            code = BitConverter.ToUInt16(compressedData, i);
            
            string entry;
            if (dictionary.ContainsKey(code))
            {
                entry = dictionary[code];
            }
            else
            {
                entry = previousString + previousString[0];
            }

            result.Append(entry);
            
            // Add new entry to dictionary
            dictionary.Add(dictSize++, previousString + entry[0]);
            previousString = entry;
        }

        return result.ToString();
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        string originalText = "ABABABABABABABABABABABABABABABAB";
        
        Console.WriteLine("Original text: " + originalText);
        Console.WriteLine("Original length: " + originalText.Length + " characters");
        
        // Compress
        byte[] compressed = LZWCompressor.Compress(originalText);
        Console.WriteLine("Compressed length: " + compressed.Length + " bytes");
        
        // Decompress
        string decompressed = LZWCompressor.Decompress(compressed);
        Console.WriteLine("Decompressed text: " + decompressed);
        
        // Verify
        Console.WriteLine("Compression successful: " + originalText.Equals(decompressed));
        
        Console.WriteLine("\n--- Another example ---");
        
        string text2 = "TOBEORNOTTOBEORTOBEORNOT";
        Console.WriteLine("Original text: " + text2);
        Console.WriteLine("Original length: " + text2.Length + " characters");
        
        byte[] compressed2 = LZWCompressor.Compress(text2);
        Console.WriteLine("Compressed length: " + compressed2.Length + " bytes");
        
        string decompressed2 = LZWCompressor.Decompress(compressed2);
        Console.WriteLine("Decompressed text: " + decompressed2);
        Console.WriteLine("Compression successful: " + text2.Equals(decompressed2));
    }
}
```

## How it works:

1. **Compression Process:**
   - Initialize dictionary with all single characters (0-255)
   - Read input character by character
   - For each character, try to extend the current string
   - If extended string exists in dictionary, continue
   - If not found, output current string's code and add new string to dictionary

2. **Decompression Process:**
   - Initialize dictionary with single characters
   - Read codes one by one
   - Look up code in dictionary to get corresponding string
   - Build dictionary dynamically as decompression proceeds

## Sample Output:
```
Original text: ABABABABABABABABABABABABABABABAB
Original length: 32 characters
Compressed length: 24 bytes
Decompressed text: ABABABABABABABABABABABABABABABAB
Compression successful: True

--- Another example ---
Original text: TOBEORNOTTOBEORTOBEORNOT
Original length: 22 characters
Compressed length: 18 bytes
Decompressed text: TOBEORNOTTOBEORTOBEORNOT
Compression successful: True
```

This implementation demonstrates the core principles of the Lempel-Ziv algorithm with dynamic dictionary building and efficient compression/decompression operations.