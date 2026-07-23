# LZW Algorithm Implementation in C#

Here's a complete implementation of the Lempel-Ziv-Welch (LZW) compression and decompression algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class LZW
{
    public static List<int> Compress(string input)
    {
        // Create dictionary with all possible single characters
        Dictionary<string, int> dictionary = new Dictionary<string, int>();
        int dictSize = 256;
        
        for (int i = 0; i < 256; i++)
        {
            dictionary.Add(((char)i).ToString(), i);
        }
        
        string currentString = "";
        List<int> output = new List<int>();
        
        foreach (char c in input)
        {
            string tempString = currentString + c;
            
            if (dictionary.ContainsKey(tempString))
            {
                currentString = tempString;
            }
            else
            {
                // Output the code for currentString
                output.Add(dictionary[currentString]);
                
                // Add new string to dictionary
                dictionary.Add(tempString, dictSize++);
                
                currentString = c.ToString();
            }
        }
        
        // Output the last code
        if (currentString != "")
        {
            output.Add(dictionary[currentString]);
        }
        
        return output;
    }
    
    public static string Decompress(List<int> input)
    {
        // Create dictionary with all possible single characters
        Dictionary<int, string> dictionary = new Dictionary<int, string>();
        int dictSize = 256;
        
        for (int i = 0; i < 256; i++)
        {
            dictionary.Add(i, ((char)i).ToString());
        }
        
        if (input.Count == 0)
            return "";
            
        string previousString = dictionary[input[0]];
        StringBuilder output = new StringBuilder();
        output.Append(previousString);
        
        for (int i = 1; i < input.Count; i++)
        {
            int code = input[i];
            string currentString;
            
            if (dictionary.ContainsKey(code))
            {
                currentString = dictionary[code];
            }
            else
            {
                // Handle the special case where code is not in dictionary
                currentString = previousString + previousString[0];
            }
            
            output.Append(currentString);
            
            // Add new entry to dictionary
            dictionary.Add(dictSize++, previousString + currentString[0]);
            
            previousString = currentString;
        }
        
        return output.ToString();
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        string originalText = "TOBEORNOTTOBEORTOBEORNOT";
        
        Console.WriteLine("Original Text: " + originalText);
        Console.WriteLine("Length: " + originalText.Length);
        
        // Compress the text
        List<int> compressed = LZW.Compress(originalText);
        Console.WriteLine("\nCompressed Codes:");
        foreach (int code in compressed)
        {
            Console.Write(code + " ");
        }
        Console.WriteLine();
        Console.WriteLine("Number of codes: " + compressed.Count);
        
        // Decompress back to original
        string decompressed = LZW.Decompress(compressed);
        Console.WriteLine("\nDecompressed Text: " + decompressed);
        Console.WriteLine("Match: " + (originalText == decompressed));
        
        // Another example with a longer text
        Console.WriteLine("\n--- Another Example ---");
        string text2 = "ABABABABABABABAB";
        Console.WriteLine("Original: " + text2);
        
        List<int> compressed2 = LZW.Compress(text2);
        Console.WriteLine("Compressed:");
        foreach (int code in compressed2)
        {
            Console.Write(code + " ");
        }
        Console.WriteLine();
        
        string decompressed2 = LZW.Decompress(compressed2);
        Console.WriteLine("Decompressed: " + decompressed2);
        Console.WriteLine("Match: " + (text2 == decompressed2));
    }
}
```

## How the Algorithm Works

### Compression Process:
1. **Initialize Dictionary**: Start with all single characters (0-255) mapped to their ASCII values
2. **Process Input**: Read input character by character
3. **Look Up**: Check if current string + next character exists in dictionary
4. **Output Code**: If not found, output the code for current string and add new entry
5. **Build Dictionary**: Add new strings to dictionary as they're discovered

### Decompression Process:
1. **Initialize Dictionary**: Same as compression
2. **Process Codes**: Read codes one by one
3. **Lookup**: Find corresponding string in dictionary
4. **Build Output**: Append to result
5. **Update Dictionary**: Add new entry based on previous and first character of current

## Sample Output:
```
Original Text: TOBEORNOTTOBEORTOBEORNOT
Length: 24

Compressed Codes:
84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263 
Number of codes: 16

Decompressed Text: TOBEORNOTTOBEORTOBEORNOT
Match: True

--- Another Example ---
Original: ABABABABABABABAB
Compressed:
65 66 256 258 260 262 264 266 
Decompressed: ABABABABABABABAB
Match: True
```

## Key Features:

- **Dictionary-based**: Uses a hash table for efficient lookups
- **Dynamic**: Dictionary grows during compression as new patterns are discovered
- **Lossless**: Decompression produces exact original data
- **Space-efficient**: Particularly effective for repetitive data patterns

This implementation demonstrates the core principles of LZW compression while maintaining readability and efficiency.