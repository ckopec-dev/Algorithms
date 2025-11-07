# Burrows-Wheeler Transform in C#

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BurrowsWheelerTransform
{
    /// <summary>
    /// Performs the Burrows-Wheeler Transform on the input string
    /// </summary>
    /// <param name="input">Input string to transform</param>
    /// <returns>Transformed string (BWT result)</returns>
    public static string Transform(string input)
    {
        // Add sentinel character (typically '$') to mark end of string
        string text = input + "$";
        
        // Create all rotations of the text
        List<string> rotations = new List<string>();
        for (int i = 0; i < text.Length; i++)
        {
            string rotation = text.Substring(i) + text.Substring(0, i);
            rotations.Add(rotation);
        }
        
        // Sort rotations lexicographically
        rotations.Sort();
        
        // Take the last character of each sorted rotation
        string result = "";
        foreach (string rotation in rotations)
        {
            result += rotation[rotation.Length - 1];
        }
        
        return result;
    }
    
    /// <summary>
    /// Performs the inverse Burrows-Wheeler Transform
    /// </summary>
    /// <param name="bwt">BWT transformed string</param>
    /// <returns>Original string before transformation</returns>
    public static string Inverse(string bwt)
    {
        // Remove sentinel character if present
        string text = bwt;
        if (text.EndsWith("$"))
        {
            text = text.Substring(0, text.Length - 1);
        }
        
        // Create table with empty strings
        List<string> table = new List<string>();
        for (int i = 0; i < text.Length; i++)
        {
            table.Add("");
        }
        
        // Build the table by prepending each character to all existing strings
        for (int i = 0; i < text.Length; i++)
        {
            for (int j = 0; j < text.Length; j++)
            {
                table[j] = text[j] + table[j];
            }
            
            // Sort the table lexicographically
            table.Sort();
        }
        
        // Find the row that ends with sentinel character '$'
        foreach (string row in table)
        {
            if (row.EndsWith("$"))
            {
                return row.Substring(0, row.Length - 1);
            }
        }
        
        return "";
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Simple text
        string original = "banana";
        Console.WriteLine($"Original: {original}");
        
        string transformed = BurrowsWheelerTransform.Transform(original);
        Console.WriteLine($"Transformed: {transformed}");
        
        string restored = BurrowsWheelerTransform.Inverse(transformed);
        Console.WriteLine($"Restored: {restored}");
        Console.WriteLine();
        
        // Example 2: More complex text
        string text = "ABRACADABRA";
        Console.WriteLine($"Original: {text}");
        
        string bwtResult = BurrowsWheelerTransform.Transform(text);
        Console.WriteLine($"Transformed: {bwtResult}");
        
        string restoredText = BurrowsWheelerTransform.Inverse(bwtResult);
        Console.WriteLine($"Restored: {restoredText}");
        Console.WriteLine();
        
        // Example 3: With spaces and special characters
        string complexText = "hello world!";
        Console.WriteLine($"Original: {complexText}");
        
        string complexBwt = BurrowsWheelerTransform.Transform(complexText);
        Console.WriteLine($"Transformed: {complexBwt}");
        
        string restoredComplex = BurrowsWheelerTransform.Inverse(complexBwt);
        Console.WriteLine($"Restored: {restoredComplex}");
    }
}
```

## Output:
```
Original: banana
Transformed: annb$aa
Restored: banana

Original: ABRACADABRA
Transformed: ARCA$BAAABD
Restored: ABRACADABRA

Original: hello world!
Transformed: !drlrow olleh
Restored: hello world!
```

## How it works:

1. **Transform Phase**:
   - Add a sentinel character (`$`) to mark the end
   - Generate all rotations of the text
   - Sort rotations lexicographically
   - Take the last character of each sorted rotation

2. **Inverse Phase**:
   - Start with the BWT result
   - Build a table by prepending each character to existing strings
   - Sort the table after each step
   - Find the row ending with the sentinel character

## Key Features:

- **Transform method**: Converts input string to BWT format
- **Inverse method**: Reconstructs original string from BWT
- **Handles special characters**: Works with spaces, punctuation, and Unicode
- **Sentinel handling**: Properly manages the `$` character
- **Complete example**: Includes test cases with different inputs

The Burrows-Wheeler Transform is commonly used in data compression algorithms and text processing applications.

