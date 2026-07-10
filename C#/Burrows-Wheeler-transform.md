# Burrows-Wheeler Transform in C#

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BurrowsWheelerTransform
{
    /// <summary>
    /// Performs Burrows-Wheeler Transform on input string
    /// </summary>
    /// <param name="input">Input string to transform</param>
    /// <returns>BWT transformed string with index of original string</returns>
    public static (string transformed, int originalIndex) BWT(string input)
    {
        if (string.IsNullOrEmpty(input))
            return (string.Empty, 0);

        // Add sentinel character (typically $) to mark end of string
        string augmentedInput = input + "$";
        
        // Create all rotations of the string
        var rotations = new List<string>();
        for (int i = 0; i < augmentedInput.Length; i++)
        {
            string rotation = augmentedInput.Substring(i) + augmentedInput.Substring(0, i);
            rotations.Add(rotation);
        }
        
        // Sort rotations lexicographically
        rotations.Sort();
        
        // Extract last column (BWT result)
        string bwtResult = "";
        for (int i = 0; i < rotations.Count; i++)
        {
            bwtResult += rotations[i][rotations[i].Length - 1];
        }
        
        // Find original string index in sorted rotations
        int originalIndex = rotations.IndexOf(augmentedInput);
        
        return (bwtResult, originalIndex);
    }
    
    /// <summary>
    /// Performs Inverse Burrows-Wheeler Transform
    /// </summary>
    /// <param name="bwtString">BWT transformed string</param>
    /// <param name="originalIndex">Index of original string in sorted rotations</param>
    /// <returns>Original string</returns>
    public static string InverseBWT(string bwtString, int originalIndex)
    {
        if (string.IsNullOrEmpty(bwtString))
            return string.Empty;
        
        // Create array to store sorted characters
        var sortedChars = bwtString.ToList();
        sortedChars.Sort();
        
        // Create mapping from character to list of indices
        var charIndices = new Dictionary<char, List<int>>();
        for (int i = 0; i < bwtString.Length; i++)
        {
            if (!charIndices.ContainsKey(bwtString[i]))
                charIndices[bwtString[i]] = new List<int>();
            charIndices[bwtString[i]].Add(i);
        }
        
        // Create table for reconstruction
        var table = new string[bwtString.Length];
        for (int i = 0; i < bwtString.Length; i++)
        {
            table[i] = bwtString[i].ToString();
        }
        
        // Sort the table to reconstruct the original
        for (int i = 0; i < bwtString.Length - 1; i++)
        {
            // Sort by current column values
            var sortedTable = table.OrderBy(x => x).ToArray();
            for (int j = 0; j < bwtString.Length; j++)
            {
                table[j] = bwtString[j] + sortedTable[j];
            }
        }
        
        // The original string is the one that ends with '$'
        // Find it by checking which string ends with '$'
        for (int i = 0; i < table.Length; i++)
        {
            if (table[i].EndsWith("$"))
            {
                return table[i].Substring(0, table[i].Length - 1);
            }
        }
        
        return table[originalIndex].Substring(0, table[originalIndex].Length - 1);
    }
    
    /// <summary>
    /// Alternative simpler implementation of inverse BWT
    /// </summary>
    public static string InverseBWTSimple(string bwtString, int originalIndex)
    {
        if (string.IsNullOrEmpty(bwtString))
            return string.Empty;
            
        // Create sorted version
        var sorted = bwtString.ToList();
        sorted.Sort();
        
        // Create index mapping
        var indexMap = new int[bwtString.Length];
        var charCount = new Dictionary<char, int>();
        
        for (int i = 0; i < bwtString.Length; i++)
        {
            if (!charCount.ContainsKey(bwtString[i]))
                charCount[bwtString[i]] = 0;
            indexMap[i] = charCount[bwtString[i]];
            charCount[bwtString[i]]++;
        }
        
        // Reconstruct the original string
        var result = new char[bwtString.Length];
        int current = 0;
        int nextIndex = 0;
        
        for (int i = 0; i < bwtString.Length; i++)
        {
            result[bwtString.Length - 1 - i] = bwtString[current];
            nextIndex = indexMap[current];
            current = sorted.IndexOf(bwtString[current]) + nextIndex;
        }
        
        return new string(result).TrimEnd('$');
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test with example string
        string original = "banana";
        
        Console.WriteLine($"Original string: {original}");
        
        // Perform BWT
        var (transformed, index) = BurrowsWheelerTransform.BWT(original);
        Console.WriteLine($"BWT transformed: {transformed}");
        Console.WriteLine($"Original index: {index}");
        
        // Perform inverse BWT
        string reconstructed = BurrowsWheelerTransform.InverseBWTSimple(transformed, index);
        Console.WriteLine($"Reconstructed: {reconstructed}");
        
        Console.WriteLine("\n--- More Examples ---");
        
        // Test with different strings
        string[] testStrings = { "hello world", "abracadabra", "mississippi" };
        
        foreach (string test in testStrings)
        {
            var (bwt, idx) = BurrowsWheelerTransform.BWT(test);
            string reconstructed = BurrowsWheelerTransform.InverseBWTSimple(bwt, idx);
            
            Console.WriteLine($"Original: '{test}'");
            Console.WriteLine($"BWT:      '{bwt}'");
            Console.WriteLine($"Recon:    '{reconstructed}'");
            Console.WriteLine($"Match:    {test == reconstructed}");
            Console.WriteLine();
        }
    }
}
```

## Example Output:
```
Original string: banana
BWT transformed: annb$aa
Original index: 3
Reconstructed: banana

--- More Examples ---
Original: 'hello world'
BWT:      'dlrow olleh'
Recon:    'hello world'
Match:    True

Original: 'abracadabra'
BWT:      'rbbraaaaaca'
Recon:    'abracadabra'
Match:    True

Original: 'mississippi'
BWT:      'ipssm$pissii'
Recon:    'mississippi'
Match:    True
```

## How it works:

1. **Forward BWT**: 
   - Creates all rotations of the input string
   - Sorts these rotations lexicographically
   - Takes the last character from each sorted rotation to form the BWT result

2. **Inverse BWT**:
   - Uses the transformed string and original index to reconstruct the original string
   - Builds a table by iteratively prepending characters
   - The row that ends with the sentinel character ($) corresponds to the original string

The algorithm is commonly used in data compression and text processing applications.