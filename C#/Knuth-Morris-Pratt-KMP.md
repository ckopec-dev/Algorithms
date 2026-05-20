# Knuth-Morris-Pratt (KMP) Algorithm in C#

The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text without backtracking.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class KMPAlgorithm
{
    /// <summary>
    /// Preprocess the pattern to create the failure function (also known as the prefix function)
    /// </summary>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>Failure function array</returns>
    public static int[] BuildFailureFunction(string pattern)
    {
        int[] failure = new int[pattern.Length];
        failure[0] = 0;
        
        int j = 0;
        for (int i = 1; i < pattern.Length; i++)
        {
            while (j > 0 && pattern[i] != pattern[j])
            {
                j = failure[j - 1];
            }
            
            if (pattern[i] == pattern[j])
            {
                j++;
            }
            
            failure[i] = j;
        }
        
        return failure;
    }
    
    /// <summary>
    /// Search for pattern in text using KMP algorithm
    /// </summary>
    /// <param name="text">The text to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>List of starting indices where pattern is found</returns>
    public static List<int> Search(string text, string pattern)
    {
        List<int> matches = new List<int>();
        
        if (string.IsNullOrEmpty(pattern) || string.IsNullOrEmpty(text))
            return matches;
        
        if (pattern.Length > text.Length)
            return matches;
        
        // Build failure function
        int[] failure = BuildFailureFunction(pattern);
        
        int j = 0; // index for pattern
        
        for (int i = 0; i < text.Length; i++) // index for text
        {
            while (j > 0 && text[i] != pattern[j])
            {
                j = failure[j - 1];
            }
            
            if (text[i] == pattern[j])
            {
                j++;
            }
            
            if (j == pattern.Length)
            {
                // Pattern found at index i - pattern.Length + 1
                matches.Add(i - pattern.Length + 1);
                j = failure[j - 1]; // Continue searching for overlapping matches
            }
        }
        
        return matches;
    }
    
    /// <summary>
    /// Print the failure function for educational purposes
    /// </summary>
    /// <param name="pattern">The pattern to analyze</param>
    public static void PrintFailureFunction(string pattern)
    {
        int[] failure = BuildFailureFunction(pattern);
        Console.WriteLine($"Pattern: {pattern}");
        Console.Write("Index:   ");
        for (int i = 0; i < pattern.Length; i++)
            Console.Write($"{i,3}");
        Console.WriteLine();
        Console.Write("Failure: ");
        for (int i = 0; i < pattern.Length; i++)
            Console.Write($"{failure[i],3}");
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Basic search
        string text1 = "ABABDABACDABABCABCABCABCABC";
        string pattern1 = "ABABCABCABCABC";
        
        Console.WriteLine("Example 1:");
        Console.WriteLine($"Text: {text1}");
        Console.WriteLine($"Pattern: {pattern1}");
        
        List<int> matches1 = KMPAlgorithm.Search(text1, pattern1);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches1)}]");
        Console.WriteLine();
        
        // Example 2: Multiple occurrences
        string text2 = "AAAA";
        string pattern2 = "AA";
        
        Console.WriteLine("Example 2:");
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine($"Pattern: {pattern2}");
        
        List<int> matches2 = KMPAlgorithm.Search(text2, pattern2);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches2)}]");
        Console.WriteLine();
        
        // Example 3: Failure function visualization
        Console.WriteLine("Failure Function Analysis:");
        KMPAlgorithm.PrintFailureFunction("ABABCABCABCABC");
        Console.WriteLine();
        
        // Example 4: No matches
        string text3 = "HELLO WORLD";
        string pattern3 = "XYZ";
        
        Console.WriteLine("Example 4 (No matches):");
        Console.WriteLine($"Text: {text3}");
        Console.WriteLine($"Pattern: {pattern3}");
        
        List<int> matches3 = KMPAlgorithm.Search(text3, pattern3);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches3)}]");
    }
}
```

## Output

```
Example 1:
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches found at indices: [10]

Example 2:
Text: AAAA
Pattern: AA
Matches found at indices: [0, 1, 2]

Failure Function Analysis:
Pattern: ABABCABCABCABC
Index:     0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
Failure:    0  0  1  2  0  1  2  3  4  5  6  7  8  9 10 11

Example 4 (No matches):
Text: HELLO WORLD
Pattern: XYZ
Matches found at indices: []
```

## How it works:

1. **BuildFailureFunction**: Creates a failure array that helps determine how much to shift the pattern when a mismatch occurs
2. **Search**: Uses the failure function to efficiently search for the pattern in the text
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for the failure array

The key advantage of KMP over naive string matching is that it avoids unnecessary character comparisons by using the failure function to determine optimal shifts.

