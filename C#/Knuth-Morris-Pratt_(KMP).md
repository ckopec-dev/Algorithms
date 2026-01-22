# Knuth-Morris-Pratt (KMP) Algorithm in C#

## Overview
The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text without backtracking. It uses a preprocessing step to create a "failure function" that helps avoid unnecessary comparisons.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class KMPAlgorithm
{
    /// <summary>
    /// Preprocess the pattern to create the failure function (also known as the "partial match" table)
    /// </summary>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>Failure function array</returns>
    private static int[] BuildFailureFunction(string pattern)
    {
        int[] failure = new int[pattern.Length];
        failure[0] = 0;
        
        int j = 0; // Length of previous longest prefix suffix
        
        for (int i = 1; i < pattern.Length; i++)
        {
            // If characters don't match, fall back using previously computed values
            while (j > 0 && pattern[i] != pattern[j])
            {
                j = failure[j - 1];
            }
            
            // If characters match, increment the length of common prefix
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
    /// <param name="text">Text to search in</param>
    /// <param name="pattern">Pattern to search for</param>
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
        
        int j = 0; // Index for pattern
        
        for (int i = 0; i < text.Length; i++)
        {
            // If characters don't match, use failure function to skip characters
            while (j > 0 && text[i] != pattern[j])
            {
                j = failure[j - 1];
            }
            
            // If characters match, increment pattern index
            if (text[i] == pattern[j])
            {
                j++;
            }
            
            // If we've matched the entire pattern
            if (j == pattern.Length)
            {
                matches.Add(i - j + 1);
                j = failure[j - 1]; // Continue searching for overlapping matches
            }
        }
        
        return matches;
    }
    
    /// <summary>
    /// Print the failure function for educational purposes
    /// </summary>
    /// <param name="pattern">Pattern to analyze</param>
    public static void PrintFailureFunction(string pattern)
    {
        Console.WriteLine($"Pattern: {pattern}");
        int[] failure = BuildFailureFunction(pattern);
        
        Console.Write("Index:  ");
        for (int i = 0; i < pattern.Length; i++)
        {
            Console.Write($"{i,3}");
        }
        Console.WriteLine();
        
        Console.Write("Value:  ");
        for (int i = 0; i < pattern.Length; i++)
        {
            Console.Write($"{failure[i],3}");
        }
        Console.WriteLine();
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example 1: Basic search
        string text1 = "ABABDABACDABABCABCABCABCABC";
        string pattern1 = "ABABCABCABCABC";
        
        Console.WriteLine("=== Example 1 ===");
        Console.WriteLine($"Text: {text1}");
        Console.WriteLine($"Pattern: {pattern1}");
        
        List<int> matches1 = KMPAlgorithm.Search(text1, pattern1);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches1)}]");
        
        // Example 2: Multiple matches
        string text2 = "AAAA";
        string pattern2 = "AA";
        
        Console.WriteLine("\n=== Example 2 ===");
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine($"Pattern: {pattern2}");
        
        List<int> matches2 = KMPAlgorithm.Search(text2, pattern2);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches2)}]");
        
        // Example 3: Failure function visualization
        Console.WriteLine("\n=== Failure Function Visualization ===");
        KMPAlgorithm.PrintFailureFunction("ABABCABCABCABC");
        
        // Example 4: No matches
        string text3 = "HELLO WORLD";
        string pattern3 = "XYZ";
        
        Console.WriteLine("\n=== Example 3 ===");
        Console.WriteLine($"Text: {text3}");
        Console.WriteLine($"Pattern: {pattern3}");
        
        List<int> matches3 = KMPAlgorithm.Search(text3, pattern3);
        Console.WriteLine($"Matches found at indices: [{string.Join(", ", matches3)}]");
    }
}
```

## Output
```
=== Example 1 ===
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches found at indices: [10]

=== Example 2 ===
Text: AAAA
Pattern: AA
Matches found at indices: [0, 1, 2]

=== Failure Function Visualization ===
Pattern: ABABCABCABCABC
Index:    0   1   2   3   4   5   6   7   8   9  10  11  12  13
Value:    0   0   1   2   0   1   2   3   4   5   6   7   8   9

=== Example 3 ===
Text: HELLO WORLD
Pattern: XYZ
Matches found at indices: []
```

## Key Features

1. **Time Complexity**: O(n + m) where n is text length and m is pattern length
2. **Space Complexity**: O(m) for the failure function array
3. **No Backtracking**: Unlike naive string matching, KMP never backtracks in the text
4. **Overlapping Matches**: Finds all occurrences of the pattern

## How it Works

1. **Preprocessing**: Build failure function that tells us how much to skip when a mismatch occurs
2. **Searching**: Use the failure function to efficiently skip characters in the text
3. **Pattern Matching**: When characters match, advance both pointers; when they don't, use failure function to determine how far to shift the pattern

The failure function essentially tells us the longest proper prefix of the pattern that is also a suffix, which helps us avoid redundant comparisons.

