# Boyer-Moore Algorithm Implementation in C#

Here's a complete implementation of the Boyer-Moore string searching algorithm in C#:

```csharp
using System;
using System.Collections.Generic;

public class BoyerMooreSearch
{
    /// <summary>
    /// Preprocesses the pattern to create the bad character table
    /// </summary>
    private static int[] BuildBadCharacterTable(string pattern)
    {
        int[] badCharTable = new int[256]; // ASCII characters
        
        // Initialize all entries to -1
        for (int i = 0; i < 256; i++)
        {
            badCharTable[i] = -1;
        }
        
        // Fill the table with last occurrence positions
        for (int i = 0; i < pattern.Length; i++)
        {
            badCharTable[pattern[i]] = i;
        }
        
        return badCharTable;
    }
    
    /// <summary>
    /// Preprocesses the pattern to create the good suffix table
    /// </summary>
    private static int[] BuildGoodSuffixTable(string pattern)
    {
        int m = pattern.Length;
        int[] goodSuffixTable = new int[m];
        
        // Initialize all entries to 0
        for (int i = 0; i < m; i++)
        {
            goodSuffixTable[i] = 0;
        }
        
        // Build the table using KMP-like approach
        int[] suffix = new int[m];
        suffix[m - 1] = m;
        
        int i = m - 2;
        int j = m - 1;
        
        while (i >= 0)
        {
            while (j < m - 1 && pattern[i] != pattern[j])
            {
                j = goodSuffixTable[j];
            }
            
            suffix[i] = j - i + 1;
            i--;
            j--;
        }
        
        // Fill the good suffix table
        for (int k = 0; k < m; k++)
        {
            goodSuffixTable[k] = 0;
        }
        
        int l = 0;
        for (int k = m - 1; k >= 0; k--)
        {
            if (suffix[k] == k + 1)
            {
                for (; l < m - k; l++)
                {
                    if (goodSuffixTable[l] == 0)
                        goodSuffixTable[l] = m - k;
                }
            }
        }
        
        for (int k = 0; k < m - 1; k++)
        {
            goodSuffixTable[m - suffix[k]] = m - k;
        }
        
        return goodSuffixTable;
    }
    
    /// <summary>
    /// Performs Boyer-Moore search on the text
    /// </summary>
    public static List<int> Search(string text, string pattern)
    {
        List<int> matches = new List<int>();
        
        if (string.IsNullOrEmpty(pattern) || string.IsNullOrEmpty(text))
            return matches;
            
        if (pattern.Length > text.Length)
            return matches;
        
        int[] badCharTable = BuildBadCharacterTable(pattern);
        int[] goodSuffixTable = BuildGoodSuffixTable(pattern);
        
        int n = text.Length;
        int m = pattern.Length;
        int i = 0;
        
        while (i <= n - m)
        {
            int j = m - 1;
            
            // Match pattern from right to left
            while (j >= 0 && pattern[j] == text[i + j])
            {
                j--;
            }
            
            // If pattern is found
            if (j < 0)
            {
                matches.Add(i);
                i += goodSuffixTable[0];
            }
            else
            {
                // Calculate shift using bad character rule and good suffix rule
                int badCharShift = j - badCharTable[text[i + j]];
                int goodSuffixShift = goodSuffixTable[j];
                
                i += Math.Max(badCharShift, goodSuffixShift);
            }
        }
        
        return matches;
    }
    
    /// <summary>
    /// Simple Boyer-Moore implementation using only bad character heuristic
    /// </summary>
    public static List<int> SearchSimple(string text, string pattern)
    {
        List<int> matches = new List<int>();
        
        if (string.IsNullOrEmpty(pattern) || string.IsNullOrEmpty(text))
            return matches;
            
        if (pattern.Length > text.Length)
            return matches;
        
        int[] badCharTable = BuildBadCharacterTable(pattern);
        
        int n = text.Length;
        int m = pattern.Length;
        int i = 0;
        
        while (i <= n - m)
        {
            int j = m - 1;
            
            // Match pattern from right to left
            while (j >= 0 && pattern[j] == text[i + j])
            {
                j--;
            }
            
            // If pattern is found
            if (j < 0)
            {
                matches.Add(i);
                i += m; // Simple shift by pattern length
            }
            else
            {
                // Calculate shift using bad character rule
                int shift = j - badCharTable[text[i + j]];
                i += Math.Max(1, shift);
            }
        }
        
        return matches;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        string text = "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC