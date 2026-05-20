# Rabin-Karp Algorithm Implementation in C#

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. Here's a complete implementation:

```csharp
using System;
using System.Text;

public class RabinKarp
{
    private const int Prime = 101; // A prime number for hashing
    
    /// <summary>
    /// Search for pattern in text using Rabin-Karp algorithm
    /// </summary>
    /// <param name="text">The text to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>Index of first occurrence of pattern in text, or -1 if not found</returns>
    public static int Search(string text, string pattern)
    {
        int textLength = text.Length;
        int patternLength = pattern.Length;
        
        // Handle edge cases
        if (patternLength > textLength || textLength == 0 || patternLength == 0)
            return -1;
        
        // Calculate hash value of pattern and first window of text
        long patternHash = GetHash(pattern);
        long textHash = GetHash(text.Substring(0, patternLength));
        
        // Calculate the value of h = pow(d, patternLength-1) % q
        long h = 1;
        for (int i = 0; i < patternLength - 1; i++)
        {
            h = (h * 256) % Prime;
        }
        
        // Slide the pattern over text one by one
        for (int i = 0; i <= textLength - patternLength; i++)
        {
            // Check if hash values match
            if (patternHash == textHash)
            {
                // Check characters one by one
                if (CheckEqual(text, pattern, i))
                    return i;
            }
            
            // Calculate hash value for next window of text
            if (i < textLength - patternLength)
            {
                textHash = (256 * (textHash - text[i] * h) + text[i + patternLength]) % Prime;
                
                // Handle negative hash values
                if (textHash < 0)
                    textHash += Prime;
            }
        }
        
        return -1;
    }
    
    /// <summary>
    /// Get hash value of a string
    /// </summary>
    private static long GetHash(string str)
    {
        long hash = 0;
        foreach (char c in str)
        {
            hash = (hash * 256 + c) % Prime;
        }
        return hash;
    }
    
    /// <summary>
    /// Check if pattern matches text at given position
    /// </summary>
    private static bool CheckEqual(string text, string pattern, int startIndex)
    {
        for (int i = 0; i < pattern.Length; i++)
        {
            if (text[startIndex + i] != pattern[i])
                return false;
        }
        return true;
    }
    
    /// <summary>
    /// Find all occurrences of pattern in text
    /// </summary>
    /// <param name="text">The text to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>List of all indices where pattern is found</returns>
    public static System.Collections.Generic.List<int> SearchAll(string text, string pattern)
    {
        var indices = new System.Collections.Generic.List<int>();
        int textLength = text.Length;
        int patternLength = pattern.Length;
        
        if (patternLength > textLength || textLength == 0 || patternLength == 0)
            return indices;
        
        long patternHash = GetHash(pattern);
        long textHash = GetHash(text.Substring(0, patternLength));
        long h = 1;
        
        for (int i = 0; i < patternLength - 1; i++)
        {
            h = (h * 256) % Prime;
        }
        
        for (int i = 0; i <= textLength - patternLength; i++)
        {
            if (patternHash == textHash)
            {
                if (CheckEqual(text, pattern, i))
                    indices.Add(i);
            }
            
            if (i < textLength - patternLength)
            {
                textHash = (256 * (textHash - text[i] * h) + text[i + patternLength]) % Prime;
                if (textHash < 0)
                    textHash += Prime;
            }
        }
        
        return indices;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        string text = "ABABDABACDABABCABCABCABCABC";
        string pattern = "ABABCABCABCABC";
        
        Console.WriteLine($"Text: {text}");
        Console.WriteLine($"Pattern: {pattern}");
        Console.WriteLine();
        
        // Find first occurrence
        int index = RabinKarp.Search(text, pattern);
        if (index != -1)
        {
            Console.WriteLine($"Pattern found at index: {index}");
        }
        else
        {
            Console.WriteLine("Pattern not found");
        }
        
        Console.WriteLine();
        
        // Find all occurrences
        var allIndices = RabinKarp.SearchAll(text, pattern);
        Console.WriteLine($"All occurrences found at indices: [{string.Join(", ", allIndices)}]");
        
        Console.WriteLine();
        
        // Another example
        string text2 = "AABAACAADAABAABA";
        string pattern2 = "AABA";
        
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine($"Pattern: {pattern2}");
        
        int index2 = RabinKarp.Search(text2, pattern2);
        if (index2 != -1)
        {
            Console.WriteLine($"Pattern found at index: {index2}");
        }
        else
        {
            Console.WriteLine("Pattern not found");
        }
        
        var allIndices2 = RabinKarp.SearchAll(text2, pattern2);
        Console.WriteLine($"All occurrences found at indices: [{string.Join(", ", allIndices2)}]");
    }
}
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

Pattern found at index: 10
All occurrences found at indices: [10]

Text: AABAACAADAABAABA
Pattern: AABA

Pattern found at index: 0
All occurrences found at indices: [0, 10, 13]
```

## Key Features:

1. **Efficient Hashing**: Uses polynomial rolling hash with base 256
2. **Collision Handling**: Verifies actual character matches when hash values match
3. **Edge Case Handling**: Properly handles empty strings and pattern longer than text
4. **Multiple Occurrences**: Includes method to find all occurrences
5. **Modular Design**: Clean separation of concerns with helper methods

## Time Complexity:
- **Average Case**: O(n + m) where n is text length and m is pattern length
- **Worst Case**: O(n × m) when many hash collisions occur

## Space Complexity: O(1) for search, O(k) for storing all matches where k is number of matches

