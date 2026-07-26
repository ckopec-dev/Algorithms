# Naïve String Matching Algorithm in C#

The naïve string matching algorithm is a simple approach to find all occurrences of a pattern within a text by checking every possible position.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class NaiveStringMatching
{
    /// <summary>
    /// Find all occurrences of pattern in text using naïve string matching
    /// </summary>
    /// <param name="text">The text to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>List of starting indices where pattern is found</returns>
    public static List<int> FindAllOccurrences(string text, string pattern)
    {
        List<int> occurrences = new List<int>();
        
        // Handle edge cases
        if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(pattern))
            return occurrences;
            
        if (pattern.Length > text.Length)
            return occurrences;
        
        // Check each possible position in text
        for (int i = 0; i <= text.Length - pattern.Length; i++)
        {
            bool match = true;
            
            // Check if pattern matches at current position
            for (int j = 0; j < pattern.Length; j++)
            {
                if (text[i + j] != pattern[j])
                {
                    match = false;
                    break;
                }
            }
            
            // If match found, add index to results
            if (match)
                occurrences.Add(i);
        }
        
        return occurrences;
    }
    
    /// <summary>
    /// Find first occurrence of pattern in text
    /// </summary>
    /// <param name="text">The text to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <returns>Index of first occurrence, or -1 if not found</returns>
    public static int FindFirstOccurrence(string text, string pattern)
    {
        // Handle edge cases
        if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(pattern))
            return -1;
            
        if (pattern.Length > text.Length)
            return -1;
        
        // Check each possible position in text
        for (int i = 0; i <= text.Length - pattern.Length; i++)
        {
            bool match = true;
            
            // Check if pattern matches at current position
            for (int j = 0; j < pattern.Length; j++)
            {
                if (text[i + j] != pattern[j])
                {
                    match = false;
                    break;
                }
            }
            
            // If match found, return index
            if (match)
                return i;
        }
        
        return -1;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        string text = "ABABCABABA";
        string pattern = "ABA";
        
        Console.WriteLine($"Text: {text}");
        Console.WriteLine($"Pattern: {pattern}");
        Console.WriteLine();
        
        // Find all occurrences
        List<int> occurrences = NaiveStringMatching.FindAllOccurrences(text, pattern);
        
        if (occurrences.Count > 0)
        {
            Console.WriteLine($"Pattern found at positions: {string.Join(", ", occurrences)}");
            
            // Display each match
            foreach (int index in occurrences)
            {
                Console.WriteLine($"Match at position {index}: '{text.Substring(index, pattern.Length)}'");
            }
        }
        else
        {
            Console.WriteLine("Pattern not found in text.");
        }
        
        Console.WriteLine();
        
        // Find first occurrence
        int firstOccurrence = NaiveStringMatching.FindFirstOccurrence(text, pattern);
        Console.WriteLine($"First occurrence at index: {firstOccurrence}");
        
        // Example with no matches
        Console.WriteLine("\n--- Example with no matches ---");
        string text2 = "HELLO WORLD";
        string pattern2 = "XYZ";
        List<int> occurrences2 = NaiveStringMatching.FindAllOccurrences(text2, pattern2);
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine($"Pattern: {pattern2}");
        Console.WriteLine($"Matches found: {occurrences2.Count}");
    }
}
```

## Output

```
Text: ABABCABABA
Pattern: ABA

Pattern found at positions: 0, 5, 7
Match at position 0: 'ABA'
Match at position 5: 'ABA'
Match at position 7: 'ABA'

First occurrence at index: 0

--- Example with no matches ---
Text: HELLO WORLD
Pattern: XYZ
Matches found: 0
```

## Algorithm Complexity

- **Time Complexity**: O(n × m) where n is the length of text and m is the length of pattern
- **Space Complexity**: O(k) where k is the number of matches found

## How it works

1. The algorithm checks each possible position in the text where the pattern could fit
2. At each position, it compares the pattern character by character with the text
3. If all characters match, it records the position as a match
4. If any character doesn't match, it moves to the next position

## Advantages and Disadvantages

**Advantages:**
- Simple to understand and implement
- Works well for small texts or patterns
- No preprocessing required

**Disadvantages:**
- Inefficient for large inputs (O(n×m) time complexity)
- May perform redundant comparisons
- Not suitable for real-time applications with large datasets

The naïve approach is mainly useful for educational purposes or when simplicity is preferred over efficiency.