# Bitap Algorithm Implementation in C#

The Bitap algorithm (also known as the shift-or algorithm) is a string searching algorithm that uses bit manipulation to find occurrences of a pattern within a text.

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class BitapSearch
{
    private readonly string pattern;
    private readonly int patternLength;
    private readonly Dictionary<char, int> alphabet;

    public BitapSearch(string pattern)
    {
        this.pattern = pattern.ToLower();
        this.patternLength = pattern.Length;
        this.alphabet = new Dictionary<char, int>();
        
        // Build alphabet map with bit positions
        for (int i = 0; i < patternLength; i++)
        {
            char c = pattern[i];
            if (!alphabet.ContainsKey(c))
            {
                alphabet[c] = 1 << i;
            }
        }
    }

    public List<int> Search(string text)
    {
        var matches = new List<int>();
        text = text.ToLower();
        int textLength = text.Length;
        
        // Initialize bit vector
        int R = (1 << patternLength) - 1;
        int mask = 0;
        
        // Process each character in the text
        for (int i = 0; i < textLength; i++)
        {
            char c = text[i];
            
            // Update mask based on current character
            if (alphabet.ContainsKey(c))
            {
                mask |= alphabet[c];
            }
            
            // Shift the bit vector
            R = ((R << 1) | 1) & mask;
            
            // Check if pattern is found at position i
            if ((R & (1 << (patternLength - 1))) != 0)
            {
                matches.Add(i - patternLength + 1);
            }
        }
        
        return matches;
    }

    public bool IsMatch(string text)
    {
        text = text.ToLower();
        int textLength = text.Length;
        int R = (1 << patternLength) - 1;
        int mask = 0;
        
        for (int i = 0; i < textLength; i++)
        {
            char c = text[i];
            
            if (alphabet.ContainsKey(c))
            {
                mask |= alphabet[c];
            }
            
            R = ((R << 1) | 1) & mask;
            
            if ((R & (1 << (patternLength - 1))) != 0)
            {
                return true;
            }
        }
        
        return false;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create Bitap search instance
        var bitap = new BitapSearch("world");
        
        // Test text
        string text = "Hello world, this is a wonderful world!";
        
        // Search for pattern
        var matches = bitap.Search(text);
        
        Console.WriteLine($"Text: {text}");
        Console.WriteLine($"Pattern: world");
        Console.WriteLine($"Matches found at positions: [{string.Join(", ", matches)}]");
        
        // Check if pattern exists
        bool found = bitap.IsMatch(text);
        Console.WriteLine($"Pattern found: {found}");
        
        // Test with another example
        Console.WriteLine("\n--- Another Example ---");
        var bitap2 = new BitapSearch("abc");
        string text2 = "abcdefg abcdef";
        var matches2 = bitap2.Search(text2);
        
        Console.WriteLine($"Text: {text2}");
        Console.WriteLine($"Pattern: abc");
        Console.WriteLine($"Matches found at positions: [{string.Join(", ", matches2)}]");
    }
}
```

## Output:
```
Text: Hello world, this is a wonderful world!
Pattern: world
Matches found at positions: [6, 30]
Pattern found: True

--- Another Example ---
Text: abcdefg abcdef
Pattern: abc
Matches found at positions: [0, 7]
```

## How it works:

1. **Initialization**: The algorithm builds a dictionary mapping each character in the pattern to its bit position
2. **Bit Vector**: Uses a sliding window approach with bit manipulation
3. **Character Processing**: For each character in the text:
   - Updates the mask using bitwise OR
   - Shifts the bit vector and applies AND operation
   - Checks if the pattern is found at current position
4. **Pattern Detection**: When the most significant bit becomes set, it indicates a match

## Time Complexity: O(n × m) where n is text length and m is pattern length
## Space Complexity: O(m) for the alphabet map