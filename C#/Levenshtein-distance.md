# Levenshtein Distance Algorithm in C#

The Levenshtein distance is a string metric that measures the difference between two sequences. It's the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one word into another.

## Implementation

```csharp
using System;

public class LevenshteinDistance
{
    public static int Calculate(string source, string target)
    {
        // Handle null inputs
        if (string.IsNullOrEmpty(source))
            return string.IsNullOrEmpty(target) ? 0 : target.Length;
        
        if (string.IsNullOrEmpty(target))
            return source.Length;

        int sourceLength = source.Length;
        int targetLength = target.Length;

        // Create a matrix to store the distances
        int[,] distance = new int[sourceLength + 1, targetLength + 1];

        // Initialize the first row and column
        for (int i = 0; i <= sourceLength; i++)
            distance[i, 0] = i;

        for (int j = 0; j <= targetLength; j++)
            distance[0, j] = j;

        // Fill the matrix
        for (int i = 1; i <= sourceLength; i++)
        {
            for (int j = 1; j <= targetLength; j++)
            {
                int cost = source[i - 1] == target[j - 1] ? 0 : 1;
                
                distance[i, j] = Math.Min(
                    Math.Min(
                        distance[i - 1, j] + 1,        // deletion
                        distance[i, j - 1] + 1         // insertion
                    ),
                    distance[i - 1, j - 1] + cost     // substitution
                );
            }
        }

        return distance[sourceLength, targetLength];
    }

    // Alternative implementation with better space complexity
    public static int CalculateOptimized(string source, string target)
    {
        if (string.IsNullOrEmpty(source))
            return string.IsNullOrEmpty(target) ? 0 : target.Length;
        
        if (string.IsNullOrEmpty(target))
            return source.Length;

        // Use only two rows instead of full matrix
        int[] prevRow = new int[target.Length + 1];
        int[] currRow = new int[target.Length + 1];

        // Initialize first row
        for (int j = 0; j <= target.Length; j++)
            prevRow[j] = j;

        // Fill the rows
        for (int i = 1; i <= source.Length; i++)
        {
            currRow[0] = i;
            
            for (int j = 1; j <= target.Length; j++)
            {
                int cost = source[i - 1] == target[j - 1] ? 0 : 1;
                
                currRow[j] = Math.Min(
                    Math.Min(
                        prevRow[j] + 1,        // deletion
                        currRow[j - 1] + 1     // insertion
                    ),
                    prevRow[j - 1] + cost     // substitution
                );
            }

            // Swap rows
            int[] temp = prevRow;
            prevRow = currRow;
            currRow = temp;
        }

        return prevRow[target.Length];
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        string word1 = "kitten";
        string word2 = "sitting";
        
        int distance = LevenshteinDistance.Calculate(word1, word2);
        Console.WriteLine($"Levenshtein distance between '{word1}' and '{word2}': {distance}");
        
        // More examples
        Console.WriteLine($"Distance between 'hello' and 'world': {LevenshteinDistance.Calculate("hello", "world")}");
        Console.WriteLine($"Distance between 'cat' and 'dog': {LevenshteinDistance.Calculate("cat", "dog")}");
        Console.WriteLine($"Distance between 'saturday' and 'sunday': {LevenshteinDistance.Calculate("saturday", "sunday")}");
        Console.WriteLine($"Distance between 'same' and 'same': {LevenshteinDistance.Calculate("same", "same")}");
        Console.WriteLine($"Distance between '' and 'test': {LevenshteinDistance.Calculate("", "test")}");
    }
}
```

## Output
```
Levenshtein distance between 'kitten' and 'sitting': 3
Distance between 'hello' and 'world': 4
Distance between 'cat' and 'dog': 3
Distance between 'saturday' and 'sunday': 3
Distance between 'same' and 'same': 0
Distance between '' and 'test': 4
```

## How it works

1. **Initialization**: Create a matrix where `distance[i,j]` represents the Levenshtein distance between the first `i` characters of the source string and the first `j` characters of the target string.

2. **Base cases**: 
   - The distance from an empty string to a string of length `n` is `n` (all insertions)
   - The distance from a string of length `n` to an empty string is `n` (all deletions)

3. **Recurrence relation**: For each cell `distance[i,j]`, calculate the minimum of:
   - `distance[i-1,j] + 1` (deletion)
   - `distance[i,j-1] + 1` (insertion)  
   - `distance[i-1,j-1] + cost` (substitution, where cost is 0 if characters match, 1 otherwise)

4. **Result**: The bottom-right cell contains the final Levenshtein distance.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the basic implementation, O(min(m,n)) for the optimized version

The optimized version uses only two rows instead of a full matrix, reducing space complexity while maintaining the same time complexity.

