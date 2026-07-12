# Damerau-Levenshtein Distance Algorithm in C#

The Damerau-Levenshtein distance is a string metric that measures the minimum number of single-character edits (insertions, deletions, substitutions, or transpositions) required to change one word into another.

## Implementation

```csharp
using System;

public class DamerauLevenshtein
{
    public static int CalculateDistance(string source, string target)
    {
        if (string.IsNullOrEmpty(source))
            return string.IsNullOrEmpty(target) ? 0 : target.Length;
        
        if (string.IsNullOrEmpty(target))
            return source.Length;

        int[,] distance = new int[source.Length + 1, target.Length + 1];

        // Initialize first row and column
        for (int i = 0; i <= source.Length; i++)
            distance[i, 0] = i;
        
        for (int j = 0; j <= target.Length; j++)
            distance[0, j] = j;

        // Fill the distance matrix
        for (int i = 1; i <= source.Length; i++)
        {
            for (int j = 1; j <= target.Length; j++)
            {
                int cost = source[i - 1] == target[j - 1] ? 0 : 1;
                
                distance[i, j] = Math.Min(
                    Math.Min(
                        distance[i - 1, j] + 1,        // deletion
                        distance[i, j - 1] + 1         // insertion
                    ),
                    distance[i - 1, j - 1] + cost     // substitution
                );

                // Check for transposition (Damerau-Levenshtein specific)
                if (i > 1 && j > 1 && 
                    source[i - 1] == target[j - 2] && 
                    source[i - 2] == target[j - 1])
                {
                    distance[i, j] = Math.Min(
                        distance[i, j],
                        distance[i - 2, j - 2] + cost
                    );
                }
            }
        }

        return distance[source.Length, target.Length];
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Test cases
        Console.WriteLine("Damerau-Levenshtein Distance Examples:");
        Console.WriteLine("-------------------------------------");
        
        string[][] testCases = {
            new string[] {"kitten", "sitting"},
            new string[] {"saturday", "sunday"},
            new string[] {"abc", "acb"},
            new string[] {"hello", "hallo"},
            new string[] {"", "test"},
            new string[] {"same", "same"}
        };

        foreach (var testCase in testCases)
        {
            int distance = DamerauLevenshtein.CalculateDistance(testCase[0], testCase[1]);
            Console.WriteLine($"'{testCase[0]}' -> '{testCase[1]}': {distance}");
        }
    }
}
```

## Output
```
Damerau-Levenshtein Distance Examples:
-------------------------------------
'kitten' -> 'sitting': 3
'saturday' -> 'sunday': 3
'abc' -> 'acb': 1
'hello' -> 'hallo': 1
'' -> 'test': 4
'same' -> 'same': 0
```

## How it works:

1. **Initialization**: Create a matrix where `distance[i,j]` represents the distance between the first `i` characters of source and the first `j` characters of target.

2. **Base cases**: Fill the first row and column with incremental values (representing insertions/deletions).

3. **Recurrence relation**: For each cell, calculate the minimum cost of:
   - Deletion: `distance[i-1,j] + 1`
   - Insertion: `distance[i,j-1] + 1`  
   - Substitution: `distance[i-1,j-1] + cost`

4. **Transposition check**: If characters at positions `(i-1,j-2)` and `(i-2,j-1)` are the same, check if a transposition would be more efficient.

## Time Complexity
- **Time**: O(m × n) where m and n are the lengths of the strings
- **Space**: O(m × n) for the distance matrix

This implementation correctly handles all four operations: insertion, deletion, substitution, and transposition, making it a complete Damerau-Levenshtein distance calculator.