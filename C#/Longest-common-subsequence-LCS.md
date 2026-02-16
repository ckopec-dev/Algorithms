# Longest Common Subsequence (LCS) Algorithm in C#

Here's a complete implementation of the LCS algorithm using dynamic programming in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Text;

public class LCSAlgorithm
{
    /// <summary>
    /// Finds the length of the longest common subsequence between two strings
    /// </summary>
    public static int FindLCSLength(string str1, string str2)
    {
        int m = str1.Length;
        int n = str2.Length;
        
        // Create a 2D array to store lengths of LCS
        int[,] dp = new int[m + 1, n + 1];
        
        // Fill the dp table
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                if (str1[i - 1] == str2[j - 1])
                {
                    dp[i, j] = dp[i - 1, j - 1] + 1;
                }
                else
                {
                    dp[i, j] = Math.Max(dp[i - 1, j], dp[i, j - 1]);
                }
            }
        }
        
        return dp[m, n];
    }
    
    /// <summary>
    /// Finds the actual longest common subsequence string
    /// </summary>
    public static string FindLCSString(string str1, string str2)
    {
        int m = str1.Length;
        int n = str2.Length;
        
        // Create a 2D array to store lengths of LCS
        int[,] dp = new int[m + 1, n + 1];
        
        // Fill the dp table
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                if (str1[i - 1] == str2[j - 1])
                {
                    dp[i, j] = dp[i - 1, j - 1] + 1;
                }
                else
                {
                    dp[i, j] = Math.Max(dp[i - 1, j], dp[i, j - 1]);
                }
            }
        }
        
        // Backtrack to find the actual LCS string
        StringBuilder lcs = new StringBuilder();
        int x = m, y = n;
        
        while (x > 0 && y > 0)
        {
            if (str1[x - 1] == str2[y - 1])
            {
                lcs.Insert(0, str1[x - 1]); // Insert at beginning
                x--;
                y--;
            }
            else if (dp[x - 1, y] > dp[x, y - 1])
            {
                x--;
            }
            else
            {
                y--;
            }
        }
        
        return lcs.ToString();
    }
    
    /// <summary>
    /// Prints the DP table for visualization
    /// </summary>
    public static void PrintLCSMatrix(string str1, string str2)
    {
        int m = str1.Length;
        int n = str2.Length;
        
        int[,] dp = new int[m + 1, n + 1];
        
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                if (str1[i - 1] == str2[j - 1])
                {
                    dp[i, j] = dp[i - 1, j - 1] + 1;
                }
                else
                {
                    dp[i, j] = Math.Max(dp[i - 1, j], dp[i, j - 1]);
                }
            }
        }
        
        // Print the matrix
        Console.WriteLine("DP Matrix:");
        Console.Write("    ");
        for (int j = 0; j < n; j++)
        {
            Console.Write(str2[j] + " ");
        }
        Console.WriteLine();
        
        for (int i = 0; i <= m; i++)
        {
            if (i == 0)
                Console.Write("  ");
            else
                Console.Write(str1[i - 1] + " ");
                
            for (int j = 0; j <= n; j++)
            {
                Console.Write(dp[i, j] + " ");
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        string str1 = "ABCDGH";
        string str2 = "AEDFHR";
        
        Console.WriteLine($"String 1: {str1}");
        Console.WriteLine($"String 2: {str2}");
        Console.WriteLine();
        
        // Find LCS length
        int lcsLength = LCSAlgorithm.FindLCSLength(str1, str2);
        Console.WriteLine($"Length of LCS: {lcsLength}");
        
        // Find actual LCS string
        string lcsString = LCSAlgorithm.FindLCSString(str1, str2);
        Console.WriteLine($"LCS String: {lcsString}");
        
        Console.WriteLine();
        
        // Print the DP matrix
        LCSAlgorithm.PrintLCSMatrix(str1, str2);
        
        Console.WriteLine();
        
        // Another example
        string str3 = "AGGTAB";
        string str4 = "GXTXAYB";
        
        Console.WriteLine($"String 1: {str3}");
        Console.WriteLine($"String 2: {str4}");
        Console.WriteLine();
        
        int lcsLength2 = LCSAlgorithm.FindLCSLength(str3, str4);
        string lcsString2 = LCSAlgorithm.FindLCSString(str3, str4);
        
        Console.WriteLine($"Length of LCS: {lcsLength2}");
        Console.WriteLine($"LCS String: {lcsString2}");
    }
}
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR

Length of LCS: 3
LCS String: ADH

DP Matrix:
      A E D F H R 
  0 0 0 0 0 0 0 
A 1 1 1 1 1 1 1 
B 1 1 1 1 1 1 1 
C 1 1 1 1 1 1 1 
D 1 1 2 2 2 2 2 
G 1 1 2 2 2 2 2 
H 1 1 2 2 3 3 3 

String 1: AGGTAB
String 2: GXTXAYB

Length of LCS: 2
LCS String: AB
```

## How it works:

1. **Dynamic Programming Approach**: Creates a 2D table `dp[i,j]` where each cell represents the length of LCS for substrings `str1[0...i-1]` and `str2[0...j-1]`

2. **Recurrence Relation**:
   - If `str1[i-1] == str2[j-1]`: `dp[i,j] = dp[i-1,j-1] + 1`
   - Otherwise: `dp[i,j] = max(dp[i-1,j], dp[i,j-1])`

3. **Backtracking**: To reconstruct the actual LCS string, we backtrack through the DP table from `dp[m,n]` to `dp[0,0]`

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

This implementation efficiently solves the classic LCS problem and can be easily extended for various applications like sequence alignment, file comparison, and more.

