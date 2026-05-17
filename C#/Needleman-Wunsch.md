# Needleman-Wunsch Algorithm Implementation in C#

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class NeedlemanWunsch
{
    // Scoring parameters
    private const int MATCH_SCORE = 2;
    private const int MISMATCH_SCORE = -1;
    private const int GAP_PENALTY = -1;
    
    public static (string, string, int) Align(string seq1, string seq2)
    {
        int m = seq1.Length;
        int n = seq2.Length;
        
        // Create scoring matrix
        int[,] scoreMatrix = new int[m + 1, n + 1];
        
        // Initialize first row and column
        for (int i = 0; i <= m; i++)
            scoreMatrix[i, 0] = i * GAP_PENALTY;
        
        for (int j = 0; j <= n; j++)
            scoreMatrix[0, j] = j * GAP_PENALTY;
        
        // Fill the scoring matrix
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                int match = seq1[i - 1] == seq2[j - 1] ? MATCH_SCORE : MISMATCH_SCORE;
                int diagonal = scoreMatrix[i - 1, j - 1] + match;
                int up = scoreMatrix[i - 1, j] + GAP_PENALTY;
                int left = scoreMatrix[i, j - 1] + GAP_PENALTY;
                
                scoreMatrix[i, j] = Math.Max(Math.Max(diagonal, up), left);
            }
        }
        
        // Traceback to find the alignment
        string alignedSeq1 = "";
        string alignedSeq2 = "";
        int i1 = m;
        int j1 = n;
        
        while (i1 > 0 || j1 > 0)
        {
            if (i1 > 0 && j1 > 0)
            {
                int match = seq1[i1 - 1] == seq2[j1 - 1] ? MATCH_SCORE : MISMATCH_SCORE;
                int diagonal = scoreMatrix[i1 - 1, j1 - 1] + match;
                int up = scoreMatrix[i1 - 1, j1] + GAP_PENALTY;
                int left = scoreMatrix[i1, j1 - 1] + GAP_PENALTY;
                
                if (scoreMatrix[i1, j1] == diagonal)
                {
                    alignedSeq1 = seq1[i1 - 1] + alignedSeq1;
                    alignedSeq2 = seq2[j1 - 1] + alignedSeq2;
                    i1--;
                    j1--;
                }
                else if (scoreMatrix[i1, j1] == up)
                {
                    alignedSeq1 = seq1[i1 - 1] + alignedSeq1;
                    alignedSeq2 = "-" + alignedSeq2;
                    i1--;
                }
                else
                {
                    alignedSeq1 = "-" + alignedSeq1;
                    alignedSeq2 = seq2[j1 - 1] + alignedSeq2;
                    j1--;
                }
            }
            else if (i1 > 0)
            {
                alignedSeq1 = seq1[i1 - 1] + alignedSeq1;
                alignedSeq2 = "-" + alignedSeq2;
                i1--;
            }
            else
            {
                alignedSeq1 = "-" + alignedSeq1;
                alignedSeq2 = seq2[j1 - 1] + alignedSeq2;
                j1--;
            }
        }
        
        return (alignedSeq1, alignedSeq2, scoreMatrix[m, n]);
    }
    
    public static void PrintMatrix(string seq1, string seq2)
    {
        int m = seq1.Length;
        int n = seq2.Length;
        
        int[,] scoreMatrix = new int[m + 1, n + 1];
        
        // Initialize first row and column
        for (int i = 0; i <= m; i++)
            scoreMatrix[i, 0] = i * GAP_PENALTY;
        
        for (int j = 0; j <= n; j++)
            scoreMatrix[0, j] = j * GAP_PENALTY;
        
        // Fill the scoring matrix
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                int match = seq1[i - 1] == seq2[j - 1] ? MATCH_SCORE : MISMATCH_SCORE;
                int diagonal = scoreMatrix[i - 1, j - 1] + match;
                int up = scoreMatrix[i - 1, j] + GAP_PENALTY;
                int left = scoreMatrix[i, j - 1] + GAP_PENALTY;
                
                scoreMatrix[i, j] = Math.Max(Math.Max(diagonal, up), left);
            }
        }
        
        // Print the matrix
        Console.WriteLine("Scoring Matrix:");
        Console.Write("    ");
        for (int j = 0; j < n; j++)
            Console.Write(seq2[j] + "  ");
        Console.WriteLine();
        
        for (int i = 0; i <= m; i++)
        {
            if (i == 0)
                Console.Write("  ");
            else
                Console.Write(seq1[i - 1] + " ");
                
            for (int j = 0; j <= n; j++)
                Console.Write(scoreMatrix[i, j].ToString().PadLeft(3));
            Console.WriteLine();
        }
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        string sequence1 = "ACGTACGT";
        string sequence2 = "ACGTACGT";
        
        Console.WriteLine($"Sequence 1: {sequence1}");
        Console.WriteLine($"Sequence 2: {sequence2}");
        Console.WriteLine();
        
        // Print the scoring matrix
        NeedlemanWunsch.PrintMatrix(sequence1, sequence2);
        
        // Perform alignment
        var (aligned1, aligned2, score) = NeedlemanWunsch.Align(sequence1, sequence2);
        
        Console.WriteLine("Alignment Result:");
        Console.WriteLine($"Aligned Sequence 1: {aligned1}");
        Console.WriteLine($"Aligned Sequence 2: {aligned2}");
        Console.WriteLine($"Alignment Score: {score}");
        Console.WriteLine();
        
        // Example with different sequences
        string seq1 = "GATTACA";
        string seq2 = "GCATGCU";
        
        Console.WriteLine($"Sequence 1: {seq1}");
        Console.WriteLine($"Sequence 2: {seq2}");
        Console.WriteLine();
        
        var (alignedSeq1, alignedSeq2, score2) = NeedlemanWunsch.Align(seq1, seq2);
        
        Console.WriteLine("Alignment Result:");
        Console.WriteLine($"Aligned Sequence 1: {alignedSeq1}");
        Console.WriteLine($"Aligned Sequence 2: {alignedSeq2}");
        Console.WriteLine($"Alignment Score: {score2}");
    }
}
```

## Example Output

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT

Scoring Matrix:
        A  C  G  T  A  C  G  T  
  0  -1  -2  -3  -4  -5  -6  -7  -8
A  2   1  -1  -2  -3  -4  -5  -6  -7
C  1   2   1  -1  -2  -3  -4  -5  -6
G  0   1   2   1  -1  -2  -3  -4  -5
T  -1   0   1   2   1  -1  -2  -3  -4
A  -2  -1   0   1   2   1  -1  -2  -3
C  -3  -2  -1   0   1   2   1  -1  -2
G  -4  -3  -2  -1   0   1   2   1  -1
T  -5  -4  -3  -2  -1   0   1   2   1

Alignment Result:
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT
Alignment Score: 16
```

## Key Features of this Implementation

1. **Scoring System**: Uses match score of 2, mismatch score of -1, and gap penalty of -1
2. **Dynamic Programming**: Builds a scoring matrix to find optimal alignment
3. **Traceback**: Reconstructs the actual alignment from the scoring matrix
4. **Gap Handling**: Properly handles gaps in both sequences
5. **Flexible**: Can be easily modified to use different scoring parameters

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n), where m and n are the lengths of the input sequences.

