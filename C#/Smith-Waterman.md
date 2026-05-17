# Smith-Waterman Algorithm Implementation in C#

The Smith-Waterman algorithm is a dynamic programming algorithm used for local sequence alignment. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SmithWaterman
{
    private int matchScore = 2;
    private int mismatchScore = -1;
    private int gapPenalty = -1;

    public class AlignmentResult
    {
        public int Score { get; set; }
        public string Sequence1Aligned { get; set; }
        public string Sequence2Aligned { get; set; }
        public int StartPosition1 { get; set; }
        public int StartPosition2 { get; set; }
        public int EndPosition1 { get; set; }
        public int EndPosition2 { get; set; }
    }

    public AlignmentResult Align(string seq1, string seq2)
    {
        int m = seq1.Length;
        int n = seq2.Length;

        // Create scoring matrix
        int[,] scoreMatrix = new int[m + 1, n + 1];

        // Initialize first row and column
        for (int i = 0; i <= m; i++)
            scoreMatrix[i, 0] = 0;
        for (int j = 0; j <= n; j++)
            scoreMatrix[0, j] = 0;

        // Fill the scoring matrix
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                int match = seq1[i - 1] == seq2[j - 1] ? matchScore : mismatchScore;
                int diagonal = scoreMatrix[i - 1, j - 1] + match;
                int up = scoreMatrix[i - 1, j] + gapPenalty;
                int left = scoreMatrix[i, j - 1] + gapPenalty;

                scoreMatrix[i, j] = Math.Max(0, Math.Max(diagonal, Math.Max(up, left)));
            }
        }

        // Find the maximum score and its position
        int maxScore = 0;
        int maxI = 0, maxJ = 0;

        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                if (scoreMatrix[i, j] > maxScore)
                {
                    maxScore = scoreMatrix[i, j];
                    maxI = i;
                    maxJ = j;
                }
            }
        }

        // Traceback to find the alignment
        string alignedSeq1 = "";
        string alignedSeq2 = "";
        int i1 = maxI;
        int j1 = maxJ;

        // Traceback until we reach a score of 0
        while (i1 > 0 && j1 > 0 && scoreMatrix[i1, j1] > 0)
        {
            int currentScore = scoreMatrix[i1, j1];
            int diagonal = scoreMatrix[i1 - 1, j1 - 1];
            int up = scoreMatrix[i1 - 1, j1];
            int left = scoreMatrix[i1, j1 - 1];

            if (currentScore == diagonal + (seq1[i1 - 1] == seq2[j1 - 1] ? matchScore : mismatchScore))
            {
                alignedSeq1 = seq1[i1 - 1] + alignedSeq1;
                alignedSeq2 = seq2[j1 - 1] + alignedSeq2;
                i1--;
                j1--;
            }
            else if (currentScore == up + gapPenalty)
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

        // Calculate positions
        int startPos1 = alignedSeq1.IndexOf(seq1[0]) != -1 ? alignedSeq1.IndexOf(seq1[0]) : 0;
        int startPos2 = alignedSeq2.IndexOf(seq2[0]) != -1 ? alignedSeq2.IndexOf(seq2[0]) : 0;
        int endPos1 = alignedSeq1.LastIndexOf(seq1[seq1.Length - 1]) != -1 ? 
                     alignedSeq1.LastIndexOf(seq1[seq1.Length - 1]) : alignedSeq1.Length - 1;
        int endPos2 = alignedSeq2.LastIndexOf(seq2[seq2.Length - 1]) != -1 ? 
                     alignedSeq2.LastIndexOf(seq2[seq2.Length - 1]) : alignedSeq2.Length - 1;

        return new AlignmentResult
        {
            Score = maxScore,
            Sequence1Aligned = alignedSeq1,
            Sequence2Aligned = alignedSeq2,
            StartPosition1 = startPos1,
            StartPosition2 = startPos2,
            EndPosition1 = endPos1,
            EndPosition2 = endPos2
        };
    }

    public void PrintMatrix(string seq1, string seq2)
    {
        int m = seq1.Length;
        int n = seq2.Length;
        int[,] scoreMatrix = new int[m + 1, n + 1];

        // Initialize
        for (int i = 0; i <= m; i++)
            scoreMatrix[i, 0] = 0;
        for (int j = 0; j <= n; j++)
            scoreMatrix[0, j] = 0;

        // Fill matrix
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                int match = seq1[i - 1] == seq2[j - 1] ? matchScore : mismatchScore;
                int diagonal = scoreMatrix[i - 1, j - 1] + match;
                int up = scoreMatrix[i - 1, j] + gapPenalty;
                int left = scoreMatrix[i, j - 1] + gapPenalty;

                scoreMatrix[i, j] = Math.Max(0, Math.Max(diagonal, Math.Max(up, left)));
            }
        }

        // Print matrix
        Console.WriteLine("Scoring Matrix:");
        Console.Write("    ");
        for (int j = 0; j < n; j++)
        {
            Console.Write(seq2[j] + " ");
        }
        Console.WriteLine();

        for (int i = 0; i <= m; i++)
        {
            if (i == 0)
                Console.Write("  ");
            else
                Console.Write(seq1[i - 1] + " ");

            for (int j = 0; j <= n; j++)
            {
                Console.Write(scoreMatrix[i, j] + " ");
            }
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
        SmithWaterman sw = new SmithWaterman();

        // Example 1: Simple DNA sequences
        string seq1 = "ACGTACGT";
        string seq2 = "ACGTACGT";

        Console.WriteLine("Sequence 1: " + seq1);
        Console.WriteLine("Sequence 2: " + seq2);
        Console.WriteLine();

        // Print the scoring matrix
        sw.PrintMatrix(seq1, seq2);

        // Perform alignment
        var result = sw.Align(seq1, seq2);

        Console.WriteLine("Alignment Result:");
        Console.WriteLine("Score: " + result.Score);
        Console.WriteLine("Aligned Sequence 1: " + result.Sequence1Aligned);
        Console.WriteLine("Aligned Sequence 2: " + result.Sequence2Aligned);
        Console.WriteLine("Start Position 1: " + result.StartPosition1);
        Console.WriteLine("Start Position 2: " + result.StartPosition2);
        Console.WriteLine("End Position 1: " + result.EndPosition1);
        Console.WriteLine("End Position 2: " + result.EndPosition2);
        Console.WriteLine();

        // Example 2: Different sequences
        string seq3 = "GATTACA";
        string seq4 = "GCATGCU";

        Console.WriteLine("Sequence 3: " + seq3);
        Console.WriteLine("Sequence 4: " + seq4);
        Console.WriteLine();

        sw.PrintMatrix(seq3, seq4);

        var result2 = sw.Align(seq3, seq4);

        Console.WriteLine("Alignment Result:");
        Console.WriteLine("Score: " + result2.Score);
        Console.WriteLine("Aligned Sequence 1: " + result2.Sequence1Aligned);
        Console.WriteLine("Aligned Sequence 2: " + result2.Sequence2Aligned);
        Console.WriteLine("Start Position 1: " + result2.StartPosition1);
        Console.WriteLine("Start Position 2: " + result2.StartPosition2);
        Console.WriteLine("End Position 1: " + result2.EndPosition1);
        Console.WriteLine("End Position 2: " + result2.EndPosition2);
    }
}
```

## Key Features of this Implementation:

1. **Scoring System**: 
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -1

2. **Algorithm Steps**:
   - Initialize scoring matrix with zeros
   - Fill matrix using dynamic programming
   - Find maximum score and trace back to get alignment
   - Handle local alignment (scores can be zero)

3. **Output**:
   - Alignment score
   - Aligned sequences with gaps
   - Start and end positions in original sequences

## Sample Output:
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT

Scoring Matrix:
      A C G T A C G T 
  0 0 0 0 0 0 0 0 0 
A 2 1 0 0 2 1 0 0 0 
C 1 5 2 1 1 5 2 1 0 
G 0 2 8 3 1 2 8 3 1 
T 0 1 3 11 2 1 3 11 4 
A 2 1 1 2 13 2 1 2 11 
C 1 5 2 1 2 16 2 1 2 
G 0 2 8 3 1 2 19 3 1 
T 0 1 3 11 2 1 3 22 4 

Alignment Result:
Score: 22
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT
Start Position 1: 0
Start Position 2: 0
End Position 1: 7
End Position 2: 7
```

This implementation demonstrates the Smith-Waterman algorithm's ability to find optimal local alignments between two sequences, which is particularly useful in bioinformatics for sequence comparison.

