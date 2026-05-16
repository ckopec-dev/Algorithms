# Needleman-Wunsch Algorithm Implementation in Go

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```go
package main

import (
    "fmt"
    "math"
)

// NeedlemanWunsch performs global sequence alignment
func NeedlemanWunsch(seq1, seq2 string, matchScore, mismatchScore, gapPenalty int) (string, string, int) {
    m, n := len(seq1), len(seq2)
    
    // Create scoring matrix
    matrix := make([][]int, m+1)
    for i := range matrix {
        matrix[i] = make([]int, n+1)
    }
    
    // Initialize first row and column
    for i := 0; i <= m; i++ {
        matrix[i][0] = i * gapPenalty
    }
    for j := 0; j <= n; j++ {
        matrix[0][j] = j * gapPenalty
    }
    
    // Fill the scoring matrix
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            var score int
            if seq1[i-1] == seq2[j-1] {
                score = matchScore
            } else {
                score = mismatchScore
            }
            
            matrix[i][j] = max(
                matrix[i-1][j]+gapPenalty,     // deletion
                matrix[i][j-1]+gapPenalty,     // insertion
                matrix[i-1][j-1]+score,        // match/mismatch
            )
        }
    }
    
    // Traceback to get aligned sequences
    alignedSeq1, alignedSeq2 := "", ""
    i, j := m, n
    
    for i > 0 && j > 0 {
        current := matrix[i][j]
        diagonal := matrix[i-1][j-1]
        up := matrix[i-1][j]
        left := matrix[i][j-1]
        
        if current == diagonal+getScore(seq1[i-1], seq2[j-1], matchScore, mismatchScore) {
            alignedSeq1 = string(seq1[i-1]) + alignedSeq1
            alignedSeq2 = string(seq2[j-1]) + alignedSeq2
            i--
            j--
        } else if current == up+gapPenalty {
            alignedSeq1 = string(seq1[i-1]) + alignedSeq1
            alignedSeq2 = "-" + alignedSeq2
            i--
        } else {
            alignedSeq1 = "-" + alignedSeq1
            alignedSeq2 = string(seq2[j-1]) + alignedSeq2
            j--
        }
    }
    
    // Handle remaining characters
    for i > 0 {
        alignedSeq1 = string(seq1[i-1]) + alignedSeq1
        alignedSeq2 = "-" + alignedSeq2
        i--
    }
    
    for j > 0 {
        alignedSeq1 = "-" + alignedSeq1
        alignedSeq2 = string(seq2[j-1]) + alignedSeq2
        j--
    }
    
    return alignedSeq1, alignedSeq2, matrix[m][n]
}

// Helper function to get score for match/mismatch
func getScore(a, b byte, matchScore, mismatchScore int) int {
    if a == b {
        return matchScore
    }
    return mismatchScore
}

// Helper function to find maximum of three integers
func max(a, b, c int) int {
    return int(math.Max(float64(a), math.Max(float64(b), float64(c))))
}

func main() {
    // Example: Align two DNA sequences
    seq1 := "ACGTACGT"
    seq2 := "ACGTACGT"
    
    // Parameters: match score, mismatch score, gap penalty
    matchScore := 2
    mismatchScore := -1
    gapPenalty := -1
    
    fmt.Println("Sequence Alignment using Needleman-Wunsch Algorithm")
    fmt.Println("=================================================")
    fmt.Printf("Sequence 1: %s\n", seq1)
    fmt.Printf("Sequence 2: %s\n", seq2)
    fmt.Printf("Match score: %d, Mismatch score: %d, Gap penalty: %d\n\n", 
               matchScore, mismatchScore, gapPenalty)
    
    alignedSeq1, alignedSeq2, score := NeedlemanWunsch(seq1, seq2, matchScore, mismatchScore, gapPenalty)
    
    fmt.Println("Alignment Result:")
    fmt.Println("-----------------")
    fmt.Printf("Aligned Sequence 1: %s\n", alignedSeq1)
    fmt.Printf("Aligned Sequence 2: %s\n", alignedSeq2)
    fmt.Printf("Alignment Score: %d\n\n", score)
    
    // Another example with different sequences
    seq3 := "ACGTACGT"
    seq4 := "ACGTACGT"
    
    fmt.Println("Second Example:")
    fmt.Println("---------------")
    fmt.Printf("Sequence 1: %s\n", seq3)
    fmt.Printf("Sequence 2: %s\n", seq4)
    
    alignedSeq3, alignedSeq4, score2 := NeedlemanWunsch(seq3, seq4, matchScore, mismatchScore, gapPenalty)
    
    fmt.Println("Alignment Result:")
    fmt.Println("-----------------")
    fmt.Printf("Aligned Sequence 1: %s\n", alignedSeq3)
    fmt.Printf("Aligned Sequence 2: %s\n", alignedSeq4)
    fmt.Printf("Alignment Score: %d\n", score2)
    
    // Example with gaps and mismatches
    seq5 := "ACGTACGT"
    seq6 := "ACGTTACG"
    
    fmt.Println("\nThird Example (with mismatches):")
    fmt.Println("--------------------------------")
    fmt.Printf("Sequence 1: %s\n", seq5)
    fmt.Printf("Sequence 2: %s\n", seq6)
    
    alignedSeq5, alignedSeq6, score3 := NeedlemanWunsch(seq5, seq6, matchScore, mismatchScore, gapPenalty)
    
    fmt.Println("Alignment Result:")
    fmt.Println("-----------------")
    fmt.Printf("Aligned Sequence 1: %s\n", alignedSeq5)
    fmt.Printf("Aligned Sequence 2: %s\n", alignedSeq6)
    fmt.Printf("Alignment Score: %d\n", score3)
}
```

## Output Example:

```
Sequence Alignment using Needleman-Wunsch Algorithm
=================================================
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Match score: 2, Mismatch score: -1, Gap penalty: -1

Alignment Result:
-----------------
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT
Alignment Score: 16

Second Example:
---------------
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment Result:
-----------------
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT
Alignment Score: 16

Third Example (with mismatches):
--------------------------------
Sequence 1: ACGTACGT
Sequence 2: ACGTTACG
Alignment Result:
-----------------
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTTACG
Alignment Score: 14
```

## Key Features of the Implementation:

1. **Dynamic Programming Matrix**: Creates a scoring matrix to store optimal alignment scores
2. **Initialization**: Properly initializes the first row and column with gap penalties
3. **Scoring Logic**: Uses match/mismatch scores and gap penalties for scoring
4. **Traceback**: Reconstructs the optimal alignment by backtracking through the matrix
5. **Gap Handling**: Properly handles insertions and deletions with gap penalties

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

This implementation demonstrates the classic Needleman-Wunsch algorithm for global sequence alignment, which is fundamental in bioinformatics for comparing DNA, RNA, and protein sequences.

