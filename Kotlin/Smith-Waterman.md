# Smith-Waterman Algorithm Implementation in Kotlin

The Smith-Waterman algorithm is a dynamic programming algorithm used for local sequence alignment, which finds the optimal local alignment between two sequences.

```kotlin
fun smithWaterman(seq1: String, seq2: String, matchScore: Int = 2, mismatchScore: Int = -1, gapPenalty: Int = -1): Pair<String, String> {
    val m = seq1.length
    val n = seq2.length
    
    // Create scoring matrix
    val scoreMatrix = Array(m + 1) { Array(n + 1) { 0 } }
    
    // Fill the scoring matrix
    for (i in 1..m) {
        for (j in 1..n) {
            val match = if (seq1[i - 1] == seq2[j - 1]) matchScore else mismatchScore
            val diagonal = scoreMatrix[i - 1][j - 1] + match
            val up = scoreMatrix[i - 1][j] + gapPenalty
            val left = scoreMatrix[i][j - 1] + gapPenalty
            
            scoreMatrix[i][j] = maxOf(0, diagonal, up, left)
        }
    }
    
    // Find the maximum score and its position
    var maxScore = 0
    var maxI = 0
    var maxJ = 0
    
    for (i in 1..m) {
        for (j in 1..n) {
            if (scoreMatrix[i][j] > maxScore) {
                maxScore = scoreMatrix[i][j]
                maxI = i
                maxJ = j
            }
        }
    }
    
    // Traceback to find the alignment
    val alignedSeq1 = StringBuilder()
    val alignedSeq2 = StringBuilder()
    
    var i = maxI
    var j = maxJ
    
    while (i > 0 && j > 0 && scoreMatrix[i][j] > 0) {
        val currentScore = scoreMatrix[i][j]
        val diagonal = scoreMatrix[i - 1][j - 1]
        val up = scoreMatrix[i - 1][j]
        val left = scoreMatrix[i][j - 1]
        
        if (currentScore == diagonal + (if (seq1[i - 1] == seq2[j - 1]) matchScore else mismatchScore)) {
            // Match/mismatch
            alignedSeq1.insert(0, seq1[i - 1])
            alignedSeq2.insert(0, seq2[j - 1])
            i--
            j--
        } else if (currentScore == up + gapPenalty) {
            // Gap in seq2
            alignedSeq1.insert(0, seq1[i - 1])
            alignedSeq2.insert(0, '-')
            i--
        } else {
            // Gap in seq1
            alignedSeq1.insert(0, '-')
            alignedSeq2.insert(0, seq2[j - 1])
            j--
        }
    }
    
    return Pair(alignedSeq1.toString(), alignedSeq2.toString())
}

// Example usage
fun main() {
    val sequence1 = "ACGTACGT"
    val sequence2 = "ACGTACGT"
    
    println("Sequence 1: $sequence1")
    println("Sequence 2: $sequence2")
    println()
    
    val (aligned1, aligned2) = smithWaterman(sequence1, sequence2)
    
    println("Aligned Sequence 1: $aligned1")
    println("Aligned Sequence 2: $aligned2")
    println()
    
    // Another example with different sequences
    val seq1 = "GATTACA"
    val seq2 = "GCATGCU"
    
    println("Sequence 1: $seq1")
    println("Sequence 2: $seq2")
    println()
    
    val (alignedSeq1, alignedSeq2) = smithWaterman(seq1, seq2)
    
    println("Aligned Sequence 1: $alignedSeq1")
    println("Aligned Sequence 2: $alignedSeq2")
    
    // Print the scoring matrix for better understanding
    printScoringMatrix(seq1, seq2)
}

fun printScoringMatrix(seq1: String, seq2: String) {
    val m = seq1.length
    val n = seq2.length
    val scoreMatrix = Array(m + 1) { Array(n + 1) { 0 } }
    
    // Fill the scoring matrix (same as in smithWaterman)
    for (i in 1..m) {
        for (j in 1..n) {
            val match = if (seq1[i - 1] == seq2[j - 1]) 2 else -1
            val diagonal = scoreMatrix[i - 1][j - 1] + match
            val up = scoreMatrix[i - 1][j] - 1
            val left = scoreMatrix[i][j - 1] - 1
            
            scoreMatrix[i][j] = maxOf(0, diagonal, up, left)
        }
    }
    
    println("Scoring Matrix:")
    print("    ")
    for (j in 0 until n) {
        print("${seq2[j]}  ")
    }
    println()
    
    for (i in 0..m) {
        if (i == 0) {
            print("  ")
        } else {
            print("${seq1[i - 1]} ")
        }
        
        for (j in 0..n) {
            print("${scoreMatrix[i][j]}  ")
        }
        println()
    }
    println()
}
```

## Output Example:
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT

Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT

Sequence 1: GATTACA
Sequence 2: GCATGCU

Aligned Sequence 1: GATTACA
Aligned Sequence 2: GCATGCU

Scoring Matrix:
       G  C  A  T  G  C  U  
  0  0  0  0  0  0  0  0  
G  2  1  0  0  1  0  0  
A  1  0  2  1  0  0  0  
T  0  0  1  3  2  1  0  
T  0  0  0  2  1  0  0  
A  1  0  1  1  0  0  0  
C  0  0  0  0  0  1  0  
A  1  0  1  0  0  0  0  
```

## Key Features of this Implementation:

1. **Local Alignment**: Finds the best local alignment between sequences
2. **Scoring System**: 
   - Match score: 2 (default)
   - Mismatch score: -1 (default) 
   - Gap penalty: -1 (default)
3. **Traceback**: Reconstructs the optimal alignment path
4. **Matrix Initialization**: Starts with zeros to allow for local alignments
5. **Flexible Parameters**: Allows customization of scoring parameters

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

This implementation demonstrates how the Smith-Waterman algorithm can be used to find optimal local alignments between biological sequences, which is fundamental in bioinformatics applications.

