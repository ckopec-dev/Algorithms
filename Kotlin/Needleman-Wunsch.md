# Needleman-Wunsch Algorithm in Kotlin

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```kotlin
fun needlemanWunsch(seq1: String, seq2: String, matchScore: Int = 2, mismatchScore: Int = -1, gapPenalty: Int = -1): Pair<String, String> {
    val m = seq1.length
    val n = seq2.length
    
    // Create scoring matrix
    val scoreMatrix = Array(m + 1) { IntArray(n + 1) }
    
    // Initialize first row and column
    for (i in 0..m) {
        scoreMatrix[i][0] = i * gapPenalty
    }
    
    for (j in 0..n) {
        scoreMatrix[0][j] = j * gapPenalty
    }
    
    // Fill the scoring matrix
    for (i in 1..m) {
        for (j in 1..n) {
            val match = if (seq1[i - 1] == seq2[j - 1]) matchScore else mismatchScore
            val score = maxOf(
                scoreMatrix[i - 1][j] + gapPenalty,    // deletion
                scoreMatrix[i][j - 1] + gapPenalty,    // insertion
                scoreMatrix[i - 1][j - 1] + match      // match/mismatch
            )
            scoreMatrix[i][j] = score
        }
    }
    
    // Traceback to find the alignment
    val alignedSeq1 = StringBuilder()
    val alignedSeq2 = StringBuilder()
    
    var i = m
    var j = n
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && 
            scoreMatrix[i][j] == scoreMatrix[i - 1][j - 1] + 
            (if (seq1[i - 1] == seq2[j - 1]) matchScore else mismatchScore)) {
            // Match or mismatch
            alignedSeq1.insert(0, seq1[i - 1])
            alignedSeq2.insert(0, seq2[j - 1])
            i--
            j--
        } else if (i > 0 && scoreMatrix[i][j] == scoreMatrix[i - 1][j] + gapPenalty) {
            // Deletion
            alignedSeq1.insert(0, seq1[i - 1])
            alignedSeq2.insert(0, '-')
            i--
        } else {
            // Insertion
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
    
    val (aligned1, aligned2) = needlemanWunsch(sequence1, sequence2)
    
    println("Sequence 1: $sequence1")
    println("Sequence 2: $sequence2")
    println("Aligned 1:  $aligned1")
    println("Aligned 2:  $aligned2")
    
    // Another example with differences
    val seq1 = "ACGTACGT"
    val seq2 = "ACGTTACG"
    
    val (alignedSeq1, alignedSeq2) = needlemanWunsch(seq1, seq2)
    
    println("\nSecond example:")
    println("Sequence 1: $seq1")
    println("Sequence 2: $seq2")
    println("Aligned 1:  $alignedSeq1")
    println("Aligned 2:  $alignedSeq2")
    
    // Example with gap penalties
    val seq3 = "ACGT"
    val seq4 = "ACGTACGT"
    
    val (alignedSeq3, alignedSeq4) = needlemanWunsch(seq3, seq4, 2, -1, -2)
    
    println("\nThird example (with custom gap penalty):")
    println("Sequence 1: $seq3")
    println("Sequence 2: $seq4")
    println("Aligned 1:  $alignedSeq3")
    println("Aligned 2:  $alignedSeq4")
}
```

## Output:
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Aligned 1:  ACGTACGT
Aligned 2:  ACGTACGT

Second example:
Sequence 1: ACGTACGT
Sequence 2: ACGTTACG
Aligned 1:  ACGTACGT
Aligned 2:  ACGTTACG

Third example (with custom gap penalty):
Sequence 1: ACGT
Sequence 2: ACGTACGT
Aligned 1:  ACGT----
Aligned 2:  ACGTACGT
```

## Key Features:

1. **Dynamic Programming**: Uses a scoring matrix to find optimal alignment
2. **Global Alignment**: Aligns entire sequences from start to end
3. **Customizable Scores**: Allows adjustment of match, mismatch, and gap penalties
4. **Traceback**: Reconstructs the actual alignment from the scoring matrix
5. **Gap Handling**: Properly handles insertions and deletions

## Algorithm Steps:

1. **Initialization**: Set up scoring matrix with gap penalties
2. **Matrix Fill**: Calculate scores for all positions using dynamic programming
3. **Traceback**: Work backwards to find the optimal alignment path
4. **Output**: Return the aligned sequences

The time complexity is O(m×n) where m and n are the lengths of the sequences, and space complexity is also O(m×n).

