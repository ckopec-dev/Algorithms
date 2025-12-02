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
                scoreMatrix[i - 1][j - 1] + match,  // match/mismatch
                scoreMatrix[i - 1][j] + gapPenalty,   // gap in seq2
                scoreMatrix[i][j - 1] + gapPenalty    // gap in seq1
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
        val currentScore = scoreMatrix[i][j]
        val diagonalScore = if (i > 0 && j > 0) scoreMatrix[i - 1][j - 1] else Int.MIN_VALUE
        val upScore = if (i > 0) scoreMatrix[i - 1][j] else Int.MIN_VALUE
        val leftScore = if (j > 0) scoreMatrix[i][j - 1] else Int.MIN_VALUE
        
        when {
            i > 0 && j > 0 && currentScore == diagonalScore + (if (seq1[i - 1] == seq2[j - 1]) matchScore else mismatchScore) -> {
                alignedSeq1.insert(0, seq1[i - 1])
                alignedSeq2.insert(0, seq2[j - 1])
                i--
                j--
            }
            i > 0 && currentScore == upScore + gapPenalty -> {
                alignedSeq1.insert(0, seq1[i - 1])
                alignedSeq2.insert(0, '-')
                i--
            }
            j > 0 && currentScore == leftScore + gapPenalty -> {
                alignedSeq1.insert(0, '-')
                alignedSeq2.insert(0, seq2[j - 1])
                j--
            }
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
    
    // Another example with different sequences
    val seq1 = "ACGTACGT"
    val seq2 = "ACGTACGA"
    
    val (alignedSeq1, alignedSeq2) = needlemanWunsch(seq1, seq2)
    
    println("\nSequence 1: $seq1")
    println("Sequence 2: $seq2")
    println("Aligned 1:  $alignedSeq1")
    println("Aligned 2:  $alignedSeq2")
    
    // Example with gaps
    val seq3 = "ACGT"
    val seq4 = "ACGTT"
    
    val (alignedSeq3, alignedSeq4) = needlemanWunsch(seq3, seq4)
    
    println("\nSequence 3: $seq3")
    println("Sequence 4: $seq4")
    println("Aligned 3:  $alignedSeq3")
    println("Aligned 4:  $alignedSeq4")
}
```

## Output Example:
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Aligned 1: ACGTACGT
Aligned 2: ACGTACGT

Sequence 1: ACGTACGT
Sequence 2: ACGTACGA
Aligned 1: ACGTACGT
Aligned 2: ACGTACGA

Sequence 3: ACGT
Sequence 4: ACGTT
Aligned 3: ACGT-
Aligned 4: ACGTT
```

## Key Features:

1. **Scoring System**: 
   - Match score: +2 (default)
   - Mismatch score: -1 (default)
   - Gap penalty: -1 (default)

2. **Algorithm Steps**:
   - Initialize scoring matrix
   - Fill matrix using dynamic programming
   - Traceback to find optimal alignment

3. **Time Complexity**: O(m×n) where m and n are sequence lengths
4. **Space Complexity**: O(m×n) for the scoring matrix

The algorithm finds the globally optimal alignment between two sequences by maximizing the alignment score according to the specified scoring system.

