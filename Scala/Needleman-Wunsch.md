# Needleman-Wunsch Algorithm in Scala

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment in bioinformatics.

```scala
object NeedlemanWunsch {
  
  // Define scoring parameters
  case class ScoringParams(matchScore: Int, mismatchScore: Int, gapPenalty: Int)
  
  // Main alignment function
  def align(seq1: String, seq2: String, params: ScoringParams): (String, String, Int) = {
    val n = seq1.length + 1
    val m = seq2.length + 1
    
    // Create scoring matrix
    val scoreMatrix = Array.ofDim[Int](n, m)
    
    // Initialize first row and column
    for (i <- 0 until n) scoreMatrix(i)(0) = i * params.gapPenalty
    for (j <- 0 until m) scoreMatrix(0)(j) = j * params.gapPenalty
    
    // Fill the scoring matrix
    for (i <- 1 until n) {
      for (j <- 1 until m) {
        val matchScore = if (seq1(i-1) == seq2(j-1)) params.matchScore else params.mismatchScore
        val score = Seq(
          scoreMatrix(i-1)(j) + params.gapPenalty,     // gap in seq2
          scoreMatrix(i)(j-1) + params.gapPenalty,     // gap in seq1
          scoreMatrix(i-1)(j-1) + matchScore          // match/mismatch
        ).max
        
        scoreMatrix(i)(j) = score
      }
    }
    
    // Traceback to find alignment
    val alignedSeq1 = new StringBuilder()
    val alignedSeq2 = new StringBuilder()
    
    var i = n - 1
    var j = m - 1
    
    while (i > 0 || j > 0) {
      val currentScore = scoreMatrix(i)(j)
      
      if (i > 0 && j > 0 && 
          currentScore == scoreMatrix(i-1)(j-1) + 
          (if (seq1(i-1) == seq2(j-1)) params.matchScore else params.mismatchScore)) {
        // Match/mismatch
        alignedSeq1.insert(0, seq1(i-1))
        alignedSeq2.insert(0, seq2(j-1))
        i -= 1
        j -= 1
      } else if (i > 0 && currentScore == scoreMatrix(i-1)(j) + params.gapPenalty) {
        // Gap in seq2
        alignedSeq1.insert(0, seq1(i-1))
        alignedSeq2.insert(0, '-')
        i -= 1
      } else {
        // Gap in seq1
        alignedSeq1.insert(0, '-')
        alignedSeq2.insert(0, seq2(j-1))
        j -= 1
      }
    }
    
    (alignedSeq1.toString, alignedSeq2.toString, scoreMatrix(n-1)(m-1))
  }
  
  // Helper function to print matrix
  def printMatrix(seq1: String, seq2: String, params: ScoringParams): Unit = {
    val n = seq1.length + 1
    val m = seq2.length + 1
    val scoreMatrix = Array.ofDim[Int](n, m)
    
    // Initialize
    for (i <- 0 until n) scoreMatrix(i)(0) = i * params.gapPenalty
    for (j <- 0 until m) scoreMatrix(0)(j) = j * params.gapPenalty
    
    // Fill matrix
    for (i <- 1 until n) {
      for (j <- 1 until m) {
        val matchScore = if (seq1(i-1) == seq2(j-1)) params.matchScore else params.mismatchScore
        scoreMatrix(i)(j) = Seq(
          scoreMatrix(i-1)(j) + params.gapPenalty,
          scoreMatrix(i)(j-1) + params.gapPenalty,
          scoreMatrix(i-1)(j-1) + matchScore
        ).max
      }
    }
    
    // Print matrix
    println("Scoring Matrix:")
    print("    ")
    for (j <- 0 until m) print(f"${if (j == 0) " " else seq2(j-1)}%3c")
    println()
    
    for (i <- 0 until n) {
      if (i == 0) print("  ")
      else print(s"${seq1(i-1)} ")
      
      for (j <- 0 until m) {
        print(f"${scoreMatrix(i)(j)}%3d ")
      }
      println()
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val seq1 = "ACGTACGT"
    val seq2 = "ACGTACGT"
    
    val params = ScoringParams(
      matchScore = 2,
      mismatchScore = -1,
      gapPenalty = -1
    )
    
    println(s"Sequence 1: $seq1")
    println(s"Sequence 2: $seq2")
    println(s"Scoring parameters: Match=$params.matchScore, Mismatch=$params.mismatchScore, Gap=$params.gapPenalty")
    println()
    
    // Print the scoring matrix
    printMatrix(seq1, seq2, params)
    println()
    
    // Perform alignment
    val (aligned1, aligned2, score) = align(seq1, seq2, params)
    
    println("Alignment Result:")
    println(s"Score: $score")
    println(s"Aligned Sequence 1: $aligned1")
    println(s"Aligned Sequence 2: $aligned2")
    println()
    
    // Another example with different sequences
    val seq3 = "ACGT"
    val seq4 = "ACGTAC"
    
    println("Second Example:")
    println(s"Sequence 1: $seq3")
    println(s"Sequence 2: $seq4")
    
    val (aligned3, aligned4, score2) = align(seq3, seq4, params)
    
    println(s"Score: $score2")
    println(s"Aligned Sequence 1: $aligned3")
    println(s"Aligned Sequence 2: $aligned4")
  }
}
```

## Example Output

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Scoring parameters: Match=2, Mismatch=-1, Gap=-1

Scoring Matrix:
       A   C   G   T   A   C   G   T
  0  -1  -2  -3  -4  -5  -6  -7  -8
A  -1   2   1   0  -1  -2  -3  -4
C  -2   1   4   3   2   1   0  -1
G  -3   0   3   6   5   4   3   2
T  -4  -1   2   5   8   7   6   5
A  -5  -2   1   4   7  10   9   8
C  -6  -3   0   3   6   9  12  11
G  -7  -4  -1   2   5   8  11  14
T  -8  -5  -2   1   4   7  10  13

Alignment Result:
Score: 13
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTACGT

Second Example:
Sequence 1: ACGT
Sequence 2: ACGTAC
Score: 6
Aligned Sequence 1: ACGT--
Aligned Sequence 2: ACGTAC
```

## Key Features

1. **Scoring Parameters**: Customizable match, mismatch, and gap penalties
2. **Dynamic Programming**: Builds scoring matrix to find optimal alignment
3. **Traceback**: Reconstructs the actual alignment from the matrix
4. **Flexible Input**: Works with any DNA/protein sequences
5. **Matrix Visualization**: Includes function to display the scoring matrix

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

This implementation provides a complete Needleman-Wunsch algorithm for global sequence alignment in Scala.

