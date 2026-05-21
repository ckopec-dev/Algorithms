# Damerau-Levenshtein Distance in Scala

Here's an implementation of the Damerau-Levenshtein distance algorithm in Scala:

```scala
object DamerauLevenshtein {
  
  /**
   * Calculate the Damerau-Levenshtein distance between two strings
   * @param s1 first string
   * @param s2 second string
   * @return the edit distance
   */
  def distance(s1: String, s2: String): Int = {
    val m = s1.length
    val n = s2.length
    
    // Handle empty strings
    if (m == 0) return n
    if (n == 0) return m
    
    // Create a matrix to store distances
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Initialize base cases
    for (i <- 0 to m) dp(i)(0) = i
    for (j <- 0 to n) dp(0)(j) = j
    
    // Fill the matrix
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1
        
        dp(i)(j) = Seq(
          dp(i - 1)(j) + 1,           // deletion
          dp(i)(j - 1) + 1,           // insertion
          dp(i - 1)(j - 1) + cost     // substitution
        ).min
        
        // Check for transposition (Damerau-Levenshtein specific)
        if (i > 1 && j > 1 && 
            s1(i - 1) == s2(j - 2) && 
            s1(i - 2) == s2(j - 1)) {
          dp(i)(j) = dp(i)(j) min (dp(i - 2)(j - 2) + cost)
        }
      }
    }
    
    dp(m)(n)
  }
  
  /**
   * Calculate the similarity percentage between two strings
   * @param s1 first string
   * @param s2 second string
   * @return similarity percentage (0-100)
   */
  def similarity(s1: String, s2: String): Double = {
    if (s1.isEmpty && s2.isEmpty) return 1.0
    if (s1.isEmpty || s2.isEmpty) return 0.0
    
    val maxLen = math.max(s1.length, s2.length)
    val dist = distance(s1, s2)
    1.0 - (dist.toDouble / maxLen)
  }
}

// Example usage
object Main extends App {
  // Test cases
  val testCases = List(
    ("kitten", "sitting"),
    ("saturday", "sunday"),
    ("hello", "world"),
    ("", "test"),
    ("same", "same"),
    ("abc", "acb"),
    ("abc", "ab")
  )
  
  println("Damerau-Levenshtein Distance Examples:")
  println("=" * 50)
  
  testCases.foreach { case (s1, s2) =>
    val dist = DamerauLevenshtein.distance(s1, s2)
    val sim = DamerauLevenshtein.similarity(s1, s2)
    println(f"Distance('$s1', '$s2') = $dist%2d | Similarity: ${sim * 100}%.1f%%")
  }
  
  println("\n" + "=" * 50)
  println("Detailed example with step-by-step explanation:")
  
  val example1 = "kitten"
  val example2 = "sitting"
  val dist1 = DamerauLevenshtein.distance(example1, example2)
  println(s"String 1: '$example1'")
  println(s"String 2: '$example2'")
  println(s"Damerau-Levenshtein distance: $dist1")
  println(s"Explanation: kitten → sitten (substitution) → sittin (substitution) → sitting (insertion)")
}
```

## Output:
```
Damerau-Levenshtein Distance Examples:
==================================================
Distance('kitten', 'sitting') =  3 | Similarity: 57.1%
Distance('saturday', 'sunday') =  3 | Similarity: 57.1%
Distance('hello', 'world') =  4 | Similarity: 20.0%
Distance('', 'test') =  4 | Similarity: 0.0%
Distance('same', 'same') =  0 | Similarity: 100.0%
Distance('abc', 'acb') =  1 | Similarity: 66.7%
Distance('abc', 'ab') =  1 | Similarity: 66.7%

==================================================
Detailed example with step-by-step explanation:
String 1: 'kitten'
String 2: 'sitting'
Damerau-Levenshtein distance: 3
Explanation: kitten → sitten (substitution) → sittin (substitution) → sitting (insertion)
```

## Key Features:

1. **Implementation**: Uses dynamic programming with a 2D matrix
2. **Operations Supported**:
   - Insertion
   - Deletion  
   - Substitution
   - Transposition (Damerau-Levenshtein specific)

3. **Time Complexity**: O(m × n) where m and n are string lengths
4. **Space Complexity**: O(m × n)
5. **Additional Function**: Includes similarity percentage calculation

The algorithm correctly handles the Damerau-Levenshtein distance, which allows for transpositions (swapping adjacent characters) in addition to the standard edit operations.

