# Levenshtein Distance Algorithm in Scala

The Levenshtein distance is a string metric for measuring the difference between two sequences. It represents the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

## Implementation

```scala
def levenshteinDistance(str1: String, str2: String): Int = {
  val m = str1.length
  val n = str2.length
  
  // Create a matrix to store the distances
  val dp = Array.ofDim[Int](m + 1, n + 1)
  
  // Initialize the first row and column
  for (i <- 0 to m) dp(i)(0) = i
  for (j <- 0 to n) dp(0)(j) = j
  
  // Fill the matrix
  for (i <- 1 to m) {
    for (j <- 1 to n) {
      if (str1(i - 1) == str2(j - 1)) {
        dp(i)(j) = dp(i - 1)(j - 1)  // No operation needed
      } else {
        dp(i)(j) = 1 + Seq(
          dp(i - 1)(j),     // deletion
          dp(i)(j - 1),     // insertion
          dp(i - 1)(j - 1)  // substitution
        ).min
      }
    }
  }
  
  dp(m)(n)
}

// Example usage
object LevenshteinExample extends App {
  val word1 = "kitten"
  val word2 = "sitting"
  
  val distance = levenshteinDistance(word1, word2)
  println(s"Levenshtein distance between '$word1' and '$word2': $distance")
  
  // More examples
  println(s"Distance between 'hello' and 'world': ${levenshteinDistance("hello", "world")}")
  println(s"Distance between 'scala' and 'scala': ${levenshteinDistance("scala", "scala")}")
  println(s"Distance between 'cat' and 'dog': ${levenshteinDistance("cat", "dog")}")
}
```

## Output
```
Levenshtein distance between 'kitten' and 'sitting': 3
Distance between 'hello' and 'world': 4
Distance between 'scala' and 'scala': 0
Distance between 'cat' and 'dog': 3
```

## How it works

1. **Initialization**: Create a matrix where `dp(i)(j)` represents the Levenshtein distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base cases**: 
   - The distance from an empty string to a string of length `j` is `j` (all insertions)
   - The distance from a string of length `i` to an empty string is `i` (all deletions)

3. **Recurrence relation**:
   - If characters match: `dp(i)(j) = dp(i-1)(j-1)`
   - If characters don't match: `dp(i)(j) = 1 + min(dp(i-1)(j), dp(i)(j-1), dp(i-1)(j-1))`

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

## Alternative Functional Implementation

```scala
def levenshteinDistanceFunctional(str1: String, str2: String): Int = {
  def computeDistance(s1: List[Char], s2: List[Char], memo: Map[(List[Char], List[Char]), Int]): Int = {
    memo.getOrElse((s1, s2), {
      val result = (s1, s2) match {
        case (Nil, _) => s2.length
        case (_, Nil) => s1.length
        case (c1 :: r1, c2 :: r2) =>
          if (c1 == c2) computeDistance(r1, r2, memo)
          else 1 + Seq(
            computeDistance(r1, s2, memo),
            computeDistance(s1, r2, memo),
            computeDistance(r1, r2, memo)
          ).min
      }
      memo + ((s1, s2) -> result)
    })
  }
  
  computeDistance(str1.toList, str2.toList, Map.empty)
}
```

This implementation provides a clear demonstration of how the Levenshtein distance algorithm works in Scala, showing both the imperative and functional approaches to solving this classic string similarity problem.

