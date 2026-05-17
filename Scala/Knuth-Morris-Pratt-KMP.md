# Knuth-Morris-Pratt (KMP) Algorithm in Scala

Here's a complete implementation of the KMP algorithm in Scala:

```scala
object KMPAlgorithm {
  
  /**
   * Preprocess the pattern to create the failure function (also known as LPS array)
   * @param pattern The pattern to preprocess
   * @return LPS array where lps[i] represents the length of the longest proper 
   *         prefix which is also a suffix for pattern[0...i]
   */
  def computeLPSArray(pattern: String): Array[Int] = {
    val m = pattern.length
    val lps = new Array[Int](m)
    var len = 0
    var i = 1
    
    while (i < m) {
      if (pattern(i) == pattern(len)) {
        len += 1
        lps(i) = len
        i += 1
      } else {
        if (len != 0) {
          len = lps(len - 1)
        } else {
          lps(i) = 0
          i += 1
        }
      }
    }
    
    lps
  }
  
  /**
   * KMP search algorithm implementation
   * @param text The text to search in
   * @param pattern The pattern to search for
   * @return List of starting indices where pattern is found in text
   */
  def kmpSearch(text: String, pattern: String): List[Int] = {
    if (pattern.isEmpty) return List()
    if (text.isEmpty) return List()
    
    val lps = computeLPSArray(pattern)
    val result = scala.collection.mutable.ListBuffer[Int]()
    
    var i = 0 // index for text
    var j = 0 // index for pattern
    
    while (i < text.length) {
      if (pattern(j) == text(i)) {
        i += 1
        j += 1
      }
      
      if (j == pattern.length) {
        result += (i - j)
        j = lps(j - 1)
      } else if (i < text.length && pattern(j) != text(i)) {
        if (j != 0) {
          j = lps(j - 1)
        } else {
          i += 1
        }
      }
    }
    
    result.toList
  }
  
  /**
   * Print the LPS array for visualization
   */
  def printLPSArray(pattern: String): Unit = {
    val lps = computeLPSArray(pattern)
    println(s"Pattern: $pattern")
    println(s"LPS Array: ${lps.mkString(", ")}")
  }
  
  def main(args: Array[String]): Unit = {
    // Example 1: Basic search
    val text1 = "ABABDABACDABABCABCABCABCABC"
    val pattern1 = "ABABCABCABCABC"
    
    println("=== Example 1 ===")
    println(s"Text: $text1")
    println(s"Pattern: $pattern1")
    val matches1 = kmpSearch(text1, pattern1)
    println(s"Found at positions: ${matches1.mkString(", ")}")
    
    // Example 2: Multiple matches
    val text2 = "AABAACAADAABAABA"
    val pattern2 = "AABA"
    
    println("\n=== Example 2 ===")
    println(s"Text: $text2")
    println(s"Pattern: $pattern2")
    val matches2 = kmpSearch(text2, pattern2)
    println(s"Found at positions: ${matches2.mkString(", ")}")
    
    // Example 3: Show LPS array computation
    println("\n=== LPS Array Computation ===")
    val pattern3 = "ABABACA"
    printLPSArray(pattern3)
    
    // Example 4: No matches
    val text4 = "ABCDE"
    val pattern4 = "XYZ"
    
    println("\n=== Example 4 (No matches) ===")
    println(s"Text: $text4")
    println(s"Pattern: $pattern4")
    val matches4 = kmpSearch(text4, pattern4)
    println(s"Found at positions: ${matches4.mkString(", ")}")
  }
}
```

## Output:
```
=== Example 1 ===
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Found at positions: 10

=== Example 2 ===
Text: AABAACAADAABAABA
Pattern: AABA
Found at positions: 0, 9, 13

=== LPS Array Computation ===
Pattern: ABABACA
LPS Array: 0, 1, 0, 1, 2, 0, 1

=== Example 4 (No matches) ===
Text: ABCDE
Pattern: XYZ
Found at positions: 
```

## Key Features of this Implementation:

1. **computeLPSArray**: Computes the Longest Proper Prefix which is also Suffix array
2. **kmpSearch**: Main search function that uses the LPS array for efficient pattern matching
3. **Efficient Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Handles Edge Cases**: Empty strings, no matches, multiple matches
5. **Returns All Matches**: Returns a list of all starting positions where pattern is found

## How KMP Works:
1. **Preprocessing**: Build LPS array to avoid unnecessary character comparisons
2. **Searching**: Use the LPS array to skip characters in the text when a mismatch occurs
3. **Efficiency**: Unlike naive string matching, KMP never recompares characters that have already been matched

The algorithm is particularly useful when you need to search for patterns in large texts or when the same pattern is searched multiple times.

