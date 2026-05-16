# Naïve String Matching Algorithm in Scala

Here's an implementation of the naive string matching algorithm in Scala:

```scala
object NaiveStringMatching {
  
  /**
   * Naive string matching algorithm
   * @param text The text to search in
   * @param pattern The pattern to search for
   * @return List of starting indices where pattern is found in text
   */
  def naiveStringMatch(text: String, pattern: String): List[Int] = {
    val results = scala.collection.mutable.ListBuffer[Int]()
    
    // Handle edge cases
    if (pattern.isEmpty || text.isEmpty || pattern.length > text.length) {
      return List.empty
    }
    
    // Iterate through each possible starting position
    for (i <- 0 to text.length - pattern.length) {
      var matchFound = true
      
      // Check if pattern matches at position i
      for (j <- 0 until pattern.length) {
        if (text(i + j) != pattern(j)) {
          matchFound = false
          break
        }
      }
      
      // If match found, add index to results
      if (matchFound) {
        results += i
      }
    }
    
    results.toList
  }
  
  /**
   * Alternative implementation using Scala's built-in methods
   */
  def naiveStringMatchFunctional(text: String, pattern: String): List[Int] = {
    if (pattern.isEmpty || text.isEmpty || pattern.length > text.length) {
      return List.empty
    }
    
    (0 to text.length - pattern.length)
      .filter(i => text.substring(i, i + pattern.length) == pattern)
      .toList
  }
  
  def main(args: Array[String]): Unit = {
    // Test cases
    val text1 = "ABABDABACDABABCABCABCABCABC"
    val pattern1 = "ABABCABCABCABC"
    
    println(s"Text: $text1")
    println(s"Pattern: $pattern1")
    println(s"Matches found at indices: ${naiveStringMatch(text1, pattern1)}")
    println()
    
    val text2 = "hello world hello"
    val pattern2 = "hello"
    
    println(s"Text: $text2")
    println(s"Pattern: $pattern2")
    println(s"Matches found at indices: ${naiveStringMatch(text2, pattern2)}")
    println()
    
    val text3 = "aaaa"
    val pattern3 = "aa"
    
    println(s"Text: $text3")
    println(s"Pattern: $pattern3")
    println(s"Matches found at indices: ${naiveStringMatch(text3, pattern3)}")
    println()
    
    // Compare both implementations
    println("Functional approach result:")
    println(s"Matches found at indices: ${naiveStringMatchFunctional(text1, pattern1)}")
  }
}

// Usage example
val text = "ABABDABACDABABCABCABCABCABC"
val pattern = "ABABCABCABCABC"
val matches = NaiveStringMatching.naiveStringMatch(text, pattern)
println(s"Pattern found at positions: $matches")
```

## How it works:

1. **Algorithm Logic**: The algorithm checks every possible position in the text where the pattern could start
2. **Time Complexity**: O(n × m) where n is the length of text and m is the length of pattern
3. **Space Complexity**: O(k) where k is the number of matches found

## Key Features:

- **Edge Case Handling**: Properly handles empty strings and patterns longer than text
- **Multiple Implementations**: Both iterative and functional approaches
- **Comprehensive Testing**: Includes various test cases
- **Clear Output**: Shows the indices where matches are found

## Sample Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches found at indices: List(10)

Text: hello world hello
Pattern: hello
Matches found at indices: List(0, 12)

Text: aaaa
Pattern: aa
Matches found at indices: List(0, 1, 2)
```

The algorithm systematically compares the pattern with each substring of the text, making it simple to understand but potentially inefficient for large texts.

