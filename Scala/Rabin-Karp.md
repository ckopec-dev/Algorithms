# Rabin-Karp Algorithm in Scala

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. Here's an implementation in Scala:

```scala
object RabinKarp {
  
  def search(pattern: String, text: String): List[Int] = {
    val prime = 101 // A prime number for hashing
    val patternLength = pattern.length
    val textLength = text.length
    
    // Handle edge cases
    if (patternLength == 0 || textLength == 0 || patternLength > textLength) {
      return List()
    }
    
    // Calculate hash value of pattern and first window of text
    var patternHash = 0L
    var textHash = 0L
    var power = 1L
    
    // Calculate highest power of prime for the pattern
    for (i <- 0 until patternLength - 1) {
      power = (power * prime) % Int.MaxValue
    }
    
    // Calculate hash values
    for (i <- 0 until patternLength) {
      patternHash = (patternHash * prime + pattern(i)) % Int.MaxValue
      textHash = (textHash * prime + text(i)) % Int.MaxValue
    }
    
    val results = scala.collection.mutable.ListBuffer[Int]()
    
    // Slide the pattern over text one by one
    for (i <- 0 to textLength - patternLength) {
      // Check if hash values match
      if (patternHash == textHash) {
        // Check character by character for actual match
        var j = 0
        while (j < patternLength && pattern(j) == text(i + j)) {
          j += 1
        }
        if (j == patternLength) {
          results += i
        }
      }
      
      // Calculate hash value for next window of text
      if (i < textLength - patternLength) {
        textHash = (prime * (textHash - text(i) * power) + text(i + patternLength)) % Int.MaxValue
        if (textHash < 0) {
          textHash += Int.MaxValue
        }
      }
    }
    
    results.toList
  }
  
  def main(args: Array[String]): Unit = {
    val text = "ABABDABACDABABCABCABC"
    val pattern = "ABABCABCABC"
    
    println(s"Text: $text")
    println(s"Pattern: $pattern")
    
    val matches = search(pattern, text)
    
    if (matches.isEmpty) {
      println("Pattern not found in text")
    } else {
      println(s"Pattern found at positions: ${matches.mkString(", ")}")
    }
    
    // Additional test cases
    println("\n--- Additional Test Cases ---")
    
    val testCases = List(
      ("abc", "abcabcabc"),
      ("test", "this is a test string for testing"),
      ("xyz", "abcdef"),
      ("", "hello world"),
      ("hello", "")
    )
    
    testCases.foreach { case (pattern, text) =>
      val result = search(pattern, text)
      println(s"Pattern '$pattern' in '$text': ${if (result.isEmpty) "Not found" else s"Found at positions ${result.mkString(", ")}"}")
    }
  }
}
```

## How it works:

1. **Hash Calculation**: The algorithm calculates hash values for the pattern and for each window of text of the same length as the pattern.

2. **Window Sliding**: It slides the pattern over the text one character at a time.

3. **Hash Comparison**: When hash values match, it performs character-by-character comparison to confirm the actual match.

4. **Rolling Hash**: The hash value for the next window is calculated efficiently using the previous hash value.

## Time Complexity:
- **Average case**: O(n + m) where n is text length and m is pattern length
- **Worst case**: O(n × m) when many hash collisions occur

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Sample Output:
```
Text: ABABDABACDABABCABCABC
Pattern: ABABCABCABC
Pattern found at positions: 10

--- Additional Test Cases ---
Pattern 'abc' in 'abcabcabc': Found at positions 0, 3, 6
Pattern 'test' in 'this is a test string for testing': Found at positions 10, 26
Pattern 'xyz' in 'abcdef': Not found
Pattern '' in 'hello world': Not found
Pattern 'hello' in '': Not found
```

