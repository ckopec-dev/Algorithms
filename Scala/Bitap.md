# Bitap Algorithm Implementation in Scala

The Bitap algorithm is a fast string searching algorithm that uses bitwise operations to find occurrences of a pattern within a text.

```scala
object BitapAlgorithm {
  
  /**
   * Implements the Bitap algorithm for string searching
   * @param text The text to search in
   * @param pattern The pattern to search for
   * @return The position of the first occurrence of pattern in text, or -1 if not found
   */
  def bitapSearch(text: String, pattern: String): Int = {
    if (pattern.isEmpty) return 0
    if (text.isEmpty) return -1
    
    val patternLength = pattern.length
    val textLength = text.length
    
    // Create the alphabet set (all unique characters in pattern)
    val alphabet = pattern.toSet
    val alphabetSize = alphabet.size
    
    // Create a bit array for each character in the alphabet
    val bitmasks = new scala.collection.mutable.HashMap[Char, Int]()
    
    // Initialize bitmasks for all characters in alphabet
    alphabet.foreach { char =>
      bitmasks(char) = 0
    }
    
    // Set the bitmasks for pattern characters
    for (i <- pattern.length - 1 downTo 0) {
      val char = pattern(i)
      bitmasks(char) = bitmasks(char) | (1 << i)
    }
    
    // Initialize the bitap state
    var state = (1 << patternLength) - 1
    
    // Search through the text
    for (i <- 0 until textLength) {
      val char = text(i)
      
      // Update the state using bitwise operations
      if (alphabet.contains(char)) {
        state = (state >> 1) | bitmasks(char)
      } else {
        state = (state >> 1) | 0x7FFFFFFF // Set all bits to 1 for non-pattern characters
      }
      
      // Check if we found a match
      if ((state & (1 << (patternLength - 1))) == 0) {
        return i - patternLength + 1
      }
    }
    
    -1 // No match found
  }
  
  /**
   * Alternative implementation with error tolerance (k-errors)
   */
  def bitapSearchWithErrors(text: String, pattern: String, maxErrors: Int): Int = {
    if (pattern.isEmpty) return 0
    if (text.isEmpty) return -1
    
    val patternLength = pattern.length
    val textLength = text.length
    
    // Create bitmasks for each character
    val bitmasks = new scala.collection.mutable.HashMap[Char, Int]()
    val alphabet = pattern.toSet
    
    // Initialize bitmasks
    alphabet.foreach { char =>
      bitmasks(char) = 0
    }
    
    // Set bitmasks for pattern
    for (i <- pattern.length - 1 downTo 0) {
      val char = pattern(i)
      bitmasks(char) = bitmasks(char) | (1 << i)
    }
    
    // Initialize states for error levels
    val states = Array.fill(maxErrors + 1)(0)
    
    // Initialize first state
    for (i <- 0 to maxErrors) {
      states(i) = (1 << i) - 1
    }
    
    // Search through text
    for (i <- 0 until textLength) {
      val char = text(i)
      val newStates = Array.fill(maxErrors + 1)(0)
      
      for (error <- 0 to maxErrors) {
        if (alphabet.contains(char)) {
          val mask = bitmasks(char)
          if (error == 0) {
            newStates(error) = (states(error) >> 1) | mask
          } else {
            newStates(error) = (states(error) >> 1) | mask | states(error - 1)
          }
        } else {
          if (error == 0) {
            newStates(error) = (states(error) >> 1) | 0x7FFFFFFF
          } else {
            newStates(error) = (states(error) >> 1) | 0x7FFFFFFF | states(error - 1)
          }
        }
      }
      
      states.indices.foreach { error =>
        states(error) = newStates(error)
      }
      
      // Check for match
      if ((states(maxErrors) & (1 << (patternLength - 1))) == 0) {
        return i - patternLength + 1
      }
    }
    
    -1
  }
  
  def main(args: Array[String]): Unit = {
    // Test cases
    val text = "The quick brown fox jumps over the lazy dog"
    val pattern = "brown"
    
    println(s"Text: $text")
    println(s"Pattern: $pattern")
    
    val result = bitapSearch(text, pattern)
    println(s"Match found at position: $result")
    
    // Test with non-existent pattern
    val pattern2 = "cat"
    val result2 = bitapSearch(text, pattern2)
    println(s"Pattern '$pattern2' found at position: $result2")
    
    // Test with error tolerance
    val text2 = "kitten"
    val pattern3 = "sitting"
    val result3 = bitapSearchWithErrors(text2, pattern3, 3)
    println(s"Text: $text2")
    println(s"Pattern: $pattern3")
    println(s"Match with up to 3 errors found at position: $result3")
  }
}

// Usage example
object BitapExample extends App {
  val text = "ABABDABACDABABCABCABCABCABC"
  val pattern = "ABABCABCABCABC"
  
  val position = BitapAlgorithm.bitapSearch(text, pattern)
  
  if (position >= 0) {
    println(s"Pattern found at position $position")
    println(s"Text: $text")
    println(s"Pattern: $pattern")
    println(s"Match: ${text.substring(position, position + pattern.length)}")
  } else {
    println("Pattern not found")
  }
}
```

## Key Features of this Implementation:

1. **Basic Bitap Search**: Uses bitwise operations to efficiently search for patterns
2. **Error Tolerance**: Extended version that can handle up to k errors
3. **Optimized Bit Operations**: Uses bit shifting and OR operations for fast matching
4. **Memory Efficient**: Only stores necessary bitmasks and states
5. **Multiple Test Cases**: Demonstrates various usage scenarios

## How it Works:

1. **Bitmask Creation**: Each character in the pattern gets a bitmask representing its positions
2. **State Tracking**: Maintains a state that represents potential matches
3. **Bitwise Updates**: Updates the state using bit operations as characters are processed
4. **Match Detection**: Checks if the final bit is zero to indicate a complete match

The algorithm has a time complexity of O(n × m) where n is the text length and m is the pattern length, but with good practical performance due to bitwise optimizations.

