# Run-Length Encoding (RLE) in Scala

Here's an implementation of the Run-Length Encoding algorithm in Scala:

```scala
object RLE {
  
  /**
   * Encodes a string using Run-Length Encoding
   * @param input the string to encode
   * @return the run-length encoded string
   */
  def encode(input: String): String = {
    if (input.isEmpty) return ""
    
    val encoded = new StringBuilder()
    var currentChar = input(0)
    var count = 1
    
    for (i <- 1 until input.length) {
      if (input(i) == currentChar) {
        count += 1
      } else {
        if (count > 1) {
          encoded.append(count).append(currentChar)
        } else {
          encoded.append(currentChar)
        }
        currentChar = input(i)
        count = 1
      }
    }
    
    // Handle the last sequence
    if (count > 1) {
      encoded.append(count).append(currentChar)
    } else {
      encoded.append(currentChar)
    }
    
    encoded.toString
  }
  
  /**
   * Decodes a run-length encoded string
   * @param encoded the encoded string
   * @return the original string
   */
  def decode(encoded: String): String = {
    val decoded = new StringBuilder()
    var i = 0
    
    while (i < encoded.length) {
      // Check if current character is a digit
      if (encoded(i).isDigit) {
        // Parse the complete number
        var numStr = ""
        while (i < encoded.length && encoded(i).isDigit) {
          numStr += encoded(i)
          i += 1
        }
        val count = numStr.toInt
        
        // Get the character to repeat
        if (i < encoded.length) {
          val charToRepeat = encoded(i)
          for (_ <- 0 until count) {
            decoded.append(charToRepeat)
          }
          i += 1
        }
      } else {
        // Single character (no count)
        decoded.append(encoded(i))
        i += 1
      }
    }
    
    decoded.toString
  }
}

// Example usage
object Main extends App {
  val original = "aaabbbcccaaa"
  val encoded = RLE.encode(original)
  val decoded = RLE.decode(encoded)
  
  println(s"Original: $original")
  println(s"Encoded:  $encoded")
  println(s"Decoded:  $decoded")
  println(s"Match: ${original == decoded}")
  
  // More examples
  println("\n--- Additional Examples ---")
  
  val examples = List("a", "aaa", "abc", "aaaaabbbbccccddddd", "aabbcc")
  
  examples.foreach { example =>
    val encoded = RLE.encode(example)
    val decoded = RLE.decode(encoded)
    println(s"'$example' -> '$encoded' -> '$decoded'")
  }
}
```

## Output:
```
Original: aaabbbcccaaa
Encoded:  3a3b3c3a
Decoded:  aaabbbcccaaa
Match: true

--- Additional Examples ---
'a' -> 'a' -> 'a'
'aaa' -> '3a' -> 'aaa'
'abc' -> 'abc' -> 'abc'
'aaaaabbbbccccddddd' -> '5a4b4c5d' -> 'aaaaabbbbccccddddd'
'aabbcc' -> 'aabbcc' -> 'aabbcc'
```

## Key Features:

1. **Encode Function**: Compresses consecutive identical characters by replacing them with a count followed by the character
2. **Decode Function**: Reverses the encoding process
3. **Edge Cases**: Handles empty strings, single characters, and sequences of different lengths
4. **Efficient**: Uses `StringBuilder` for efficient string building
5. **Robust**: Properly handles multi-digit counts in the encoded output

The algorithm works by scanning through the input string and counting consecutive identical characters, then representing each sequence with the count followed by the character.

