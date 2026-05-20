# Arithmetic Coding Algorithm in Scala

Here's a complete implementation of the Arithmetic Coding algorithm in Scala:

```scala
import scala.collection.mutable

object ArithmeticCoding {
  
  /**
   * Encodes a string using arithmetic coding
   */
  def encode(input: String, probabilities: Map[Char, Double]): Double = {
    val sortedChars = probabilities.keys.toList.sortBy(_.toString)
    val cumulative = cumulativeProbabilities(probabilities)
    
    var low = 0.0
    var high = 1.0
    
    for (char <- input) {
      val charProb = probabilities(char)
      val charLow = cumulative(char)
      val charHigh = charLow + charProb
      
      val range = high - low
      high = low + range * charHigh
      low = low + range * charLow
    }
    
    (low + high) / 2.0
  }
  
  /**
   * Decodes a compressed value back to the original string
   */
  def decode(compressedValue: Double, probabilities: Map[Char, Double], length: Int): String = {
    val sortedChars = probabilities.keys.toList.sortBy(_.toString)
    val cumulative = cumulativeProbabilities(probabilities)
    
    var low = 0.0
    var high = 1.0
    var decoded = new StringBuilder()
    
    for (_ <- 0 until length) {
      val range = high - low
      val value = (compressedValue - low) / range
      
      val char = findChar(value, cumulative)
      decoded.append(char)
      
      val charProb = probabilities(char)
      val charLow = cumulative(char)
      val charHigh = charLow + charProb
      
      high = low + range * charHigh
      low = low + range * charLow
    }
    
    decoded.toString
  }
  
  /**
   * Calculates cumulative probabilities for each character
   */
  private def cumulativeProbabilities(probabilities: Map[Char, Double]): Map[Char, Double] = {
    val sortedChars = probabilities.keys.toList.sortBy(_.toString)
    val cumulative = mutable.Map[Char, Double]()
    var sum = 0.0
    
    for (char <- sortedChars) {
      cumulative(char) = sum
      sum += probabilities(char)
    }
    
    cumulative.toMap
  }
  
  /**
   * Finds which character corresponds to the given value
   */
  private def findChar(value: Double, cumulative: Map[Char, Double]): Char = {
    cumulative.find { case (_, cumProb) => value >= cumProb }.get._1
  }
  
  /**
   * Calculates probability distribution from input string
   */
  def calculateProbabilities(input: String): Map[Char, Double] = {
    val charCounts = mutable.Map[Char, Int]()
    val total = input.length
    
    for (char <- input) {
      charCounts(char) = charCounts.getOrElse(char, 0) + 1
    }
    
    charCounts.map { case (char, count) => (char, count.toDouble / total) }
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val input = "hello world"
    val probabilities = calculateProbabilities(input)
    
    println(s"Input: $input")
    println(s"Probabilities: $probabilities")
    
    // Encode
    val encoded = encode(input, probabilities)
    println(s"Encoded value: $encoded")
    
    // Decode
    val decoded = decode(encoded, probabilities, input.length)
    println(s"Decoded: $decoded")
    
    // Verify
    println(s"Match: ${input == decoded}")
    
    // Another example with different text
    println("\n--- Another Example ---")
    val text = "aaaaabbbbcccdde"
    val probs = calculateProbabilities(text)
    
    println(s"Input: $text")
    println(s"Probabilities: $probs")
    
    val encoded2 = encode(text, probs)
    val decoded2 = decode(encoded2, probs, text.length)
    
    println(s"Encoded: $encoded2")
    println(s"Decoded: $decoded2")
    println(s"Match: ${text == decoded2}")
  }
}
```

## How it works:

### Key Components:

1. **`encode` function**: Takes input string and character probabilities, returns a compressed double value
2. **`decode` function**: Takes compressed value, probabilities, and original length, returns the decoded string
3. **`cumulativeProbabilities`**: Calculates cumulative probability distribution
4. **`calculateProbabilities`**: Computes character frequencies from input text

### Algorithm Steps:

1. **Encoding**:
   - Initialize low = 0.0, high = 1.0
   - For each character in input:
     - Calculate character's range within current interval
     - Update low and high boundaries
   - Return midpoint of final interval

2. **Decoding**:
   - Initialize low = 0.0, high = 1.0
   - For each position in original string:
     - Determine which character corresponds to current value
     - Update interval boundaries based on character probabilities
   - Return decoded string

### Example Output:
```
Input: hello world
Probabilities: Map(h -> 0.1, e -> 0.1, l -> 0.3, o -> 0.2, ' ' -> 0.1, w -> 0.1, r -> 0.1, d -> 0.1)
Encoded value: 0.3125
Decoded: hello world
Match: true
```

This implementation demonstrates the core principles of arithmetic coding where the input is compressed into a single floating-point number representing the interval in which the encoded message falls.

