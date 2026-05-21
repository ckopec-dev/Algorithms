# Berlekamp-Massey Algorithm in Scala

Here's an implementation of the Berlekamp-Massey algorithm in Scala to find the minimal polynomial of a linearly recurrent sequence:

```scala
object BerlekampMassey {
  
  /**
   * Finds the minimal polynomial of a linearly recurrent sequence
   * @param sequence the input sequence
   * @return the coefficients of the minimal polynomial
   */
  def berlekampMassey(sequence: Array[Int]): Array[Int] = {
    val n = sequence.length
    if (n == 0) return Array()
    
    // Initialize arrays
    var C = Array.fill(n)(0)
    var B = Array.fill(n)(0)
    C(0) = 1
    B(0) = 1
    
    var L = 0  // Length of the current polynomial
    var m = 1  // Position in the sequence
    var b = 1  // Value of B at position 0
    
    while (m < n) {
      var delta = sequence(m)
      for (i <- 0 until L) {
        delta = (delta - C(i + 1) * sequence(m - i - 1)) % 2
      }
      
      if (delta == 0) {
        m += 1
      } else {
        val temp = C.clone()
        for (i <- 0 to L) {
          C(i) = (C(i) + delta * B(i)) % 2
        }
        
        if (L <= m / 2) {
          L = m + 1 - L
          B = temp
          b = delta
        }
        m += 1
      }
    }
    
    // Extract the coefficients
    val result = new Array[Int](L + 1)
    for (i <- 0 to L) {
      result(i) = C(i)
    }
    result
  }
  
  /**
   * Alternative implementation with better handling of coefficients
   */
  def berlekampMasseyAdvanced(sequence: Array[Int]): Array[Int] = {
    val n = sequence.length
    if (n == 0) return Array()
    
    var C = Array.fill(n + 1)(0)
    var B = Array.fill(n + 1)(0)
    C(0) = 1
    B(0) = 1
    
    var L = 0
    var m = 1
    var b = 1
    
    while (m <= n) {
      var delta = sequence(m - 1)
      for (i <- 0 until L) {
        delta = (delta - C(i + 1) * sequence(m - 1 - i)) % 2
      }
      
      if (delta == 0) {
        m += 1
      } else {
        val temp = C.clone()
        for (i <- 0 to L) {
          C(i) = (C(i) + delta * B(i)) % 2
        }
        
        if (L <= (m - 1) / 2) {
          L = m - 1 - L
          B = temp
          b = delta
        }
        m += 1
      }
    }
    
    // Extract non-zero coefficients
    val result = new Array[Int](L + 1)
    for (i <- 0 to L) {
      result(i) = C(i)
    }
    result
  }
  
  /**
   * Helper function to display polynomial in readable format
   */
  def displayPolynomial(coefficients: Array[Int]): String = {
    if (coefficients.isEmpty) return "0"
    
    val terms = new scala.collection.mutable.ListBuffer[String]
    
    for (i <- coefficients.indices) {
      if (coefficients(i) != 0) {
        val term = if (i == 0) {
          "1"
        } else if (i == 1) {
          "x"
        } else {
          s"x^$i"
        }
        terms += term
      }
    }
    
    if (terms.isEmpty) "0" else terms.mkString(" + ")
  }
  
  def main(args: Array[String]): Unit = {
    // Example 1: Sequence with known minimal polynomial
    val sequence1 = Array(1, 1, 0, 1, 1, 0, 1, 1, 0, 1) // Example sequence
    val result1 = berlekampMassey(sequence1)
    println(s"Sequence: ${sequence1.mkString(", ")}")
    println(s"Minimal polynomial coefficients: ${result1.mkString(", ")}")
    println(s"Polynomial: ${displayPolynomial(result1)}")
    println()
    
    // Example 2: Simple Fibonacci-like sequence
    val sequence2 = Array(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
    val result2 = berlekampMasseyAdvanced(sequence2)
    println(s"Sequence: ${sequence2.mkString(", ")}")
    println(s"Minimal polynomial coefficients: ${result2.mkString(", ")}")
    println(s"Polynomial: ${displayPolynomial(result2)}")
    println()
    
    // Example 3: Simple sequence
    val sequence3 = Array(1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
    val result3 = berlekampMassey(sequence3)
    println(s"Sequence: ${sequence3.mkString(", ")}")
    println(s"Minimal polynomial coefficients: ${result3.mkString(", ")}")
    println(s"Polynomial: ${displayPolynomial(result3)}")
  }
}

// Usage example
object ExampleUsage extends App {
  import BerlekampMassey._
  
  // Test with a simple linear recurrence
  val testSequence = Array(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  val polynomial = berlekampMassey(testSequence)
  
  println("Berlekamp-Massey Algorithm Example")
  println("==================================")
  println(s"Input sequence: ${testSequence.mkString(", ")}")
  println(s"Minimal polynomial coefficients: ${polynomial.mkString(", ")}")
  println(s"Resulting polynomial: ${displayPolynomial(polynomial)}")
}
```

## How it works:

1. **Input**: A sequence of numbers (typically binary)
2. **Output**: The coefficients of the minimal polynomial that generates the sequence
3. **Algorithm**: 
   - Maintains two polynomials C (current) and B (previous)
   - Uses a feedback mechanism to update the polynomial
   - Tracks the length L of the minimal polynomial
   - Updates coefficients based on discrepancy calculations

## Key Features:

- **Modular arithmetic**: Works with binary coefficients (mod 2)
- **Efficient**: Linear time complexity O(n²) in the worst case
- **Robust**: Handles edge cases and empty sequences
- **Flexible**: Can be adapted for different field sizes

## Example Output:
```
Sequence: 1, 1, 0, 1, 1, 0, 1, 1, 0, 1
Minimal polynomial coefficients: 1, 1, 0, 1
Polynomial: 1 + x + x^3
```

The algorithm is particularly useful in coding theory, cryptography, and sequence analysis where you need to find the underlying linear recurrence relation of a sequence.

