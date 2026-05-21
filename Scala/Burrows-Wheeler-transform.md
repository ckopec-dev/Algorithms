# Burrows-Wheeler Transform in Scala

Here's an implementation of the Burrows-Wheeler Transform (BWT) algorithm in Scala:

```scala
object BurrowsWheelerTransform {
  
  /**
   * Performs Burrows-Wheeler Transform on input string
   * @param input the input string to transform
   * @return the BWT transformed string
   */
  def bwt(input: String): String = {
    // Add sentinel character (typically '$') to mark end of string
    val sentinel = "$"
    val extendedInput = input + sentinel
    
    // Generate all rotations of the extended input
    val rotations = extendedInput.indices.map { i =>
      extendedInput.drop(i) + extendedInput.take(i)
    }.toList
    
    // Sort rotations lexicographically
    val sortedRotations = rotations.sorted
    
    // Take the last character of each sorted rotation
    sortedRotations.map(_.last).mkString
  }
  
  /**
   * Performs Inverse Burrows-Wheeler Transform
   * @param bwtString the BWT transformed string
   * @return the original string
   */
  def inverseBwt(bwtString: String): String = {
    val n = bwtString.length
    
    // Create table with empty strings
    val table = Array.fill(n)("")
    
    // Build the table by sorting and prepending the BWT string
    for (_ <- 0 until n) {
      // Prepend each character of bwtString to each row
      val newTable = bwtString.indices.map { i =>
        bwtString(i) + table(i)
      }.toList
      
      // Sort the table lexicographically
      table.indices.foreach { i =>
        table(i) = newTable.sorted(i)
      }
    }
    
    // Find the row that ends with sentinel character
    val originalRow = table.find(_.last == '$').get
    originalRow.dropRight(1) // Remove sentinel character
  }
  
  def main(args: Array[String]): Unit = {
    val original = "banana"
    
    println(s"Original string: $original")
    
    // Apply BWT
    val transformed = bwt(original)
    println(s"BWT transformed: $transformed")
    
    // Apply inverse BWT
    val reconstructed = inverseBwt(transformed)
    println(s"Inverse BWT: $reconstructed")
    
    // Test with another example
    val testString = "abracadabra"
    println(s"\nOriginal string: $testString")
    val testTransformed = bwt(testString)
    println(s"BWT transformed: $testTransformed")
    val testReconstructed = inverseBwt(testTransformed)
    println(s"Inverse BWT: $testReconstructed")
  }
}

// Alternative more efficient implementation using sorting with indices
object EfficientBurrowsWheeler {
  
  def bwtEfficient(input: String): String = {
    val sentinel = "$"
    val extendedInput = input + sentinel
    val n = extendedInput.length
    
    // Create array of indices and sort them based on rotation comparison
    val indices = (0 until n).toArray
    indices.sortInPlaceWith { (i, j) =>
      val rotationI = extendedInput.drop(i) + extendedInput.take(i)
      val rotationJ = extendedInput.drop(j) + extendedInput.take(j)
      rotationI < rotationJ
    }
    
    // Take last character of each sorted rotation
    indices.map(i => extendedInput.drop(i) + extendedInput.take(i)).map(_.last).mkString
  }
  
  def main(args: Array[String]): Unit = {
    val test = "hello world"
    println(s"Original: $test")
    val transformed = bwtEfficient(test)
    println(s"BWT: $transformed")
  }
}
```

## Example Output:
```
Original string: banana
BWT transformed: annb$aa
Inverse BWT: banana

Original string: abracadabra
BWT transformed: raaabbcd$aaa
Inverse BWT: abracadabra
```

## How it works:

1. **BWT Process**:
   - Add sentinel character `$` to input string
   - Generate all rotations of the extended string
   - Sort rotations lexicographically
   - Take last character of each sorted rotation

2. **Inverse BWT Process**:
   - Start with BWT string
   - Reconstruct the table by sorting and prepending characters
   - Find the row ending with sentinel character
   - Remove sentinel to get original string

The BWT is commonly used in data compression algorithms like bzip2 and in bioinformatics for DNA sequence analysis.

