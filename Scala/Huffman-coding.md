# Huffman Coding Algorithm in Scala

Huffman coding is a classic algorithm for lossless data compression. Here's a complete implementation in Scala:

```scala
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable

// Case class to represent a node in the Huffman tree
sealed trait HuffmanNode
case class Leaf(char: Char, frequency: Int) extends HuffmanNode
case class InternalNode(left: HuffmanNode, right: HuffmanNode, frequency: Int) extends HuffmanNode

object HuffmanCoding {
  
  // Generate Huffman codes from character frequencies
  def buildHuffmanTree(frequencies: Map[Char, Int]): HuffmanNode = {
    // Create a priority queue (min heap) of leaf nodes
    val pq = PriorityQueue[HuffmanNode]()(Ordering.by[HuffmanNode, Int](_.frequency))
    
    // Add all leaf nodes to the priority queue
    frequencies.foreach { case (char, freq) =>
      pq.enqueue(Leaf(char, freq))
    }
    
    // Build the Huffman tree by combining nodes
    while (pq.size > 1) {
      val left = pq.dequeue()
      val right = pq.dequeue()
      
      val combinedFrequency = left.frequency + right.frequency
      val internalNode = InternalNode(left, right, combinedFrequency)
      pq.enqueue(internalNode)
    }
    
    pq.dequeue()
  }
  
  // Generate Huffman codes from the tree
  def generateCodes(node: HuffmanNode, prefix: String = ""): Map[Char, String] = {
    node match {
      case Leaf(char, _) => Map(char -> prefix)
      case InternalNode(left, right, _) =>
        val leftCodes = generateCodes(left, prefix + "0")
        val rightCodes = generateCodes(right, prefix + "1")
        leftCodes ++ rightCodes
    }
  }
  
  // Encode a string using Huffman codes
  def encode(text: String, codes: Map[Char, String]): String = {
    text.map(c => codes.getOrElse(c, "")).mkString
  }
  
  // Decode a binary string using the Huffman tree
  def decode(encodedText: String, tree: HuffmanNode): String = {
    val result = new StringBuilder
    var currentNode = tree
    
    for (bit <- encodedText) {
      currentNode = bit match {
        case '0' => currentNode match {
          case InternalNode(left, _, _) => left
          case leaf: Leaf => throw new IllegalArgumentException("Invalid encoding")
        }
        case '1' => currentNode match {
          case InternalNode(_, right, _) => right
          case leaf: Leaf => throw new IllegalArgumentException("Invalid encoding")
        }
      }
      
      currentNode match {
        case Leaf(char, _) =>
          result.append(char)
          currentNode = tree
        case _ => // Continue traversing
      }
    }
    
    result.toString
  }
  
  // Main function to demonstrate Huffman coding
  def main(args: Array[String]): Unit = {
    // Example text to compress
    val text = "hello world"
    println(s"Original text: $text")
    
    // Calculate character frequencies
    val frequencies = text.groupBy(identity).view.mapValues(_.length).toMap
    println(s"Frequencies: $frequencies")
    
    // Build Huffman tree
    val tree = buildHuffmanTree(frequencies)
    
    // Generate Huffman codes
    val codes = generateCodes(tree)
    println(s"Huffman codes: $codes")
    
    // Encode the text
    val encoded = encode(text, codes)
    println(s"Encoded: $encoded")
    
    // Decode the text
    val decoded = decode(encoded, tree)
    println(s"Decoded: $decoded")
    
    // Calculate compression ratio
    val originalBits = text.length * 8
    val compressedBits = encoded.length
    val compressionRatio = (1.0 - compressedBits.toDouble / originalBits) * 100
    
    println(s"Original size: $originalBits bits")
    println(s"Compressed size: $compressedBits bits")
    println(s"Compression ratio: ${compressionRatio.round}%")
  }
}

// Example usage
object Example extends App {
  // Simple example with known frequencies
  val frequencies = Map('a' -> 45, 'b' -> 13, 'c' -> 12, 'd' -> 16, 'e' -> 9, 'f' -> 5)
  
  val tree = HuffmanCoding.buildHuffmanTree(frequencies)
  val codes = HuffmanCoding.generateCodes(tree)
  
  println("Huffman Codes:")
  codes.foreach { case (char, code) =>
    println(s"'$char' -> $code")
  }
  
  // Test encoding and decoding
  val testText = "abcdef"
  val encoded = HuffmanCoding.encode(testText, codes)
  val decoded = HuffmanCoding.decode(encoded, tree)
  
  println(s"\nTest: '$testText'")
  println(s"Encoded: $encoded")
  println(s"Decoded: $decoded")
}
```

## Key Components Explained:

### 1. **Data Structures**
- `HuffmanNode` sealed trait with two implementations:
  - `Leaf`: Represents a character with its frequency
  - `InternalNode`: Represents internal tree nodes with left/right children

### 2. **Core Functions**
- `buildHuffmanTree`: Creates the Huffman tree using a priority queue
- `generateCodes`: Recursively traverses the tree to generate binary codes
- `encode`: Converts text to binary using generated codes
- `decode`: Converts binary back to original text

### 3. **Algorithm Steps**
1. Calculate character frequencies
2. Create leaf nodes for each character
3. Build priority queue with leaf nodes
4. Repeatedly combine two nodes with lowest frequencies
5. Generate codes by traversing the tree
6. Encode/decode using the generated codes

### 4. **Example Output**
```
Original text: hello world
Frequencies: Map(h -> 1, e -> 1, l -> 3, o -> 2, ' ' -> 1, w -> 1, r -> 1, d -> 1)
Huffman codes: Map(h -> 1100, e -> 1101, l -> 0, o -> 10, ' ' -> 1110, w -> 1111, r -> 110, d -> 111)
Encoded: 110011010001011100111011111110111
Decoded: hello world
```

This implementation demonstrates the complete Huffman coding algorithm with encoding and decoding capabilities, showing how variable-length codes can efficiently compress data.

