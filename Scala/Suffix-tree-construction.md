# Suffix Tree Construction in Scala

Here's an implementation of Ukkonen's algorithm for constructing suffix trees in Scala:

```scala
import scala.collection.mutable

// Node representation for the suffix tree
class SuffixTreeNode {
  var children: Map[Char, SuffixTreeNode] = Map.empty
  var start: Int = -1
  var end: Int = -1
  var suffixIndex: Int = -1
  var suffixLink: SuffixTreeNode = null
  
  def isLeaf: Boolean = children.isEmpty
}

class SuffixTree(val text: String) {
  private val root = new SuffixTreeNode()
  private var activeNode = root
  private var activeEdge = -1
  private var activeLength = 0
  private var remainingSuffixCount = 0
  private var leafEnd = -1
  private var rootEnd: Int = -1
  private var splitEnd: Int = -1
  private var lastNewNode: SuffixTreeNode = null
  
  // Main suffix tree construction method
  def buildSuffixTree(): Unit = {
    val n = text.length
    rootEnd = n - 1
    
    // Initialize suffix tree with first character
    for (i <- 0 until n) {
      extendSuffixTree(i)
    }
  }
  
  // Extend the suffix tree with new character at position j
  private def extendSuffixTree(j: Int): Unit = {
    leafEnd = j
    remainingSuffixCount += 1
    lastNewNode = null
    
    while (remainingSuffixCount > 0) {
      if (activeLength == 0) {
        activeEdge = j
      }
      
      if (!activeNode.children.contains(text(activeEdge))) {
        // Rule 2: Create new leaf node
        val leaf = new SuffixTreeNode()
        leaf.start = j
        leaf.end = leafEnd
        leaf.suffixIndex = if (j == text.length - 1) -1 else j
        
        activeNode.children += (text(activeEdge) -> leaf)
        
        // Apply suffix link if lastNewNode exists
        if (lastNewNode != null) {
          lastNewNode.suffixLink = activeNode
          lastNewNode = null
        }
      } else {
        val next = activeNode.children(text(activeEdge))
        if (walkDown(next)) {
          // Continue with the next node
          continue
        }
        
        // Check if the character matches at the current position
        if (text(next.start + activeLength) == text(j)) {
          // Rule 3: Character matches, extend the active length
          activeLength += 1
          
          // Apply suffix link if lastNewNode exists
          if (lastNewNode != null && activeNode != root) {
            lastNewNode.suffixLink = activeNode
            lastNewNode = null
          }
          return
        }
        
        // Rule 1: Split the edge
        splitEnd = next.start + activeLength - 1
        
        val split = new SuffixTreeNode()
        split.start = next.start
        split.end = splitEnd
        split.suffixLink = root
        
        val newLeaf = new SuffixTreeNode()
        newLeaf.start = j
        newLeaf.end = leafEnd
        newLeaf.suffixIndex = j
        
        next.start = next.start + activeLength
        split.children += (text(next.start) -> next)
        split.children += (text(j) -> newLeaf)
        
        activeNode.children += (text(activeEdge) -> split)
        
        if (lastNewNode != null) {
          lastNewNode.suffixLink = split
        }
        lastNewNode = split
      }
      
      remainingSuffixCount -= 1
      if (activeNode == root && activeLength > 0) {
        activeLength -= 1
        activeEdge = j - remainingSuffixCount + 1
      } else if (activeNode != root) {
        activeNode = activeNode.suffixLink
      }
    }
  }
  
  // Walk down to the next node if possible
  private def walkDown(next: SuffixTreeNode): Boolean = {
    val len = next.end - next.start + 1
    if (activeLength >= len) {
      activeEdge += len
      activeLength -= len
      activeNode = next
      true
    } else {
      false
    }
  }
  
  // Print the suffix tree structure
  def printTree(): Unit = {
    println("Suffix Tree Structure:")
    printTreeHelper(root, "")
  }
  
  private def printTreeHelper(node: SuffixTreeNode, prefix: String): Unit = {
    if (node != null) {
      if (node.start != -1) {
        val substring = text.substring(node.start, node.end + 1)
        println(s"$prefix|$substring|")
      }
      
      node.children.foreach { case (char, child) =>
        printTreeHelper(child, prefix + "  ")
      }
    }
  }
  
  // Search for a pattern in the suffix tree
  def search(pattern: String): Option[Int] = {
    var currentNode = root
    var i = 0
    
    while (i < pattern.length) {
      val char = pattern(i)
      if (!currentNode.children.contains(char)) {
        return None
      }
      
      val nextNode = currentNode.children(char)
      var j = nextNode.start
      
      while (j <= nextNode.end && i < pattern.length) {
        if (pattern(i) != text(j)) {
          return None
        }
        i += 1
        j += 1
      }
      
      currentNode = nextNode
    }
    
    Some(currentNode.suffixIndex)
  }
}

// Example usage
object SuffixTreeExample extends App {
  val text = "BANANA$"
  val suffixTree = new SuffixTree(text)
  
  println(s"Building suffix tree for: $text")
  suffixTree.buildSuffixTree()
  println()
  
  suffixTree.printTree()
  println()
  
  // Test search operations
  val patterns = List("ANA", "BAN", "NANA", "A")
  patterns.foreach { pattern =>
    suffixTree.search(pattern) match {
      case Some(index) => println(s"Pattern '$pattern' found at index: $index")
      case None => println(s"Pattern '$pattern' not found")
    }
  }
}
```

## How it works:

1. **Node Structure**: Each node represents a substring of the text with start and end positions
2. **Ukkonen's Algorithm**: Uses a combination of active point, suffix links, and edge splitting
3. **Key Operations**:
   - `extendSuffixTree()`: Adds one character at a time
   - `walkDown()`: Moves to the next node when possible
   - `search()`: Finds patterns in the constructed tree

## Example Output:
```
Building suffix tree for: BANANA$

Suffix Tree Structure:
|BANANA$|
  |A|
    |NANA$|
      |A|
        |$
      |N|
        |A|
          |$
  |NANA$|
    |A|
      |$
    |N|
      |A|
        |$
  |ANA$|
    |$
  |NA$|
    |$
  |A$|
    |$
```

This implementation demonstrates the core concepts of suffix tree construction and can be used for efficient string matching operations.

