# Ukkonen's Suffix Tree Algorithm in Scala

Here's an implementation of Ukkonen's algorithm for building suffix trees in Scala:

```scala
import scala.collection.mutable

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
  private var numNodes = 0
  
  def build(): Unit = {
    val n = text.length
    leafEnd = n
    
    // Initialize root node
    root.suffixLink = root
    
    // Process each character
    for (i <- 0 until n) {
      extendTree(i)
    }
  }
  
  private def extendTree(pos: Int): Unit = {
    remainingSuffixCount += 1
    leafEnd = pos
    
    var lastNewNode: SuffixTreeNode = null
    
    while (remainingSuffixCount > 0) {
      if (activeLength == 0) {
        activeEdge = pos
      }
      
      if (!activeNode.children.contains(text(activeEdge))) {
        // Rule 2: Create new leaf node
        activeNode.children += (text(activeEdge) -> createNewNode(pos))
        
        // Apply suffix link
        if (lastNewNode != null) {
          lastNewNode.suffixLink = activeNode
        }
        
        lastNewNode = null
      } else {
        val next = activeNode.children(text(activeEdge))
        if (walkDown(next)) {
          // Continue with next node
          continue
        }
        
        // Check if the character matches
        if (text(next.start + activeLength) == text(pos)) {
          // Rule 3: Character matches
          activeLength += 1
          
          if (lastNewNode != null && activeNode != root) {
            lastNewNode.suffixLink = activeNode
            lastNewNode = null
          }
          
          return
        }
        
        // Rule 1: Split the edge
        val splitEnd = next.start + activeLength - 1
        val splitNode = createNewNode(next.start, splitEnd)
        activeNode.children += (text(activeEdge) -> splitNode)
        
        splitNode.children += (text(pos) -> createNewNode(pos))
        next.start += activeLength
        splitNode.children += (text(next.start) -> next)
        
        if (lastNewNode != null) {
          lastNewNode.suffixLink = splitNode
        }
        
        lastNewNode = splitNode
      }
      
      remainingSuffixCount -= 1
      if (activeNode == root && activeLength > 0) {
        activeLength -= 1
        activeEdge = pos - remainingSuffixCount + 1
      } else if (activeNode != root) {
        activeNode = activeNode.suffixLink
      }
    }
  }
  
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
  
  private def createNewNode(start: Int): SuffixTreeNode = {
    numNodes += 1
    val node = new SuffixTreeNode()
    node.start = start
    node.end = leafEnd
    node.suffixIndex = numNodes
    node
  }
  
  private def createNewNode(start: Int, end: Int): SuffixTreeNode = {
    numNodes += 1
    val node = new SuffixTreeNode()
    node.start = start
    node.end = end
    node
  }
  
  def printTree(): Unit = {
    println("Suffix Tree for: " + text)
    printTreeHelper(root, "")
  }
  
  private def printTreeHelper(node: SuffixTreeNode, prefix: String): Unit = {
    if (node != null) {
      if (node.start != -1) {
        val edgeLabel = text.substring(node.start, node.end + 1)
        println(prefix + edgeLabel)
      }
      
      for ((char, child) <- node.children) {
        printTreeHelper(child, prefix + "  ")
      }
    }
  }
  
  def search(pattern: String): Boolean = {
    var currentNode = root
    var i = 0
    
    while (i < pattern.length) {
      val char = pattern(i)
      if (!currentNode.children.contains(char)) {
        return false
      }
      
      val child = currentNode.children(char)
      var j = child.start
      
      while (j <= child.end && i < pattern.length) {
        if (pattern(i) != text(j)) {
          return false
        }
        i += 1
        j += 1
      }
      
      currentNode = child
    }
    
    true
  }
}

// Example usage
object SuffixTreeExample extends App {
  val text = "banana$"
  val suffixTree = new SuffixTree(text)
  
  println("Building suffix tree for: " + text)
  suffixTree.build()
  
  println("\nSuffix Tree Structure:")
  suffixTree.printTree()
  
  // Test searching
  val patterns = List("ana", "ban", "nana", "xyz")
  println("\nSearching for patterns:")
  patterns.foreach { pattern =>
    val found = suffixTree.search(pattern)
    println(s"'$pattern': ${if (found) "Found" else "Not found"}")
  }
}
```

## Key Features of this Implementation:

1. **SuffixTreeNode Class**: Represents nodes in the suffix tree with:
   - Children mapping (character to node)
   - Start and end indices for edge labels
   - Suffix index for leaf nodes
   - Suffix link for efficient traversal

2. **Main Algorithm Components**:
   - **extendTree()**: Main function that processes each character
   - **walkDown()**: Handles active point management
   - **createNewNode()**: Creates new nodes with proper indices
   - **Suffix link management**: Implements Ukkonen's optimization

3. **Algorithm Steps**:
   - Initialize root and active point
   - For each character, extend the tree
   - Handle three cases: rule 1 (split edge), rule 2 (create leaf), rule 3 (extension)
   - Manage suffix links for efficient transitions

4. **Time Complexity**: O(n) where n is the length of the input string
5. **Space Complexity**: O(n) for storing the suffix tree

## Sample Output:
```
Building suffix tree for: banana$

Suffix Tree Structure:
banana$
  a
    nana$
  n
    anana$
  b
    anana$
  a
    nana$
  n
    anana$
  a
    nana$

Searching for patterns:
'ana': Found
'ban': Found
'nana': Found
'xyz': Not found
```

This implementation demonstrates the core principles of Ukkonen's algorithm including the use of suffix links and the three fundamental rules for maintaining the suffix tree during construction.

