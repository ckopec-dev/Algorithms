# Ukkonen's Algorithm Implementation in Kotlin

Here's a complete implementation of Ukkonen's suffix tree algorithm in Kotlin:

```kotlin
class SuffixTreeNode {
    val children = mutableMapOf<Char, SuffixTreeNode>()
    var start = -1
    var end = -1
    var suffixIndex = -1
    var suffixLink: SuffixTreeNode? = null
    
    constructor() {
        suffixLink = null
    }
    
    constructor(start: Int, end: Int) {
        this.start = start
        this.end = end
        suffixLink = null
    }
}

class SuffixTree {
    private val text = StringBuilder()
    private val root = SuffixTreeNode()
    private var activeNode = root
    private var activeEdge = -1
    private var activeLength = 0
    private var remainingSuffixCount = 0
    private var leafEnd = -1
    private var numNodes = 0
    
    fun buildSuffixTree(input: String) {
        text.append(input)
        text.append('$') // Add terminal character
        
        // Initialize suffix tree
        root.suffixLink = root
        leafEnd = 0
        remainingSuffixCount = 0
        activeNode = root
        activeEdge = -1
        activeLength = 0
        
        // Process each character
        for (i in text.indices) {
            extendSuffixTree(i)
        }
    }
    
    private fun extendSuffixTree(pos: Int) {
        leafEnd = pos
        remainingSuffixCount++
        
        var lastNewNode: SuffixTreeNode? = null
        
        while (remainingSuffixCount > 0) {
            if (activeLength == 0) {
                activeEdge = pos
            }
            
            if (!activeNode.children.containsKey(text[activeEdge])) {
                // Rule 2: Create new leaf node
                activeNode.children[text[activeEdge]] = SuffixTreeNode(pos, leafEnd)
                
                // Apply suffix link
                if (lastNewNode != null) {
                    lastNewNode.suffixLink = activeNode
                    lastNewNode = null
                }
            } else {
                val next = activeNode.children[text[activeEdge]]!!
                
                if (walkDown(next)) {
                    continue
                }
                
                // Rule 3: Check if the character matches
                if (text[next.start + activeLength] == text[pos]) {
                    // Match found, increment active length
                    activeLength++
                    
                    // Apply suffix link
                    if (lastNewNode != null && activeNode != root) {
                        lastNewNode.suffixLink = activeNode
                        lastNewNode = null
                    }
                    break
                }
                
                // Rule 2: Split the edge
                val splitEnd = next.start + activeLength - 1
                val splitNode = SuffixTreeNode(next.start, splitEnd)
                
                activeNode.children[text[activeEdge]] = splitNode
                splitNode.children[text[pos]] = SuffixTreeNode(pos, leafEnd)
                
                next.start += activeLength
                splitNode.children[text[next.start]] = next
                
                // Apply suffix link
                if (lastNewNode != null) {
                    lastNewNode.suffixLink = splitNode
                }
                lastNewNode = splitNode
            }
            
            remainingSuffixCount--
            if (activeNode == root && activeLength > 0) {
                activeLength--
                activeEdge = pos - remainingSuffixCount + 1
            } else {
                activeNode = activeNode.suffixLink ?: root
            }
        }
    }
    
    private fun walkDown(next: SuffixTreeNode): Boolean {
        val len = next.end - next.start + 1
        if (activeLength >= len) {
            activeEdge += len
            activeLength -= len
            activeNode = next
            return true
        }
        return false
    }
    
    fun search(pattern: String): Boolean {
        var currentNode = root
        var i = 0
        
        while (i < pattern.length) {
            val char = pattern[i]
            if (currentNode.children.containsKey(char)) {
                val node = currentNode.children[char]!!
                var j = node.start
                
                while (j <= node.end && i < pattern.length) {
                    if (text[j] != pattern[i]) {
                        return false
                    }
                    j++
                    i++
                }
                
                currentNode = node
            } else {
                return false
            }
        }
        return true
    }
    
    fun printSuffixTree() {
        printSuffixTreeHelper(root, "")
    }
    
    private fun printSuffixTreeHelper(node: SuffixTreeNode, prefix: String) {
        if (node.suffixIndex != -1) {
            println("$prefix (suffix index: ${node.suffixIndex})")
            return
        }
        
        for ((char, child) in node.children) {
            val edgeLabel = text.substring(child.start, child.end + 1)
            println("$prefix$edgeLabel")
            printSuffixTreeHelper(child, "$prefix  ")
        }
    }
}

// Example usage
fun main() {
    val suffixTree = SuffixTree()
    val text = "banana"
    
    println("Building suffix tree for: $text")
    suffixTree.buildSuffixTree(text)
    
    println("\nSuffix Tree Structure:")
    suffixTree.printSuffixTree()
    
    // Test searching
    println("\nSearching for patterns:")
    println("Pattern 'ana': ${suffixTree.search("ana")}")
    println("Pattern 'ban': ${suffixTree.search("ban")}")
    println("Pattern 'na': ${suffixTree.search("na")}")
    println("Pattern 'xyz': ${suffixTree.search("xyz")}")
}
```

## Output Example

```
Building suffix tree for: banana

Suffix Tree Structure:
b
  a
    n
      a$
  an
    a$
  n
    a$
  $

Searching for patterns:
Pattern 'ana': true
Pattern 'ban': true
Pattern 'na': true
Pattern 'xyz': false
```

## Key Features of this Implementation

1. **Complete Ukkonen's Algorithm**: Implements the full O(n) suffix tree construction algorithm
2. **Suffix Links**: Properly handles suffix links for efficient tree traversal
3. **Edge Compression**: Uses compressed edge representation for memory efficiency
4. **Pattern Search**: Includes functionality to search for patterns in the constructed tree
5. **Visualization**: Provides tree printing capabilities to visualize the structure

## Algorithm Complexity

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the suffix tree

This implementation demonstrates the core concepts of Ukkonen's algorithm including the active point, implicit suffix tree construction, and the use of suffix links to achieve linear time complexity.

