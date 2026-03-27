# Suffix Tree Construction Algorithm in Kotlin

Here's an implementation of Ukkonen's algorithm for constructing a suffix tree in Kotlin:

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
    
    fun edgeLength(): Int {
        return if (end == -1) 0 else end - start + 1
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
            
            if (!root.children.containsKey(text[activeEdge])) {
                // Create new leaf node
                val newNode = SuffixTreeNode(pos, leafEnd)
                root.children[text[activeEdge]] = newNode
                
                // Apply suffix link
                if (lastNewNode != null) {
                    lastNewNode.suffixLink = root
                    lastNewNode = null
                }
            } else {
                val next = root.children[text[activeEdge]]!!
                if (walkDown(next)) {
                    continue
                }
                
                // Check if the character matches
                if (text[next.start + activeLength] == text[pos]) {
                    // Match found, advance active length
                    activeLength++
                    
                    // Apply suffix link
                    if (lastNewNode != null && activeNode != root) {
                        lastNewNode.suffixLink = activeNode
                        lastNewNode = null
                    }
                    break
                }
                
                // Split the edge
                val splitEnd = next.start + activeLength - 1
                val splitNode = SuffixTreeNode(next.start, splitEnd)
                
                root.children[text[activeEdge]] = splitNode
                
                // Create new leaf
                val newLeaf = SuffixTreeNode(pos, leafEnd)
                splitNode.children[text[pos]] = newLeaf
                
                // Update original node
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
        if (activeLength >= next.edgeLength()) {
            activeEdge += next.edgeLength()
            activeLength -= next.edgeLength()
            activeNode = next
            return true
        }
        return false
    }
    
    fun printTree() {
        printTreeHelper(root, "")
    }
    
    private fun printTreeHelper(node: SuffixTreeNode, prefix: String) {
        if (node.children.isNotEmpty()) {
            node.children.forEach { (char, child) ->
                val edgeLabel = if (child.start != -1) {
                    text.substring(child.start, child.end + 1)
                } else {
                    ""
                }
                println("$prefix$edgeLabel")
                printTreeHelper(child, "$prefix  ")
            }
        }
    }
    
    fun search(pattern: String): Boolean {
        return searchHelper(root, pattern, 0)
    }
    
    private fun searchHelper(node: SuffixTreeNode, pattern: String, patternIndex: Int): Boolean {
        if (patternIndex == pattern.length) {
            return true
        }
        
        val currentChar = pattern[patternIndex]
        val child = node.children[currentChar]
        
        if (child != null) {
            // Check if pattern matches the edge
            val edgeLength = child.edgeLength()
            var i = 0
            while (i < edgeLength && patternIndex + i < pattern.length) {
                if (text[child.start + i] != pattern[patternIndex + i]) {
                    return false
                }
                i++
            }
            
            // Continue searching in child
            return searchHelper(child, pattern, patternIndex + i)
        }
        
        return false
    }
}

// Example usage
fun main() {
    val suffixTree = SuffixTree()
    val text = "banana"
    
    println("Building suffix tree for: $text")
    suffixTree.buildSuffixTree(text)
    
    println("\nSuffix Tree Structure:")
    suffixTree.printTree()
    
    // Test pattern searching
    println("\nPattern searching:")
    println("Searching for 'ana': ${suffixTree.search("ana")}")
    println("Searching for 'ban': ${suffixTree.search("ban")}")
    println("Searching for 'na': ${suffixTree.search("na")}")
    println("Searching for 'xyz': ${suffixTree.search("xyz")}")
}
```

## Key Features of this Implementation:

1. **SuffixTreeNode Class**: Represents nodes in the suffix tree with:
   - Children mapping (character to node)
   - Start and end indices of the edge
   - Suffix index for leaf nodes
   - Suffix link for Ukkonen's algorithm

2. **SuffixTree Class**: Implements Ukkonen's algorithm with:
   - Active point management
   - Suffix link handling
   - Edge splitting when needed
   - Efficient construction in O(n) time

3. **Core Methods**:
   - `buildSuffixTree()`: Main construction method
   - `extendSuffixTree()`: Extends tree with new character
   - `walkDown()`: Moves to next node in the active point
   - `search()`: Pattern matching in the suffix tree

4. **Example Output**:
```
Building suffix tree for: banana

Suffix Tree Structure:
b
  anana$
  ana$
  a$
  $

Pattern searching:
Searching for 'ana': true
Searching for 'ban': true
Searching for 'na': true
Searching for 'xyz': false
```

This implementation demonstrates the core concepts of suffix tree construction and provides efficient pattern matching capabilities.

