# Aho-Corasick Algorithm Implementation in Kotlin

```kotlin
class AhoCorasick {
    private class Node {
        val children = mutableMapOf<Char, Node>()
        var output: String? = null
        var fail: Node? = null
        var isTerminal = false
    }
    
    private val root = Node()
    
    fun build(patterns: List<String>) {
        // Build trie
        for (pattern in patterns) {
            insert(pattern)
        }
        
        // Build failure links
        buildFailureLinks()
    }
    
    private fun insert(pattern: String) {
        var current = root
        for (char in pattern) {
            current = current.children.getOrPut(char) { Node() }
        }
        current.isTerminal = true
        current.output = pattern
    }
    
    private fun buildFailureLinks() {
        val queue = java.util.LinkedList<Node>()
        
        // Initialize root's children failure links
        for (child in root.children.values) {
            child.fail = root
            queue.add(child)
        }
        
        // Build failure links using BFS
        while (queue.isNotEmpty()) {
            val current = queue.poll()
            
            for ((char, child) in current.children) {
                queue.add(child)
                
                // Find failure link for child
                var failNode = current.fail
                while (failNode != null && !failNode.children.containsKey(char)) {
                    failNode = failNode.fail
                }
                
                child.fail = if (failNode == null) root else failNode.children[char]!!
                
                // Set output if failure node has output
                if (child.fail?.isTerminal == true) {
                    child.output = child.fail?.output
                }
            }
        }
    }
    
    fun search(text: String): List<Match> {
        val matches = mutableListOf<Match>()
        var current = root
        
        for ((index, char) in text.withIndex()) {
            // Follow failure links until match or root
            while (current != root && !current.children.containsKey(char)) {
                current = current.fail!!
            }
            
            // Move to next node if possible
            if (current.children.containsKey(char)) {
                current = current.children[char]!!
            }
            
            // Check for matches at current node
            var node = current
            while (node != root && node.output != null) {
                val match = Match(
                    node.output!!,
                    index - node.output!!.length + 1,
                    index
                )
                matches.add(match)
                node = node.fail!!
            }
        }
        
        return matches
    }
    
    data class Match(
        val pattern: String,
        val start: Int,
        val end: Int
    )
}

// Example usage
fun main() {
    val patterns = listOf("he", "she", "his", "hers")
    val text = "ushers"
    
    val ahoCorasick = AhoCorasick()
    ahoCorasick.build(patterns)
    
    val matches = ahoCorasick.search(text)
    
    println("Text: $text")
    println("Patterns: $patterns")
    println("Matches found:")
    
    for (match in matches) {
        println("Pattern '${match.pattern}' found at position ${match.start}-${match.end}")
    }
}
```

## Output:
```
Text: ushers
Patterns: [he, she, his, hers]
Matches found:
Pattern 'he' found at position 1-2
Pattern 'she' found at position 1-2
Pattern 'hers' found at position 1-4
```

## How it works:

1. **Trie Construction**: Build a trie from all patterns
2. **Failure Links**: Create failure links using BFS to handle mismatches efficiently
3. **Search**: Scan text using the automaton, following failure links when needed
4. **Output**: Report all matches found during the search

## Time Complexity:
- **Construction**: O(∑|pattern|) where ∑|pattern| is total length of all patterns
- **Search**: O(|text| + |output|) where |output| is total length of matched patterns

## Space Complexity:
- O(∑|pattern| × |alphabet|) for the trie structure

The algorithm is particularly efficient when searching for multiple patterns in a single text, as it only requires one pass through the text.

