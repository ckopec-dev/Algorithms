# Iterative Deepening Depth-First Search (IDDFS) in Kotlin

```kotlin
import java.util.*

data class Node(val id: String, val children: List<Node> = emptyList())

class IDDFS {
    
    /**
     * Iterative Deepening Depth-First Search implementation
     * @param root The starting node
     * @param target The value to search for
     * @return The path to target node or null if not found
     */
    fun search(root: Node, target: String): List<String>? {
        var depth = 0
        
        while (true) {
            val result = depthLimitedSearch(root, target, depth)
            if (result != null) {
                return result
            }
            depth++
            
            // Prevent infinite loop in case of very deep trees
            if (depth > 1000) break
        }
        
        return null
    }
    
    /**
     * Depth-Limited Search (helper function for IDDFS)
     * @param node Current node
     * @param target Target value to find
     * @param depthLimit Maximum depth allowed
     * @return Path to target or null if not found within depth limit
     */
    private fun depthLimitedSearch(node: Node, target: String, depthLimit: Int): List<String>? {
        return if (depthLimit < 0) {
            null // Depth limit exceeded
        } else if (node.id == target) {
            listOf(node.id) // Found target
        } else {
            // Explore children
            for (child in node.children) {
                val result = depthLimitedSearch(child, target, depthLimit - 1)
                if (result != null) {
                    return listOf(node.id) + result
                }
            }
            null // Target not found in this subtree
        }
    }
}

// Example usage
fun main() {
    // Create a sample tree structure
    val nodeF = Node("F")
    val nodeE = Node("E")
    val nodeD = Node("D")
    val nodeC = Node("C", listOf(nodeF))
    val nodeB = Node("B", listOf(nodeD, nodeE))
    val nodeA = Node("A", listOf(nodeB, nodeC))
    
    val iddfs = IDDFS()
    
    // Test searching for different nodes
    println("Searching for 'F': ${iddfs.search(nodeA, "F")}")
    println("Searching for 'E': ${iddfs.search(nodeA, "E")}")
    println("Searching for 'B': ${iddfs.search(nodeA, "B")}")
    println("Searching for 'X': ${iddfs.search(nodeA, "X")}")
    
    // Visual representation of the tree:
    //       A
    //    /     \
    //   B       C
    //  / \     /
    // D   E   F
}
```

## Output:
```
Searching for 'F': [A, C, F]
Searching for 'E': [A, B, E]
Searching for 'B': [A, B]
Searching for 'X': null
```

## How IDDFS Works:

1. **Iterative Approach**: Starts with depth limit = 0 and increases it incrementally
2. **Depth-Limited Search**: For each depth limit, performs a regular DFS with that limit
3. **Complete**: Guarantees finding the shallowest solution
4. **Space Efficient**: Uses O(bd) space where b is branching factor and d is depth
5. **Time Complexity**: O(b^d) where d is the depth of the solution

## Key Features:

- **Complete**: Finds solution if one exists
- **Optimal**: Finds the shallowest solution (like BFS)
- **Space Efficient**: Only stores current path, not all nodes
- **Adaptable**: Works with any tree/graph structure
- **No Prior Knowledge**: Doesn't require knowledge of solution depth

The algorithm combines the benefits of both DFS (low memory usage) and BFS (completeness and optimality) by systematically increasing the depth limit until a solution is found.

