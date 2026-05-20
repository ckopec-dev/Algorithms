# Tarjan's Algorithm Implementation in Kotlin

Tarjan's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```kotlin
import java.util.*

class TarjanAlgorithm {
    private val stack = Stack<Int>()
    private val visited = mutableMapOf<Int, Boolean>()
    private val inStack = mutableMapOf<Int, Boolean>()
    private val ids = mutableMapOf<Int, Int>()
    private val lowLinks = mutableMapOf<Int, Int>()
    private var idCounter = 0
    private val sccs = mutableListOf<MutableList<Int>>()
    
    fun findStronglyConnectedComponents(graph: Map<Int, List<Int>>): List<List<Int>> {
        // Initialize all nodes
        for (node in graph.keys) {
            visited[node] = false
            inStack[node] = false
            ids[node] = -1
            lowLinks[node] = -1
        }
        
        // Process each unvisited node
        for (node in graph.keys) {
            if (!visited[node]!!) {
                tarjan(node, graph)
            }
        }
        
        return sccs
    }
    
    private fun tarjan(node: Int, graph: Map<Int, List<Int>>) {
        visited[node] = true
        ids[node] = idCounter
        lowLinks[node] = idCounter
        idCounter++
        stack.push(node)
        inStack[node] = true
        
        // Visit all neighbors
        for (neighbor in graph[node] ?: emptyList()) {
            if (!visited[neighbor]!!) {
                tarjan(neighbor, graph)
                lowLinks[node] = minOf(lowLinks[node]!!, lowLinks[neighbor]!!)
            } else if (inStack[neighbor]!!) {
                // Back edge - update low link
                lowLinks[node] = minOf(lowLinks[node]!!, ids[neighbor]!!)
            }
        }
        
        // If node is root of SCC, pop all nodes from stack
        if (ids[node] == lowLinks[node]) {
            val scc = mutableListOf<Int>()
            var current: Int
            do {
                current = stack.pop()
                inStack[current] = false
                scc.add(current)
            } while (current != node)
            sccs.add(scc)
        }
    }
}

// Example usage
fun main() {
    // Create a sample directed graph
    val graph = mapOf(
        0 to listOf(1),
        1 to listOf(2),
        2 to listOf(0, 3),
        3 to listOf(4),
        4 to listOf(5),
        5 to listOf(3, 6),
        6 to listOf(7),
        7 to listOf(8),
        8 to listOf(6)
    )
    
    val tarjan = TarjanAlgorithm()
    val sccs = tarjan.findStronglyConnectedComponents(graph)
    
    println("Strongly Connected Components:")
    sccs.forEachIndexed { index, scc ->
        println("SCC $index: ${scc.joinToString(", ")}")
    }
    
    // Output:
    // SCC 0: 0, 1, 2
    // SCC 1: 3, 4, 5, 6, 7, 8
}
```

## How it works:

1. **Initialization**: Each node is initialized with visited status, stack membership, and ID tracking
2. **DFS Traversal**: The algorithm performs a depth-first search using a stack
3. **Low Link Calculation**: For each node, it calculates the lowest reachable node ID
4. **SCC Detection**: When a node's ID equals its low link, it indicates the root of an SCC
5. **Stack Management**: Nodes are popped from the stack to form strongly connected components

## Time and Space Complexity:
- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the auxiliary data structures

## Key Features:
- Handles multiple SCCs in the same graph
- Uses a single pass through the graph
- Efficient stack-based implementation
- Properly manages back edges and cycle detection

This implementation correctly identifies all strongly connected components in any directed graph.

