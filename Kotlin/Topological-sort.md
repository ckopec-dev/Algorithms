# Topological Sort Algorithm in Kotlin

Here's an implementation of topological sort using Kahn's algorithm with adjacency list representation:

```kotlin
import java.util.*

class TopologicalSort {
    
    // Graph representation using adjacency list
    private val adjacencyList = mutableMapOf<Int, MutableList<Int>>()
    private val inDegree = mutableMapOf<Int, Int>()
    
    // Add edge to the graph
    fun addEdge(from: Int, to: Int) {
        adjacencyList.getOrPut(from) { mutableListOf() }.add(to)
        inDegree[to] = inDegree.getOrDefault(to, 0) + 1
        inDegree.getOrPut(from) { 0 }
    }
    
    // Perform topological sort
    fun topologicalSort(): List<Int> {
        val result = mutableListOf<Int>()
        val queue = LinkedList<Int>()
        
        // Find all nodes with in-degree 0
        for ((node, degree) in inDegree) {
            if (degree == 0) {
                queue.offer(node)
            }
        }
        
        // Process nodes in topological order
        while (queue.isNotEmpty()) {
            val current = queue.poll()
            result.add(current)
            
            // Reduce in-degree of adjacent nodes
            adjacencyList[current]?.forEach { neighbor ->
                inDegree[neighbor] = inDegree[neighbor]!! - 1
                if (inDegree[neighbor] == 0) {
                    queue.offer(neighbor)
                }
            }
        }
        
        // Check for cycles
        if (result.size != inDegree.size) {
            throw IllegalArgumentException("Graph contains cycle")
        }
        
        return result
    }
    
    // Clear the graph
    fun clear() {
        adjacencyList.clear()
        inDegree.clear()
    }
}

// Example usage
fun main() {
    val topSort = TopologicalSort()
    
    // Create a sample directed acyclic graph
    // 5 -> 0 -> 2 -> 3
    // 5 -> 2
    // 0 -> 1
    // 2 -> 3
    
    topSort.addEdge(5, 0)
    topSort.addEdge(5, 2)
    topSort.addEdge(0, 1)
    topSort.addEdge(2, 3)
    topSort.addEdge(2, 1)
    topSort.addEdge(3, 1)
    
    try {
        val result = topSort.topologicalSort()
        println("Topological order: $result")
        // Output: Topological order: [5, 0, 2, 3, 1]
    } catch (e: IllegalArgumentException) {
        println("Cycle detected: ${e.message}")
    }
    
    // Example with cycle
    val topSortWithCycle = TopologicalSort()
    topSortWithCycle.addEdge(0, 1)
    topSortWithCycle.addEdge(1, 2)
    topSortWithCycle.addEdge(2, 0) // Creates cycle
    
    try {
        val result = topSortWithCycle.topologicalSort()
        println("Topological order: $result")
    } catch (e: IllegalArgumentException) {
        println("Cycle detected: ${e.message}")
        // Output: Cycle detected: Graph contains cycle
    }
}
```

## How it works:

1. **Graph Representation**: Uses adjacency list to store the graph
2. **In-degree Tracking**: Maintains in-degree count for each node
3. **Kahn's Algorithm**:
   - Start with nodes having in-degree 0
   - Process nodes and reduce in-degrees of neighbors
   - Add nodes with in-degree 0 to queue
   - Continue until all nodes are processed
4. **Cycle Detection**: If result size doesn't match number of nodes, a cycle exists

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for adjacency list and in-degree tracking

## Output:
```
Topological order: [5, 0, 2, 3, 1]
Cycle detected: Graph contains cycle
```

