# Depth-First Search (DFS) Algorithm in Kotlin

Here's a complete implementation of the Depth-First Search algorithm in Kotlin:

```kotlin
import java.util.*

// Graph representation using adjacency list
class Graph(private val vertices: Int) {
    private val adjacencyList = Array(vertices) { mutableListOf<Int>() }
    
    // Add edge to the graph
    fun addEdge(source: Int, destination: Int) {
        adjacencyList[source].add(destination)
        adjacencyList[destination].add(source) // For undirected graph
    }
    
    // DFS using recursion
    fun dfsRecursive(startVertex: Int) {
        val visited = BooleanArray(vertices) { false }
        println("DFS Traversal (Recursive):")
        dfsRecursiveHelper(startVertex, visited)
        println()
    }
    
    private fun dfsRecursiveHelper(vertex: Int, visited: BooleanArray) {
        visited[vertex] = true
        print("$vertex ")
        
        // Visit all adjacent vertices
        for (adjacent in adjacencyList[vertex]) {
            if (!visited[adjacent]) {
                dfsRecursiveHelper(adjacent, visited)
            }
        }
    }
    
    // DFS using iteration (stack)
    fun dfsIterative(startVertex: Int) {
        val visited = BooleanArray(vertices) { false }
        val stack = Stack<Int>()
        
        println("DFS Traversal (Iterative):")
        stack.push(startVertex)
        
        while (stack.isNotEmpty()) {
            val vertex = stack.pop()
            
            if (!visited[vertex]) {
                visited[vertex] = true
                print("$vertex ")
                
                // Add all unvisited adjacent vertices to stack
                // Reverse the list to maintain consistent order
                adjacencyList[vertex].reversed().forEach { adjacent ->
                    if (!visited[adjacent]) {
                        stack.push(adjacent)
                    }
                }
            }
        }
        println()
    }
}

// Example usage
fun main() {
    // Create a graph with 6 vertices (0 to 5)
    val graph = Graph(6)
    
    // Add edges to create the following graph:
    // 0 -- 1 -- 2
    // |    |    |
    // 3 -- 4 -- 5
    graph.addEdge(0, 1)
    graph.addEdge(0, 3)
    graph.addEdge(1, 2)
    graph.addEdge(1, 4)
    graph.addEdge(2, 5)
    graph.addEdge(4, 5)
    
    // Perform DFS traversal
    graph.dfsRecursive(0)
    graph.dfsIterative(0)
}
```

## Output:
```
DFS Traversal (Recursive):
0 1 2 5 4 3 

DFS Traversal (Iterative):
0 1 2 5 4 3 
```

## Key Features:

1. **Graph Representation**: Uses adjacency list to store the graph
2. **Two Implementations**: 
   - Recursive DFS
   - Iterative DFS using stack
3. **Visited Tracking**: Boolean array to keep track of visited vertices
4. **Vertex Traversal**: Visits all connected vertices in depth-first manner

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the visited array and recursion stack

## Algorithm Steps:
1. Start with the initial vertex
2. Mark it as visited
3. Visit all unvisited adjacent vertices recursively or iteratively
4. Continue until all reachable vertices are visited

This implementation demonstrates both the recursive and iterative approaches to DFS, which are commonly used in graph traversal problems.

