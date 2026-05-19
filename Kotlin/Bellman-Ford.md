# Bellman-Ford Algorithm in Kotlin

Here's a complete implementation of the Bellman-Ford algorithm in Kotlin to find the shortest paths from a source vertex to all other vertices in a weighted graph:

```kotlin
import java.util.*

data class Edge(val from: Int, val to: Int, val weight: Int)

class BellmanFord {
    fun findShortestPaths(vertices: Int, edges: List<Edge>, source: Int): Pair<IntArray, Boolean> {
        // Initialize distances array with infinity
        val distances = IntArray(vertices) { Int.MAX_VALUE }
        distances[source] = 0
        
        // Relax edges repeatedly
        for (i in 0 until vertices - 1) {
            for (edge in edges) {
                if (distances[edge.from] != Int.MAX_VALUE && 
                    distances[edge.from] + edge.weight < distances[edge.to]) {
                    distances[edge.to] = distances[edge.from] + edge.weight
                }
            }
        }
        
        // Check for negative weight cycles
        val hasNegativeCycle = checkForNegativeCycle(edges, distances)
        
        return Pair(distances, hasNegativeCycle)
    }
    
    private fun checkForNegativeCycle(edges: List<Edge>, distances: IntArray): Boolean {
        for (edge in edges) {
            if (distances[edge.from] != Int.MAX_VALUE && 
                distances[edge.from] + edge.weight < distances[edge.to]) {
                return true // Negative cycle detected
            }
        }
        return false
    }
    
    fun printShortestPaths(distances: IntArray, source: Int) {
        println("Shortest distances from vertex $source:")
        for (i in distances.indices) {
            if (distances[i] == Int.MAX_VALUE) {
                println("Vertex $i: INF")
            } else {
                println("Vertex $i: ${distances[i]}")
            }
        }
    }
}

fun main() {
    val bellmanFord = BellmanFord()
    
    // Create a sample graph with 5 vertices
    val vertices = 5
    val edges = listOf(
        Edge(0, 1, 4),
        Edge(0, 2, 2),
        Edge(1, 2, 1),
        Edge(1, 3, 5),
        Edge(2, 3, 8),
        Edge(2, 4, 10),
        Edge(3, 4, 2)
    )
    
    val source = 0
    
    println("Graph edges:")
    edges.forEach { edge ->
        println("${edge.from} -> ${edge.to} (weight: ${edge.weight})")
    }
    
    val (distances, hasNegativeCycle) = bellmanFord.findShortestPaths(vertices, edges, source)
    
    if (hasNegativeCycle) {
        println("Graph contains negative weight cycle!")
    } else {
        bellmanFord.printShortestPaths(distances, source)
    }
    
    // Example with negative cycle
    println("\n--- Testing with negative cycle ---")
    val edgesWithNegativeCycle = listOf(
        Edge(0, 1, 1),
        Edge(1, 2, 1),
        Edge(2, 0, -3) // This creates a negative cycle
    )
    
    val (distances2, hasNegativeCycle2) = bellmanFord.findShortestPaths(3, edgesWithNegativeCycle, 0)
    
    if (hasNegativeCycle2) {
        println("Graph contains negative weight cycle!")
    } else {
        bellmanFord.printShortestPaths(distances2, 0)
    }
}
```

## Output:
```
Graph edges:
0 -> 1 (weight: 4)
0 -> 2 (weight: 2)
1 -> 2 (weight: 1)
1 -> 3 (weight: 5)
2 -> 3 (weight: 8)
2 -> 4 (weight: 10)
3 -> 4 (weight: 2)
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11

--- Testing with negative cycle ---
Graph contains negative weight cycle!
```

## Key Features:

1. **Edge Class**: Represents weighted edges with `from`, `to`, and `weight` properties
2. **Main Algorithm**: Implements the Bellman-Ford algorithm with:
   - Initialization of distances
   - Relaxation of edges for V-1 iterations
   - Negative cycle detection
3. **Negative Cycle Detection**: Checks if any distance can still be improved after V-1 iterations
4. **Edge Cases**: Handles unreachable vertices (marked as INF) and negative weight cycles

## Time Complexity:
- **Time**: O(VE) where V is vertices and E is edges
- **Space**: O(V) for storing distances

## Use Cases:
- Finding shortest paths in graphs with negative edge weights
- Detecting negative weight cycles
- Network routing algorithms
- Financial arbitrage detection

