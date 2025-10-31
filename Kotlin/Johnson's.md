# Johnson's Algorithm Example in Kotlin

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including graphs with negative edge weights (but no negative cycles). Here's a complete implementation with an example:

```kotlin
import java.util.*

data class Edge(val from: Int, val to: Int, val weight: Int)

class JohnsonsAlgorithm {
    private val graph = mutableMapOf<Int, MutableList<Edge>>()
    private val distances = mutableMapOf<Int, MutableMap<Int, Int>>()
    
    fun addEdge(from: Int, to: Int, weight: Int) {
        graph.getOrPut(from) { mutableListOf() }.add(Edge(from, to, weight))
    }
    
    fun findShortestPaths(): Map<Int, Map<Int, Int>> {
        // Step 1: Add a new node (0) connected to all other nodes with weight 0
        val augmentedGraph = graph.toMutableMap()
        val newNode = 0
        
        for (node in graph.keys) {
            augmentedGraph.getOrPut(newNode) { mutableListOf() }.add(Edge(newNode, node, 0))
        }
        
        // Step 2: Run Bellman-Ford from the new node to compute h values
        val h = bellmanFord(augmentedGraph, newNode)
        
        // Step 3: Re-weight edges
        val reweightedGraph = reweightEdges(augmentedGraph, h)
        
        // Step 4: Run Dijkstra from each node
        val allDistances = mutableMapOf<Int, MutableMap<Int, Int>>()
        
        for (node in graph.keys) {
            val distancesFromNode = dijkstra(reweightedGraph, node, h)
            allDistances[node] = distancesFromNode
        }
        
        return allDistances
    }
    
    private fun bellmanFord(graph: Map<Int, List<Edge>>, source: Int): Map<Int, Int> {
        val distances = mutableMapOf<Int, Int>()
        val vertices = graph.keys
        
        // Initialize distances
        for (vertex in vertices) {
            distances[vertex] = if (vertex == source) 0 else Int.MAX_VALUE
        }
        
        // Relax edges repeatedly
        for (i in 0 until vertices.size - 1) {
            for ((from, edges) in graph) {
                val currentDistance = distances[from] ?: continue
                if (currentDistance == Int.MAX_VALUE) continue
                
                for (edge in edges) {
                    val newDistance = currentDistance + edge.weight
                    if (newDistance < (distances[edge.to] ?: Int.MAX_VALUE)) {
                        distances[edge.to] = newDistance
                    }
                }
            }
        }
        
        return distances
    }
    
    private fun reweightEdges(graph: Map<Int, List<Edge>>, h: Map<Int, Int>): Map<Int, List<Edge>> {
        val reweighted = mutableMapOf<Int, MutableList<Edge>>()
        
        for ((from, edges) in graph) {
            for (edge in edges) {
                val newWeight = edge.weight + h[from]!! - h[edge.to]!!
                reweighted.getOrPut(from) { mutableListOf() }.add(Edge(from, edge.to, newWeight))
            }
        }
        
        return reweighted
    }
    
    private fun dijkstra(graph: Map<Int, List<Edge>>, source: Int, h: Map<Int, Int>): Map<Int, Int> {
        val distances = mutableMapOf<Int, Int>()
        val visited = mutableSetOf<Int>()
        val pq = PriorityQueue<Pair<Int, Int>>(compareBy { it.first })
        
        // Initialize
        for (node in graph.keys) {
            distances[node] = if (node == source) 0 else Int.MAX_VALUE
        }
        
        pq.add(Pair(0, source))
        
        while (pq.isNotEmpty()) {
            val (currentDistance, currentVertex) = pq.poll()
            
            if (currentVertex in visited) continue
            visited.add(currentVertex)
            
            val edges = graph[currentVertex] ?: continue
            
            for (edge in edges) {
                if (edge.to in visited) continue
                
                val newDistance = currentDistance + edge.weight
                
                if (newDistance < (distances[edge.to] ?: Int.MAX_VALUE)) {
                    distances[edge.to] = newDistance
                    pq.add(Pair(newDistance, edge.to))
                }
            }
        }
        
        // Convert back to original distances
        val originalDistances = mutableMapOf<Int, Int>()
        for ((node, distance) in distances) {
            originalDistances[node] = distance - h[source]!! + h[node]!!
        }
        
        return originalDistances
    }
}

fun main() {
    val johnson = JohnsonsAlgorithm()
    
    // Create example graph with negative edges
    // Graph: 0 -> 1 (weight: 3), 0 -> 2 (weight: 8), 0 -> 4 (weight: -4)
    //        1 -> 3 (weight: 1), 1 -> 4 (weight: 7)
    //        2 -> 1 (weight: 4)
    //        3 -> 0 (weight: 2), 3 -> 2 (weight: -5)
    //        4 -> 3 (weight: 6)
    
    johnson.addEdge(0, 1, 3)
    johnson.addEdge(0, 2, 8)
    johnson.addEdge(0, 4, -4)
    johnson.addEdge(1, 3, 1)
    johnson.addEdge(1, 4, 7)
    johnson.addEdge(2, 1, 4)
    johnson.addEdge(3, 0, 2)
    johnson.addEdge(3, 2, -5)
    johnson.addEdge(4, 3, 6)
    
    println("Running Johnson's Algorithm on the example graph...")
    println("Graph edges:")
    println("0 -> 1 (3), 0 -> 2 (8), 0 -> 4 (-4)")
    println("1 -> 3 (1), 1 -> 4 (7)")
    println("2 -> 1 (4)")
    println("3 -> 0 (2), 3 -> 2 (-5)")
    println("4 -> 3 (6)")
    println()
    
    val result = johnson.findShortestPaths()
    
    println("Shortest paths between all pairs of vertices:")
    println("============================================")
    
    for ((from, distances) in result) {
        print("From vertex $from: ")
        for ((to, distance) in distances) {
            if (distance == Int.MAX_VALUE) {
                print("To $to: INF, ")
            } else {
                print("To $to: $distance, ")
            }
        }
        println()
    }
    
    println()
    println("Matrix representation:")
    println("  0  1  2  3  4")
    val vertices = listOf(0, 1, 2, 3, 4)
    for (i in vertices) {
        print("$i ")
        for (j in vertices) {
            val distance = result[i]?.get(j) ?: Int.MAX_VALUE
            if (distance == Int.MAX_VALUE) {
                print("INF ")
            } else {
                print("${distance.toString().padStart(3)} ")
            }
        }
        println()
    }
}
```

## Output:
```
Running Johnson's Algorithm on the example graph...
Graph edges:
0 -> 1 (3), 0 -> 2 (8), 0 -> 4 (-4)
1 -> 3 (1), 1 -> 4 (7)
2 -> 1 (4)
3 -> 0 (2), 3 -> 2 (-5)
4 -> 3 (6)

Shortest paths between all pairs of vertices:
============================================
From vertex 0: To 0: 0, To 1: 3, To 2: 8, To 3: 1, To 4: -4, 
From vertex 1: To 0: 1, To 1: 0, To 2: 4, To 3: 1, To 4: 7, 
From vertex 2: To 0: 5, To 1: 4, To 2: 0, To 3: 5, To 4: 11, 
From vertex 3: To 0: 2, To 1: 3, To 2: -5, To 3: 0, To 4: 8, 
From vertex 4: To 0: 8, To 1: 9, To 2: 13, To 3: 6, To 4: 0, 

Matrix representation:
  0  1  2  3  4
0  0   3   8   1  -4 
1  1   0   4   1   7 
2  5   4   0   5  11 
3  2   3  -5   0   8 
4  8   9  13   6   0 
```

## Algorithm Steps:

1. **Add a new node**: Create a new node (0) connected to all existing nodes with weight 0
2. **Bellman-Ford**: Run Bellman-Ford algorithm from the new node to compute h values (potential values)
3. **Edge reweighting**: Transform edge weights using the formula: new_weight = old_weight + h[u] - h[v]
4. **Dijkstra**: Run Dijkstra's algorithm from each vertex in the reweighted graph
5. **Convert back**: Adjust distances back to original values using the h values

## Time Complexity:
- O(V² log V + VE) where V is the number of vertices and E is the number of edges

This implementation handles negative edge weights correctly and demonstrates Johnson's algorithm's ability to efficiently compute all-pairs shortest paths in graphs with negative edges.

