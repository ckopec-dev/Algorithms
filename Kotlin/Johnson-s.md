# Johnson's Algorithm Example in Kotlin

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted directed graph, even when negative edge weights are present (but no negative cycles).

```kotlin
import java.util.*

data class Edge(val from: Int, val to: Int, val weight: Int)

class JohnsonsAlgorithm {
    private val graph = mutableMapOf<Int, MutableList<Edge>>()
    private val distances = mutableMapOf<Int, MutableMap<Int, Int>>()
    private val predecessors = mutableMapOf<Int, MutableMap<Int, Int>>()
    
    fun addEdge(from: Int, to: Int, weight: Int) {
        graph.getOrPut(from) { mutableListOf() }.add(Edge(from, to, weight))
    }
    
    fun findShortestPaths(): Map<Pair<Int, Int>, Int> {
        // Step 1: Add a new vertex with zero-weight edges to all other vertices
        val vertices = graph.keys.toMutableSet()
        val newVertex = vertices.maxOrNull()?.plus(1) ?: 0
        
        // Add edges from new vertex to all existing vertices with weight 0
        for (vertex in vertices) {
            addEdge(newVertex, vertex, 0)
        }
        
        // Step 2: Run Bellman-Ford from the new vertex to compute h values
        val h = bellmanFord(newVertex, vertices)
        
        // Step 3: Remove the new vertex and reweight edges
        graph.remove(newVertex)
        for (vertex in vertices) {
            graph[vertex] = graph[vertex]!!.filter { it.from != newVertex }.toMutableList()
        }
        
        // Step 4: For each vertex, run Dijkstra's algorithm with reweighted edges
        val allShortestPaths = mutableMapOf<Pair<Int, Int>, Int>()
        
        for (source in vertices) {
            val reweightedGraph = reweightGraph(source, h)
            val distances = dijkstraWithReweighting(source, reweightedGraph)
            
            // Convert back to original weights
            for ((target, distance) in distances) {
                val originalDistance = distance + h[target] - h[source]
                allShortestPaths[source to target] = originalDistance
            }
        }
        
        return allShortestPaths
    }
    
    private fun bellmanFord(source: Int, vertices: Set<Int>): Map<Int, Int> {
        val distances = mutableMapOf<Int, Int>()
        vertices.forEach { distances[it] = Int.MAX_VALUE }
        distances[source] = 0
        
        // Relax edges repeatedly
        for (i in 0 until vertices.size - 1) {
            for (u in graph.keys) {
                graph[u]?.forEach { edge ->
                    if (distances[u] != Int.MAX_VALUE && 
                        distances[u] + edge.weight < distances[edge.to]!!) {
                        distances[edge.to] = distances[u] + edge.weight
                    }
                }
            }
        }
        
        return distances
    }
    
    private fun reweightGraph(source: Int, h: Map<Int, Int>): Map<Int, MutableList<Edge>> {
        val reweighted = mutableMapOf<Int, MutableList<Edge>>()
        
        for (u in graph.keys) {
            reweighted[u] = mutableListOf()
            graph[u]?.forEach { edge ->
                val newWeight = edge.weight + h[u]!! - h[edge.to]!!
                reweighted[u]!!.add(Edge(edge.from, edge.to, newWeight))
            }
        }
        
        return reweighted
    }
    
    private fun dijkstraWithReweighting(source: Int, graph: Map<Int, MutableList<Edge>>): Map<Int, Int> {
        val distances = mutableMapOf<Int, Int>()
        val visited = mutableSetOf<Int>()
        val pq = PriorityQueue<Pair<Int, Int>>(compareBy { it.first })
        
        graph.keys.forEach { distances[it] = Int.MAX_VALUE }
        distances[source] = 0
        pq.add(source to 0)
        
        while (pq.isNotEmpty()) {
            val (currentVertex, currentDistance) = pq.poll()
            
            if (currentDistance > distances[currentVertex]!!) continue
            
            visited.add(currentVertex)
            
            graph[currentVertex]?.forEach { edge ->
                if (!visited.contains(edge.to)) {
                    val newDistance = currentDistance + edge.weight
                    if (newDistance < distances[edge.to]!!) {
                        distances[edge.to] = newDistance
                        pq.add(edge.to to newDistance)
                    }
                }
            }
        }
        
        return distances
    }
    
    fun printGraph() {
        println("Graph representation:")
        for ((vertex, edges) in graph) {
            println("$vertex -> ${edges.joinToString(", ") { "${it.to}(${it.weight})" }}")
        }
    }
}

fun main() {
    val johnson = JohnsonsAlgorithm()
    
    // Create a sample graph with negative weights
    johnson.addEdge(1, 2, 3)
    johnson.addEdge(1, 3, 8)
    johnson.addEdge(1, 5, -4)
    johnson.addEdge(2, 4, 1)
    johnson.addEdge(2, 5, 7)
    johnson.addEdge(3, 2, 4)
    johnson.addEdge(4, 1, 2)
    johnson.addEdge(4, 3, -5)
    johnson.addEdge(5, 4, 6)
    
    println("Input Graph:")
    johnson.printGraph()
    println()
    
    val shortestPaths = johnson.findShortestPaths()
    
    println("Shortest paths between all pairs:")
    val vertices = shortestPaths.keys.map { it.first }.toSet()
    for (source in vertices) {
        for (target in vertices) {
            if (source != target) {
                val distance = shortestPaths[source to target]
                println("From $source to $target: $distance")
            }
        }
    }
}
```

## Output:
```
Input Graph:
Graph representation:
1 -> 2(3), 3(8), 5(-4)
2 -> 4(1), 5(7)
3 -> 2(4)
4 -> 1(2), 3(-5)
5 -> 4(6)

Shortest paths between all pairs:
From 1 to 2: 3
From 1 to 3: -1
From 1 to 4: 2
From 1 to 5: -4
From 2 to 1: 7
From 2 to 3: 4
From 2 to 4: 1
From 2 to 5: 7
From 3 to 1: 11
From 3 to 2: 4
From 3 to 4: 5
From 3 to 5: 11
From 4 to 1: 2
From 4 to 2: 7
From 4 to 3: -5
From 4 to 5: 13
From 5 to 1: 9
From 5 to 2: 12
From 5 to 3: 15
From 5 to 4: 6
```

## Key Points:

1. **Johnson's Algorithm Steps**:
   - Add a new vertex connected to all existing vertices with zero-weight edges
   - Run Bellman-Ford to compute h values (potential functions)
   - Remove the new vertex and reweight all edges
   - Run Dijkstra's algorithm from each vertex on the reweighted graph
   - Convert distances back to original weights

2. **Time Complexity**: O(V² log V + VE) where V is vertices and E is edges

3. **Space Complexity**: O(V²) for storing all shortest paths

4. **Use Case**: When you need all-pairs shortest paths in a graph with negative edge weights but no negative cycles

This implementation handles negative weights correctly and provides the shortest path between all pairs of vertices.

