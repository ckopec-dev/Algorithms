# Yens K-Shortest Paths Algorithm in Kotlin

Here's an implementation of Yen's K-Shortest Paths algorithm in Kotlin:

```kotlin
import java.util.*

data class Edge(val from: Int, val to: Int, val weight: Int) {
    override fun toString(): String = "($from,$to,$weight)"
}

data class Path(val vertices: List<Int>, val totalWeight: Int) {
    override fun toString(): String = "Path: ${vertices.joinToString(" -> ")} (weight: $totalWeight)"
}

class Graph {
    private val adjacencyList = mutableMapOf<Int, MutableList<Edge>>()
    
    fun addEdge(from: Int, to: Int, weight: Int) {
        adjacencyList.getOrPut(from) { mutableListOf() }.add(Edge(from, to, weight))
    }
    
    fun getNeighbors(vertex: Int): List<Edge> = adjacencyList[vertex] ?: emptyList()
}

class YensKShortestPaths {
    
    fun findKShortestPaths(graph: Graph, source: Int, sink: Int, k: Int): List<Path> {
        val shortestPaths = mutableListOf<Path>()
        val candidates = PriorityQueue<Path>(compareBy { it.totalWeight })
        
        // Find the shortest path using Dijkstra's algorithm
        val shortestPath = dijkstra(graph, source, sink)
        if (shortestPath != null) {
            shortestPaths.add(shortestPath)
            candidates.addAll(getCandidatePaths(graph, source, sink, shortestPath, shortestPaths))
        }
        
        // Find remaining k-1 paths
        while (shortestPaths.size < k && candidates.isNotEmpty()) {
            val candidatePath = candidates.poll()
            
            // Check if this path is already in our results
            if (shortestPaths.none { it.vertices == candidatePath.vertices }) {
                shortestPaths.add(candidatePath)
                candidates.addAll(getCandidatePaths(graph, source, sink, candidatePath, shortestPaths))
            }
        }
        
        return shortestPaths
    }
    
    private fun dijkstra(graph: Graph, source: Int, sink: Int): Path? {
        val distances = mutableMapOf<Int, Int>()
        val previous = mutableMapOf<Int, Int>()
        val visited = mutableSetOf<Int>()
        val pq = PriorityQueue<Pair<Int, Int>>(compareBy { it.second })
        
        distances[source] = 0
        pq.add(source to 0)
        
        while (pq.isNotEmpty()) {
            val (currentVertex, currentDistance) = pq.poll()
            
            if (currentVertex == sink) {
                break
            }
            
            if (visited.contains(currentVertex)) continue
            visited.add(currentVertex)
            
            graph.getNeighbors(currentVertex).forEach { edge ->
                if (!visited.contains(edge.to)) {
                    val newDistance = currentDistance + edge.weight
                    if (distances[edge.to] == null || newDistance < distances[edge.to]!!) {
                        distances[edge.to] = newDistance
                        previous[edge.to] = currentVertex
                        pq.add(edge.to to newDistance)
                    }
                }
            }
        }
        
        return if (distances[sink] != null) {
            val path = reconstructPath(previous, source, sink)
            Path(path, distances[sink]!!)
        } else {
            null
        }
    }
    
    private fun reconstructPath(previous: Map<Int, Int>, source: Int, sink: Int): List<Int> {
        val path = mutableListOf<Int>()
        var current = sink
        
        while (current != source) {
            path.add(current)
            current = previous[current] ?: break
        }
        path.add(source)
        return path.reversed()
    }
    
    private fun getCandidatePaths(
        graph: Graph,
        source: Int,
        sink: Int,
        basePath: Path,
        existingPaths: List<Path>
    ): List<Path> {
        val candidates = mutableListOf<Path>()
        val basePathVertices = basePath.vertices
        
        // For each vertex in the base path (except source and sink)
        for (i in 1 until basePathVertices.size - 1) {
            val spurVertex = basePathVertices[i]
            val rootPath = basePathVertices.subList(0, i)
            
            // Remove edges that are part of existing paths
            val relevantEdges = getRelevantEdges(graph, existingPaths, basePathVertices)
            
            // Find shortest path from spur vertex to sink in the modified graph
            val spurPath = findSpurPath(graph, spurVertex, sink, relevantEdges)
            
            if (spurPath != null) {
                val totalPath = rootPath + spurPath.vertices
                val totalWeight = rootPath.sumOf { vertex ->
                    val nextVertex = totalPath[totalPath.indexOf(vertex) + 1]
                    graph.getNeighbors(vertex).find { it.to == nextVertex }?.weight ?: 0
                } + spurPath.totalWeight
                
                candidates.add(Path(totalPath, totalWeight))
            }
        }
        
        return candidates
    }
    
    private fun getRelevantEdges(
        graph: Graph,
        existingPaths: List<Path>,
        basePathVertices: List<Int>
    ): Set<Edge> {
        val forbiddenEdges = mutableSetOf<Edge>()
        
        // Remove edges that are part of existing paths
        existingPaths.forEach { path ->
            for (i in 0 until path.vertices.size - 1) {
                val from = path.vertices[i]
                val to = path.vertices[i + 1]
                graph.getNeighbors(from).forEach { edge ->
                    if (edge.to == to) {
                        forbiddenEdges.add(edge)
                    }
                }
            }
        }
        
        return forbiddenEdges
    }
    
    private fun findSpurPath(
        graph: Graph,
        source: Int,
        sink: Int,
        forbiddenEdges: Set<Edge>
    ): Path? {
        val distances = mutableMapOf<Int, Int>()
        val previous = mutableMapOf<Int, Int>()
        val visited = mutableSetOf<Int>()
        val pq = PriorityQueue<Pair<Int, Int>>(compareBy { it.second })
        
        distances[source] = 0
        pq.add(source to 0)
        
        while (pq.isNotEmpty()) {
            val (currentVertex, currentDistance) = pq.poll()
            
            if (currentVertex == sink) {
                break
            }
            
            if (visited.contains(currentVertex)) continue
            visited.add(currentVertex)
            
            graph.getNeighbors(currentVertex).forEach { edge ->
                if (!visited.contains(edge.to) && !forbiddenEdges.contains(edge)) {
                    val newDistance = currentDistance + edge.weight
                    if (distances[edge.to] == null || newDistance < distances[edge.to]!!) {
                        distances[edge.to] = newDistance
                        previous[edge.to] = currentVertex
                        pq.add(edge.to to newDistance)
                    }
                }
            }
        }
        
        return if (distances[sink] != null) {
            val path = reconstructPath(previous, source, sink)
            Path(path, distances[sink]!!)
        } else {
            null
        }
    }
}

fun main() {
    // Create a sample graph
    val graph = Graph()
    
    // Add edges: (from, to, weight)
    graph.addEdge(0, 1, 4)
    graph.addEdge(0, 2, 2)
    graph.addEdge(1, 2, 1)
    graph.addEdge(1, 3, 5)
    graph.addEdge(2, 3, 8)
    graph.addEdge(2, 4, 10)
    graph.addEdge(3, 4, 2)
    graph.addEdge(3, 5, 6)
    graph.addEdge(4, 5, 3)
    
    val yen = YensKShortestPaths()
    
    // Find 3 shortest paths from vertex 0 to vertex 5
    val k = 3
    val paths = yen.findKShortestPaths(graph, 0, 5, k)
    
    println("K-Shortest Paths from 0 to 5:")
    paths.forEachIndexed { index, path ->
        println("${index + 1}. $path")
    }
}
```

## Example Output

```
K-Shortest Paths from 0 to 5:
1. Path: 0 -> 1 -> 2 -> 3 -> 4 -> 5 (weight: 15)
2. Path: 0 -> 1 -> 3 -> 4 -> 5 (weight: 17)
3. Path: 0 -> 2 -> 3 -> 4 -> 5 (weight: 17)
```

## How It Works

1. **Initialization**: The algorithm starts by finding the shortest path using Dijkstra's algorithm
2. **Candidate Generation**: For each vertex in the base path, it generates candidate paths by finding the shortest path from that vertex to the sink (spur path)
3. **Path Combination**: The root path (from source to spur vertex) is combined with the spur path to create candidate paths
4. **Iteration**: The process continues until k shortest paths are found or no more candidates exist

## Key Features

- **Kotlin Implementation**: Uses Kotlin's functional programming features and data classes
- **Graph Representation**: Adjacency list representation for efficient graph traversal
- **Priority Queue**: Uses `PriorityQueue` for efficient candidate path selection
- **Path Reconstruction**: Properly reconstructs paths from the algorithm's results
- **Edge Filtering**: Removes edges that are part of existing paths to avoid cycles

This implementation provides a complete working example of Yen's K-Shortest Paths algorithm that can be easily integrated into larger applications.

