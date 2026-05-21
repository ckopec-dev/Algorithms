# Edmonds-Karp Algorithm Implementation in Kotlin

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```kotlin
import java.util.*

class FlowNetwork {
    private val adjList: MutableMap<Int, MutableList<Edge>> = mutableMapOf()
    private val vertices: Int
    
    constructor(vertices: Int) {
        this.vertices = vertices
        for (i in 0 until vertices) {
            adjList[i] = mutableListOf()
        }
    }
    
    fun addEdge(from: Int, to: Int, capacity: Int) {
        val forwardEdge = Edge(from, to, capacity)
        val backwardEdge = Edge(to, from, 0) // Backward edge with 0 capacity initially
        
        adjList[from]!!.add(forwardEdge)
        adjList[to]!!.add(backwardEdge)
    }
    
    fun getEdges(from: Int): List<Edge> {
        return adjList[from] ?: emptyList()
    }
    
    fun getVertices(): Int {
        return vertices
    }
}

data class Edge(val from: Int, val to: Int, var capacity: Int) {
    fun getResidualCapacity(): Int {
        return capacity
    }
}

class EdmondsKarp {
    private val graph: FlowNetwork
    
    constructor(graph: FlowNetwork) {
        this.graph = graph
    }
    
    fun findMaxFlow(source: Int, sink: Int): Int {
        var maxFlow = 0
        var path: List<Edge>? = null
        
        while (true) {
            path = findAugmentingPath(source, sink)
            if (path == null) break
            
            val bottleneck = findBottleneck(path)
            augmentPath(path, bottleneck)
            maxFlow += bottleneck
        }
        
        return maxFlow
    }
    
    private fun findAugmentingPath(source: Int, sink: Int): List<Edge>? {
        val visited = BooleanArray(graph.getVertices()) { false }
        val parent = IntArray(graph.getVertices()) { -1 }
        val queue = LinkedList<Int>()
        
        queue.offer(source)
        visited[source] = true
        
        while (queue.isNotEmpty()) {
            val current = queue.poll()
            
            for (edge in graph.getEdges(current)) {
                if (!visited[edge.to] && edge.getResidualCapacity() > 0) {
                    visited[edge.to] = true
                    parent[edge.to] = current
                    queue.offer(edge.to)
                    
                    if (edge.to == sink) {
                        return buildPath(parent, source, sink)
                    }
                }
            }
        }
        
        return null
    }
    
    private fun buildPath(parent: IntArray, source: Int, sink: Int): List<Edge> {
        val path = mutableListOf<Edge>()
        var current = sink
        
        while (current != source) {
            val prev = parent[current]
            // Find the edge from prev to current
            for (edge in graph.getEdges(prev)) {
                if (edge.to == current) {
                    path.add(0, edge) // Add at beginning to maintain order
                    break
                }
            }
            current = prev
        }
        
        return path
    }
    
    private fun findBottleneck(path: List<Edge>): Int {
        return path.minOfOrNull { it.getResidualCapacity() } ?: 0
    }
    
    private fun augmentPath(path: List<Edge>, bottleneck: Int) {
        for (edge in path) {
            edge.capacity -= bottleneck
            // Find the reverse edge and update it
            val reverseEdge = findReverseEdge(edge.to, edge.from)
            if (reverseEdge != null) {
                reverseEdge.capacity += bottleneck
            }
        }
    }
    
    private fun findReverseEdge(from: Int, to: Int): Edge? {
        for (edge in graph.getEdges(from)) {
            if (edge.to == to) {
                return edge
            }
        }
        return null
    }
}

// Example usage
fun main() {
    // Create a flow network with 6 vertices (0 to 5)
    val network = FlowNetwork(6)
    
    // Add edges with capacities
    network.addEdge(0, 1, 10)
    network.addEdge(0, 2, 10)
    network.addEdge(1, 2, 2)
    network.addEdge(1, 3, 4)
    network.addEdge(1, 4, 8)
    network.addEdge(2, 4, 9)
    network.addEdge(3, 5, 10)
    network.addEdge(4, 3, 6)
    network.addEdge(4, 5, 10)
    
    val ek = EdmondsKarp(network)
    val maxFlow = ek.findMaxFlow(0, 5)
    
    println("Maximum flow from vertex 0 to vertex 5: $maxFlow")
    
    // Expected output: Maximum flow from vertex 0 to vertex 5: 19
}
```

## Algorithm Explanation

The Edmonds-Karp algorithm works as follows:

1. **Initialization**: Create a flow network with vertices and edges having capacities
2. **Find Augmenting Paths**: Use BFS to find paths from source to sink in the residual graph
3. **Calculate Bottleneck**: Find the minimum capacity along the path
4. **Update Flow**: Augment the flow along the path by the bottleneck capacity
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity

- **Time Complexity**: O(VE²) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Example Network

The example creates a flow network with:
```
    0
   / \
  1   2
 / \ / \
3  4  5
```

With the following capacities:
- 0→1: 10, 0→2: 10
- 1→2: 2, 1→3: 4, 1→4: 8
- 2→4: 9, 3→5: 10, 4→3: 6, 4→5: 10

The maximum flow from vertex 0 to vertex 5 is 19.

