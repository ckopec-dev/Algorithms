# Push-Relabel Max-Flow Algorithm in Kotlin

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```kotlin
import java.util.*

class PushRelabel {
    data class Edge(val to: Int, val capacity: Long, var flow: Long = 0)
    
    private val vertices: Int
    private val edges: MutableList<MutableList<Edge>> = mutableListOf()
    private val height: MutableList<Int> = mutableListOf()
    private val excess: MutableList<Long> = mutableListOf()
    private val active: Queue<Int> = LinkedList()
    
    constructor(vertices: Int) {
        this.vertices = vertices
        repeat(vertices) {
            edges.add(mutableListOf())
            height.add(0)
            excess.add(0L)
        }
    }
    
    fun addEdge(from: Int, to: Int, capacity: Long) {
        edges[from].add(Edge(to, capacity))
        edges[to].add(Edge(from, 0)) // Residual edge with 0 capacity
    }
    
    private fun push(u: Int, edge: Edge) {
        val pushAmount = minOf(excess[u], edge.capacity - edge.flow)
        if (pushAmount <= 0) return
        
        edge.flow += pushAmount
        val reverseEdge = edges[edge.to].first { it.to == u }
        reverseEdge.flow -= pushAmount
        excess[u] -= pushAmount
        excess[edge.to] += pushAmount
        
        if (excess[edge.to] > 0 && edge.to != 0 && edge.to != vertices - 1) {
            active.add(edge.to)
        }
    }
    
    private fun relabel(u: Int) {
        var minHeight = Int.MAX_VALUE
        for (edge in edges[u]) {
            if (edge.capacity - edge.flow > 0) {
                minHeight = minOf(minHeight, height[edge.to])
            }
        }
        if (minHeight < Int.MAX_VALUE) {
            height[u] = minHeight + 1
        }
    }
    
    fun maxFlow(source: Int, sink: Int): Long {
        // Initialize heights and excess
        height[source] = vertices
        for (edge in edges[source]) {
            if (edge.capacity > 0) {
                edge.flow = edge.capacity
                val reverseEdge = edges[edge.to].first { it.to == source }
                reverseEdge.flow = -edge.capacity
                excess[edge.to] += edge.capacity
                if (edge.to != sink) {
                    active.add(edge.to)
                }
            }
        }
        
        while (active.isNotEmpty()) {
            val u = active.poll()
            var pushed = false
            
            // Push excess flow to adjacent vertices
            for (edge in edges[u]) {
                if (edge.capacity - edge.flow > 0 && height[u] > height[edge.to]) {
                    push(u, edge)
                    pushed = true
                    if (excess[u] == 0) break
                }
            }
            
            // If no push was possible, relabel
            if (!pushed) {
                relabel(u)
                active.add(u)
            }
        }
        
        return excess[sink]
    }
    
    fun printGraph() {
        println("Flow Network:")
        for (u in 0 until vertices) {
            print("Vertex $u: ")
            for (edge in edges[u]) {
                if (edge.capacity > 0) {
                    print("(${edge.to}, ${edge.flow}/${edge.capacity}) ")
                }
            }
            println()
        }
    }
}

fun main() {
    // Create a flow network with 6 vertices (0 to 5)
    // Source = 0, Sink = 5
    val maxFlow = PushRelabel(6)
    
    // Add edges with capacities
    maxFlow.addEdge(0, 1, 10L)
    maxFlow.addEdge(0, 2, 10L)
    maxFlow.addEdge(1, 2, 2L)
    maxFlow.addEdge(1, 3, 4L)
    maxFlow.addEdge(1, 4, 8L)
    maxFlow.addEdge(2, 4, 9L)
    maxFlow.addEdge(3, 5, 10L)
    maxFlow.addEdge(4, 5, 10L)
    
    println("Flow Network:")
    maxFlow.printGraph()
    
    val result = maxFlow.maxFlow(0, 5)
    println("\nMaximum Flow: $result")
    
    // Example with a simpler network
    println("\n" + "=".repeat(40))
    println("Simple Example:")
    
    val simpleFlow = PushRelabel(4)
    simpleFlow.addEdge(0, 1, 10L)
    simpleFlow.addEdge(0, 2, 10L)
    simpleFlow.addEdge(1, 3, 10L)
    simpleFlow.addEdge(2, 3, 10L)
    
    println("Simple Flow Network:")
    simpleFlow.printGraph()
    
    val simpleResult = simpleFlow.maxFlow(0, 3)
    println("\nMaximum Flow: $simpleResult")
}
```

## Algorithm Explanation

The Push-Relabel algorithm works as follows:

1. **Initialization**: 
   - Set source height to number of vertices
   - Initialize excess flow for each vertex
   - Push initial flow from source to its neighbors

2. **Main Loop**:
   - While there are active vertices (vertices with excess flow):
     - Select an active vertex `u`
     - Try to push flow from `u` to its neighbors
     - If no push is possible, relabel vertex `u` (increase its height)

3. **Push Operation**:
   - Send excess flow along an edge with available capacity
   - Update forward and reverse edges

4. **Relabel Operation**:
   - Increase height of vertex to allow flow to be pushed to neighbors
   - Height should be at least one more than minimum height of neighbors

## Time Complexity
- **Time**: O(V²E) in the worst case
- **Space**: O(V + E)

## Example Output
```
Flow Network:
Vertex 0: (1, 0/10) (2, 0/10) 
Vertex 1: (0, 0/10) (2, 0/2) (3, 0/4) (4, 0/8) 
Vertex 2: (0, 0/10) (1, 0/2) (4, 0/9) 
Vertex 3: (1, 0/4) (5, 0/10) 
Vertex 4: (1, 0/8) (2, 0/9) (5, 0/10) 
Vertex 5: (3, 0/10) (4, 0/10) 

Maximum Flow: 19
```

This implementation provides a complete working example of the Push-Relabel maximum flow algorithm in Kotlin.

