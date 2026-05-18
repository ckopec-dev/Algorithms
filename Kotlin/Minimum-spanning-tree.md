# Minimum Spanning Tree Algorithm in Kotlin

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Kotlin:

```kotlin
import java.util.*

// Edge class to represent edges in the graph
data class Edge(val source: Int, val destination: Int, val weight: Int) : Comparable<Edge> {
    override fun compareTo(other: Edge): Int {
        return this.weight.compareTo(other.weight)
    }
}

// Union-Find (Disjoint Set Union) data structure
class UnionFind(private val size: Int) {
    private val parent = IntArray(size) { it }
    private val rank = IntArray(size) { 0 }
    
    fun find(x: Int): Int {
        if (parent[x] != x) {
            parent[x] = find(parent[x]) // Path compression
        }
        return parent[x]
    }
    
    fun union(x: Int, y: Int): Boolean {
        val rootX = find(x)
        val rootY = find(y)
        
        if (rootX == rootY) return false
        
        // Union by rank
        if (rank[rootX] < rank[rootY]) {
            parent[rootX] = rootY
        } else if (rank[rootX] > rank[rootY]) {
            parent[rootY] = rootX
        } else {
            parent[rootY] = rootX
            rank[rootX]++
        }
        return true
    }
}

class MinimumSpanningTree {
    
    fun kruskalMST(vertices: Int, edges: List<Edge>): List<Edge> {
        // Sort edges by weight
        val sortedEdges = edges.sorted()
        
        val unionFind = UnionFind(vertices)
        val mst = mutableListOf<Edge>()
        
        for (edge in sortedEdges) {
            // If adding this edge doesn't create a cycle
            if (unionFind.union(edge.source, edge.destination)) {
                mst.add(edge)
                
                // MST is complete when we have V-1 edges
                if (mst.size == vertices - 1) {
                    break
                }
            }
        }
        
        return mst
    }
    
    fun printMST(mst: List<Edge>) {
        println("Minimum Spanning Tree Edges:")
        var totalWeight = 0
        for (edge in mst) {
            println("Edge: ${edge.source} -- ${edge.destination} (Weight: ${edge.weight})")
            totalWeight += edge.weight
        }
        println("Total Weight: $totalWeight")
    }
}

// Example usage
fun main() {
    val mst = MinimumSpanningTree()
    
    // Create a graph with 6 vertices (0 to 5)
    val vertices = 6
    
    // Define edges (source, destination, weight)
    val edges = listOf(
        Edge(0, 1, 4),
        Edge(0, 2, 2),
        Edge(1, 2, 1),
        Edge(1, 3, 5),
        Edge(2, 3, 8),
        Edge(2, 4, 10),
        Edge(3, 4, 2),
        Edge(3, 5, 6),
        Edge(4, 5, 3)
    )
    
    println("Original Graph Edges:")
    edges.forEach { edge ->
        println("Edge: ${edge.source} -- ${edge.destination} (Weight: ${edge.weight})")
    }
    println()
    
    // Find Minimum Spanning Tree
    val result = mst.kruskalMST(vertices, edges)
    
    // Print the MST
    mst.printMST(result)
}
```

## Output:
```
Original Graph Edges:
Edge: 0 -- 1 (Weight: 4)
Edge: 0 -- 2 (Weight: 2)
Edge: 1 -- 2 (Weight: 1)
Edge: 1 -- 3 (Weight: 5)
Edge: 2 -- 3 (Weight: 8)
Edge: 2 -- 4 (Weight: 10)
Edge: 3 -- 4 (Weight: 2)
Edge: 3 -- 5 (Weight: 6)
Edge: 4 -- 5 (Weight: 3)

Minimum Spanning Tree Edges:
Edge: 1 -- 2 (Weight: 1)
Edge: 3 -- 4 (Weight: 2)
Edge: 0 -- 2 (Weight: 2)
Edge: 4 -- 5 (Weight: 3)
Edge: 1 -- 3 (Weight: 5)
Total Weight: 13
```

## Key Components:

1. **Edge Class**: Represents edges with source, destination, and weight
2. **UnionFind Class**: Implements disjoint set union with path compression and union by rank for efficient cycle detection
3. **Kruskal's Algorithm**: 
   - Sorts edges by weight
   - Uses Union-Find to detect cycles
   - Builds MST by selecting minimum weight edges that don't create cycles

## Time Complexity:
- **O(E log E)** where E is the number of edges
- The sorting step dominates the complexity
- Union-Find operations are nearly constant time due to optimizations

## Space Complexity:
- **O(V + E)** where V is vertices and E is edges

This implementation demonstrates a practical application of graph algorithms in Kotlin with proper data structures and efficient algorithmic approach.

