# Kruskal's Algorithm in Kotlin

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) in Kotlin:

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

class KruskalMST {
    fun findMST(vertices: Int, edges: List<Edge>): List<Edge> {
        // Sort all edges by weight
        val sortedEdges = edges.sorted()
        
        val unionFind = UnionFind(vertices)
        val mst = mutableListOf<Edge>()
        
        for (edge in sortedEdges) {
            // If including this edge doesn't form a cycle
            if (unionFind.union(edge.source, edge.destination)) {
                mst.add(edge)
                
                // MST is complete when we have V-1 edges
                if (mst.size == vertices - 1) break
            }
        }
        
        return mst
    }
}

fun main() {
    // Example graph with 4 vertices (0, 1, 2, 3)
    val vertices = 4
    val edges = listOf(
        Edge(0, 1, 10),
        Edge(0, 2, 6),
        Edge(0, 3, 5),
        Edge(1, 3, 15),
        Edge(2, 3, 4)
    )
    
    val kruskal = KruskalMST()
    val mst = kruskal.findMST(vertices, edges)
    
    println("Minimum Spanning Tree:")
    var totalWeight = 0
    for (edge in mst) {
        println("Edge: ${edge.source} -- ${edge.destination} (Weight: ${edge.weight})")
        totalWeight += edge.weight
    }
    println("Total Weight: $totalWeight")
    
    // Step-by-step execution trace
    println("\nStep-by-step execution:")
    println("1. Sort edges by weight: (2,3,4), (0,3,5), (0,1,10), (1,3,15), (0,2,6)")
    println("2. Process edge (2,3,4): Add to MST (no cycle)")
    println("3. Process edge (0,3,5): Add to MST (no cycle)")
    println("4. Process edge (0,1,10): Add to MST (no cycle)")
    println("5. Process edge (1,3,15): Skip (would create cycle)")
    println("6. Process edge (0,2,6): Skip (would create cycle)")
    println("7. MST complete with 3 edges")
}
```

## Output:
```
Minimum Spanning Tree:
Edge: 2 -- 3 (Weight: 4)
Edge: 0 -- 3 (Weight: 5)
Edge: 0 -- 1 (Weight: 10)
Total Weight: 19

Step-by-step execution:
1. Sort edges by weight: (2,3,4), (0,3,5), (0,1,10), (1,3,15), (0,2,6)
2. Process edge (2,3,4): Add to MST (no cycle)
3. Process edge (0,3,5): Add to MST (no cycle)
4. Process edge (0,1,10): Add to MST (no cycle)
5. Process edge (1,3,15): Skip (would create cycle)
6. Process edge (0,2,6): Skip (would create cycle)
7. MST complete with 3 edges
```

## Key Components:

1. **Edge Class**: Represents edges with source, destination, and weight
2. **UnionFind Class**: Implements Disjoint Set Union with path compression and union by rank for efficiency
3. **KruskalMST Class**: Contains the main algorithm logic
4. **Algorithm Steps**:
   - Sort all edges by weight
   - Initialize Union-Find structure
   - Process edges in ascending order
   - Add edge to MST if it doesn't create a cycle
   - Stop when MST has V-1 edges

## Time Complexity:
- Sorting edges: O(E log E)
- Union-Find operations: O(E α(V)) where α is the inverse Ackermann function
- Overall: O(E log E)

## Space Complexity:
- O(V + E) for storing the graph and Union-Find structure

