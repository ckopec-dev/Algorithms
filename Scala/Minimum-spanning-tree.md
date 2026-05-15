# Minimum Spanning Tree Algorithm in Scala

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Scala:

```scala
import scala.collection.mutable

// Edge case class to represent edges in the graph
case class Edge(from: Int, to: Int, weight: Int) extends Ordered[Edge] {
  def compare(other: Edge): Int = this.weight.compare(other.weight)
}

class UnionFind(n: Int) {
  private val parent = Array.tabulate(n)(i => i)
  private val rank = Array.fill(n)(0)
  
  def find(x: Int): Int = {
    if (parent(x) != x) {
      parent(x) = find(parent(x)) // Path compression
    }
    parent(x)
  }
  
  def union(x: Int, y: Int): Boolean = {
    val rootX = find(x)
    val rootY = find(y)
    
    if (rootX != rootY) {
      // Union by rank
      if (rank(rootX) < rank(rootY)) {
        parent(rootX) = rootY
      } else if (rank(rootX) > rank(rootY)) {
        parent(rootY) = rootX
      } else {
        parent(rootY) = rootX
        rank(rootX) += 1
      }
      true
    } else {
      false
    }
  }
}

object MinimumSpanningTree {
  def kruskalMST(vertices: Int, edges: List[Edge]): List[Edge] = {
    // Sort edges by weight
    val sortedEdges = edges.sorted
    
    // Initialize Union-Find structure
    val uf = new UnionFind(vertices)
    
    // Result list for MST edges
    val mst = mutable.ListBuffer[Edge]()
    
    // Process each edge in sorted order
    for (edge <- sortedEdges) {
      if (uf.union(edge.from, edge.to)) {
        mst += edge
        // Stop when we have V-1 edges
        if (mst.length == vertices - 1) {
          return mst.toList
        }
      }
    }
    
    mst.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example graph with 4 vertices and 5 edges
    val vertices = 4
    val edges = List(
      Edge(0, 1, 10),
      Edge(0, 2, 6),
      Edge(0, 3, 5),
      Edge(1, 3, 15),
      Edge(2, 3, 4)
    )
    
    val mst = kruskalMST(vertices, edges)
    
    println("Minimum Spanning Tree edges:")
    mst.foreach(edge => println(s"Edge: ${edge.from} -- ${edge.to} (weight: ${edge.weight})"))
    
    val totalWeight = mst.map(_.weight).sum
    println(s"\nTotal weight of MST: $totalWeight")
  }
}
```

## Output:
```
Minimum Spanning Tree edges:
Edge: 2 -- 3 (weight: 4)
Edge: 0 -- 3 (weight: 5)
Edge: 0 -- 1 (weight: 10)

Total weight of MST: 19
```

## Key Components:

1. **Edge Class**: Represents graph edges with from, to, and weight properties
2. **UnionFind Class**: Implements Union-Find data structure with path compression and union by rank for efficient operations
3. **kruskalMST Function**: Main algorithm implementation using Kruskal's approach:
   - Sort edges by weight
   - Use Union-Find to detect cycles
   - Add edges to MST if they don't create cycles
4. **Time Complexity**: O(E log E) where E is the number of edges
5. **Space Complexity**: O(V + E) where V is vertices and E is edges

This implementation demonstrates a clean, functional approach to solving the MST problem in Scala using immutable data structures and efficient algorithms.

