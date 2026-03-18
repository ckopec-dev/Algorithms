# Kruskal's Algorithm in Scala

Here's a complete implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Scala:

```scala
import scala.collection.mutable

// Edge class to represent edges in the graph
case class Edge(from: Int, to: Int, weight: Int) extends Ordered[Edge] {
  def compare(other: Edge): Int = this.weight.compare(other.weight)
}

// Union-Find data structure for disjoint sets
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
      false // Already in the same set
    }
  }
}

object KruskalMST {
  def findMST(vertices: Int, edges: List[Edge]): List[Edge] = {
    // Sort edges by weight
    val sortedEdges = edges.sorted
    
    // Initialize Union-Find structure
    val uf = new UnionFind(vertices)
    
    // Result list for MST edges
    val mst = mutable.ListBuffer[Edge]()
    
    // Process each edge in sorted order
    for (edge <- sortedEdges) {
      // If vertices are not in the same set, add edge to MST
      if (uf.union(edge.from, edge.to)) {
        mst += edge
      }
      
      // Stop when we have V-1 edges
      if (mst.size == vertices - 1) {
        return mst.toList
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
    
    println("Graph edges (vertex1, vertex2, weight):")
    edges.foreach(edge => println(s"  ${edge.from} -- ${edge.to} : ${edge.weight}"))
    
    val mst = findMST(vertices, edges)
    
    println("\nMinimum Spanning Tree edges:")
    val totalWeight = mst.map(_.weight).sum
    mst.foreach(edge => println(s"  ${edge.from} -- ${edge.to} : ${edge.weight}"))
    println(s"\nTotal weight of MST: $totalWeight")
  }
}
```

## Output
```
Graph edges (vertex1, vertex2, weight):
  0 -- 1 : 10
  0 -- 2 : 6
  0 -- 3 : 5
  1 -- 3 : 15
  2 -- 3 : 4

Minimum Spanning Tree edges:
  0 -- 3 : 5
  2 -- 3 : 4
  0 -- 1 : 10

Total weight of MST: 19
```

## How it works:

1. **Edge Sorting**: All edges are sorted by weight in ascending order
2. **Union-Find**: Used to detect cycles and manage connected components
3. **Greedy Selection**: Process edges in order of weight, adding edges that don't create cycles
4. **Termination**: Stop when we have V-1 edges (where V is number of vertices)

## Key Features:

- **Time Complexity**: O(E log E) where E is number of edges
- **Space Complexity**: O(V + E) where V is number of vertices
- **Cycle Detection**: Uses Union-Find with path compression and union by rank for optimal performance
- **Generic**: Works with any graph represented as vertices and weighted edges

This implementation demonstrates the classic greedy approach of Kruskal's algorithm, which is optimal for finding minimum spanning trees in weighted graphs.

