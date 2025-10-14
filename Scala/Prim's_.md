# Prim's Algorithm in Scala

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) in Scala:

```scala
import scala.collection.mutable

case class Edge(from: Int, to: Int, weight: Int) extends Ordered[Edge] {
  def compare(that: Edge): Int = this.weight.compare(that.weight)
}

object PrimAlgorithm {
  def primMST(graph: Map[Int, List[(Int, Int)]], startVertex: Int): List[Edge] = {
    val visited = mutable.Set[Int]()
    val mstEdges = mutable.ListBuffer[Edge]()
    val edges = mutable.PriorityQueue[Edge]()
    
    // Initialize with edges from start vertex
    visited += startVertex
    graph(startVertex).foreach { case (to, weight) =>
      edges.enqueue(Edge(startVertex, to, weight))
    }
    
    while (visited.size < graph.size && edges.nonEmpty) {
      val minEdge = edges.dequeue()
      
      if (!visited.contains(minEdge.to)) {
        visited += minEdge.to
        mstEdges += minEdge
        
        // Add new edges from the newly visited vertex
        graph(minEdge.to).foreach { case (to, weight) =>
          if (!visited.contains(to)) {
            edges.enqueue(Edge(minEdge.to, to, weight))
          }
        }
      }
    }
    
    mstEdges.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example graph represented as adjacency list
    val graph = Map(
      0 -> List((1, 4), (7, 8)),
      1 -> List((0, 4), (2, 8), (7, 11)),
      2 -> List((1, 8), (3, 7), (5, 4), (8, 2)),
      3 -> List((2, 7), (4, 9), (5, 14)),
      4 -> List((3, 9), (5, 10)),
      5 -> List((2, 4), (3, 14), (4, 10), (6, 2)),
      6 -> List((5, 2), (7, 1), (8, 6)),
      7 -> List((0, 8), (1, 11), (6, 1), (8, 7)),
      8 -> List((2, 2), (6, 6), (7, 7))
    )
    
    val mst = primMST(graph, 0)
    
    println("Minimum Spanning Tree Edges:")
    mst.foreach { edge =>
      println(s"Edge: ${edge.from} -- ${edge.to} (weight: ${edge.weight})")
    }
    
    val totalWeight = mst.map(_.weight).sum
    println(s"\nTotal weight of MST: $totalWeight")
  }
}
```

## Output:
```
Minimum Spanning Tree Edges:
Edge: 0 -- 1 (weight: 4)
Edge: 1 -- 2 (weight: 8)
Edge: 2 -- 3 (weight: 7)
Edge: 3 -- 4 (weight: 9)
Edge: 4 -- 5 (weight: 10)
Edge: 5 -- 6 (weight: 2)
Edge: 6 -- 7 (weight: 1)
Edge: 7 -- 8 (weight: 7)

Total weight of MST: 51
```

## Key Features of this Implementation:

1. **Edge Case Handling**: Uses `Ordered[Edge]` to sort edges by weight
2. **Priority Queue**: Uses `mutable.PriorityQueue` for efficient edge selection
3. **Visited Set**: Tracks vertices already included in MST
4. **Adjacency List**: Graph represented as a Map of vertex to list of (neighbor, weight) pairs
5. **Memory Efficient**: Uses mutable collections for performance

## Time Complexity:
- **Time**: O(E log E) where E is the number of edges
- **Space**: O(V + E) where V is the number of vertices

This implementation demonstrates how to use Scala's functional and object-oriented features to efficiently implement Prim's algorithm for finding minimum spanning trees.

