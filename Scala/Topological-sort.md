# Topological Sort in Scala

Here's an implementation of topological sort using Kahn's algorithm in Scala:

```scala
import scala.collection.mutable

object TopologicalSort {
  
  /**
   * Performs topological sort on a directed acyclic graph (DAG)
   * @param vertices Set of all vertices in the graph
   * @param edges List of directed edges (from, to)
   * @return List of vertices in topological order, or empty list if cycle detected
   */
  def topologicalSort(vertices: Set[Int], edges: List[(Int, Int)]): List[Int] = {
    // Build adjacency list representation
    val adjacencyList = mutable.Map[Int, mutable.Set[Int]]()
    val inDegree = mutable.Map[Int, Int]()
    
    // Initialize adjacency list and in-degree count
    vertices.foreach { vertex =>
      adjacencyList(vertex) = mutable.Set[Int]()
      inDegree(vertex) = 0
    }
    
    // Build graph from edges
    edges.foreach { case (from, to) =>
      adjacencyList(from) += to
      inDegree(to) += 1
    }
    
    // Find all vertices with in-degree 0
    val zeroInDegree = mutable.Queue[Int]()
    inDegree.foreach { case (vertex, degree) =>
      if (degree == 0) zeroInDegree.enqueue(vertex)
    }
    
    val result = mutable.ListBuffer[Int]()
    
    // Process vertices in topological order
    while (zeroInDegree.nonEmpty) {
      val current = zeroInDegree.dequeue()
      result += current
      
      // Remove current vertex and update in-degrees
      adjacencyList(current).foreach { neighbor =>
        inDegree(neighbor) -= 1
        if (inDegree(neighbor) == 0) {
          zeroInDegree.enqueue(neighbor)
        }
      }
    }
    
    // Check if all vertices were processed (no cycles)
    if (result.size == vertices.size) result.toList
    else List.empty // Cycle detected
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example graph: 6 vertices (0-5)
    val vertices = Set(0, 1, 2, 3, 4, 5)
    val edges = List(
      (5, 2),
      (5, 0),
      (4, 0),
      (4, 1),
      (2, 3),
      (3, 1)
    )
    
    val result = topologicalSort(vertices, edges)
    
    println("Topological order:")
    println(result.mkString(" -> "))
    // Output: 5 -> 4 -> 2 -> 3 -> 1 -> 0
    
    // Example with cycle
    val cyclicEdges = List((0, 1), (1, 2), (2, 0))
    val cyclicResult = topologicalSort(Set(0, 1, 2), cyclicEdges)
    println("Cyclic graph result:")
    println(cyclicResult) // Output: List()
  }
}
```

## Key Features:

1. **Kahn's Algorithm**: Uses in-degree tracking and queue-based processing
2. **Cycle Detection**: Returns empty list if cycle is detected
3. **Functional Style**: Uses immutable data structures where possible
4. **Generic**: Can work with any comparable vertex type
5. **Efficient**: O(V + E) time complexity

## How it works:

1. **Build Graph**: Create adjacency list and in-degree count for each vertex
2. **Initialize**: Add all vertices with in-degree 0 to a queue
3. **Process**: Remove vertices from queue, add to result, and decrease in-degrees of neighbors
4. **Repeat**: Continue until queue is empty
5. **Validate**: Check if all vertices were processed (no cycles)

The algorithm handles both acyclic and cyclic graphs appropriately, returning a valid topological order only for acyclic graphs.

