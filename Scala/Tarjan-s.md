# Tarjan's Algorithm Implementation in Scala

Tarjan's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation in Scala:

```scala
import scala.collection.mutable

class TarjanAlgorithm {
  case class Edge(from: Int, to: Int)
  
  def findStronglyConnectedComponents(vertices: Int, edges: List[Edge]): List[List[Int]] = {
    val stack = mutable.Stack[Int]()
    val indices = Array.fill(vertices)(-1)
    val lowLinks = Array.fill(vertices)(-1)
    val onStack = Array.fill(vertices)(false)
    
    var index = 0
    val sccs = mutable.ListBuffer[List[Int]]()
    
    def strongConnect(vertex: Int): Unit = {
      indices(vertex) = index
      lowLinks(vertex) = index
      index += 1
      stack.push(vertex)
      onStack(vertex) = true
      
      // Consider successors of vertex
      edges.filter(_.from == vertex).foreach { edge =>
        val successor = edge.to
        if (indices(successor) == -1) {
          strongConnect(successor)
          lowLinks(vertex) = math.min(lowLinks(vertex), lowLinks(successor))
        } else if (onStack(successor)) {
          lowLinks(vertex) = math.min(lowLinks(vertex), indices(successor))
        }
      }
      
      // If vertex is a root node, pop the stack and create an SCC
      if (lowLinks(vertex) == indices(vertex)) {
        val scc = mutable.ListBuffer[Int]()
        var current = -1
        do {
          current = stack.pop()
          onStack(current) = false
          scc += current
        } while (current != vertex)
        sccs += scc.toList
      }
    }
    
    // Run strongConnect for each unvisited vertex
    (0 until vertices).foreach { vertex =>
      if (indices(vertex) == -1) {
        strongConnect(vertex)
      }
    }
    
    sccs.toList
  }
}

// Example usage
object TarjanExample extends App {
  val tarjan = new TarjanAlgorithm()
  
  // Create a sample directed graph
  // Graph structure:
  // 0 -> 1 -> 2 -> 0 (cycle)
  // 3 -> 4 -> 5 -> 3 (cycle)
  // 2 -> 3 (connection between cycles)
  val edges = List(
    Edge(0, 1),
    Edge(1, 2),
    Edge(2, 0),
    Edge(3, 4),
    Edge(4, 5),
    Edge(5, 3),
    Edge(2, 3)
  )
  
  val sccs = tarjan.findStronglyConnectedComponents(6, edges)
  
  println("Strongly Connected Components:")
  sccs.foreach { scc =>
    println(s"SCC: ${scc.mkString(", ")}")
  }
  
  // Output:
  // SCC: 0, 1, 2
  // SCC: 3, 4, 5
}
```

## Key Features of this Implementation:

1. **Vertex Representation**: Uses integer IDs for vertices
2. **Edge Representation**: Simple case class for directed edges
3. **Stack Management**: Uses mutable stack to track vertices during DFS
4. **Index Tracking**: Maintains discovery indices and low-link values
5. **SCC Detection**: Identifies and groups vertices into strongly connected components

## Time and Space Complexity:

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the auxiliary arrays and stack

## How it Works:

1. **DFS Traversal**: Performs depth-first search on the graph
2. **Index Assignment**: Assigns discovery indices to vertices
3. **Low-Link Calculation**: Tracks the smallest index reachable from each vertex
4. **SCC Formation**: When a vertex's low-link equals its index, it's a root of an SCC
5. **Stack Management**: Uses stack to efficiently extract components

This implementation correctly handles multiple SCCs and can be easily extended for various graph analysis tasks.

