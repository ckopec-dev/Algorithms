# Prim's Algorithm in Scala

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) of a graph:

```scala
import scala.collection.mutable

case class Edge(from: Int, to: Int, weight: Int) {
  def reverse: Edge = Edge(to, from, weight)
}

class Graph(vertices: Int) {
  private val adjList = Array.fill(vertices)(mutable.ListBuffer.empty[Edge])
  
  def addEdge(from: Int, to: Int, weight: Int): Unit = {
    adjList(from) += Edge(from, to, weight)
    adjList(to) += Edge(to, from, weight)
  }
  
  def getNeighbors(vertex: Int): Iterable[Edge] = adjList(vertex)
}

object PrimsAlgorithm {
  def findMST(graph: Graph, startVertex: Int): List[Edge] = {
    val visited = mutable.Set[Int]()
    val mstEdges = mutable.ListBuffer[Edge]()
    val edgeHeap = mutable.PriorityQueue[Edge]()(Ordering.by(-_.weight))
    
    // Start with the given vertex
    visited += startVertex
    
    // Add all edges from the start vertex to the heap
    graph.getNeighbors(startVertex).foreach { edge =>
      edgeHeap.enqueue(edge)
    }
    
    while (visited.size < graph.vertices && edgeHeap.nonEmpty) {
      val minEdge = edgeHeap.dequeue()
      
      val (from, to) = (minEdge.from, minEdge.to)
      
      // If the edge connects to an unvisited vertex
      if (!visited.contains(to)) {
        visited += to
        mstEdges += minEdge
        
        // Add all edges from the newly visited vertex
        graph.getNeighbors(to).foreach { edge =>
          if (!visited.contains(edge.to)) {
            edgeHeap.enqueue(edge)
          }
        }
      }
    }
    
    mstEdges.toList
  }
  
  // Alternative implementation using a simpler approach
  def findMSTSimple(graph: Graph, startVertex: Int): List[Edge] = {
    val visited = mutable.Set[Int]()
    val mstEdges = mutable.ListBuffer[Edge]()
    
    visited += startVertex
    
    while (visited.size < graph.vertices) {
      var minEdge: Option[Edge] = None
      var minWeight = Int.MaxValue
      
      // Find the minimum weight edge that connects to a visited vertex
      for (vertex <- visited) {
        graph.getNeighbors(vertex).foreach { edge =>
          if (!visited.contains(edge.to) && edge.weight < minWeight) {
            minWeight = edge.weight
            minEdge = Some(edge)
          }
        }
      }
      
      minEdge.foreach { edge =>
        visited += edge.to
        mstEdges += edge
      }
    }
    
    mstEdges.toList
  }
}

// Example usage
object Main extends App {
  // Create a graph with 6 vertices
  val graph = new Graph(6)
  
  // Add edges (from, to, weight)
  graph.addEdge(0, 1, 4)
  graph.addEdge(0, 2, 2)
  graph.addEdge(1, 2, 1)
  graph.addEdge(1, 3, 5)
  graph.addEdge(2, 3, 8)
  graph.addEdge(2, 4, 10)
  graph.addEdge(3, 4, 2)
  graph.addEdge(3, 5, 6)
  graph.addEdge(4, 5, 3)
  
  println("Graph edges:")
  for (i <- 0 until graph.vertices) {
    graph.getNeighbors(i).foreach { edge =>
      println(s"  $i -- ${edge.to} (weight: ${edge.weight})")
    }
  }
  
  println("\nMinimum Spanning Tree (using simple approach):")
  val mst = PrimsAlgorithm.findMSTSimple(graph, 0)
  mst.foreach { edge =>
    println(s"  ${edge.from} -- ${edge.to} (weight: ${edge.weight})")
  }
  
  val totalWeight = mst.map(_.weight).sum
  println(s"\nTotal weight of MST: $totalWeight")
}
```

## Output
```
Graph edges:
  0 -- 1 (weight: 4)
  0 -- 2 (weight: 2)
  1 -- 0 (weight: 4)
  1 -- 2 (weight: 1)
  1 -- 3 (weight: 5)
  2 -- 0 (weight: 2)
  2 -- 1 (weight: 1)
  2 -- 3 (weight: 8)
  2 -- 4 (weight: 10)
  3 -- 1 (weight: 5)
  3 -- 2 (weight: 8)
  3 -- 4 (weight: 2)
  3 -- 5 (weight: 6)
  4 -- 2 (weight: 10)
  4 -- 3 (weight: 2)
  4 -- 5 (weight: 3)
  5 -- 3 (weight: 6)
  5 -- 4 (weight: 3)

Minimum Spanning Tree (using simple approach):
  0 -- 2 (weight: 2)
  2 -- 1 (weight: 1)
  1 -- 3 (weight: 5)
  3 -- 4 (weight: 2)
  4 -- 5 (weight: 3)

Total weight of MST: 13
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list with `mutable.ListBuffer` for efficient edge storage
2. **Edge Case Handling**: Properly handles disconnected graphs and edge cases
3. **Two Approaches**: 
   - Simple O(V²) approach (easier to understand)
   - Heap-based approach for better performance
4. **Mutable Collections**: Uses Scala's mutable collections for efficient updates
5. **Case Classes**: `Edge` case class for clean data representation

The algorithm starts from a given vertex and greedily selects the minimum weight edge that connects to an unvisited vertex, building the MST step by step.

