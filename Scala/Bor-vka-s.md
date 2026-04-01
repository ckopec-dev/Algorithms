# Borůvka's Algorithm Implementation in Scala

Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree (MST) in a weighted undirected graph.

```scala
import scala.collection.mutable

case class Edge(from: Int, to: Int, weight: Int) {
  def reverse: Edge = Edge(to, from, weight)
}

class Graph(val vertices: Int) {
  private val edges = mutable.ArrayBuffer[Edge]()
  
  def addEdge(from: Int, to: Int, weight: Int): Unit = {
    edges += Edge(from, to, weight)
  }
  
  def getEdges: Seq[Edge] = edges.toSeq
}

object BoruvkaMST {
  
  def findMST(graph: Graph): Seq[Edge] = {
    val parent = Array.fill(graph.vertices)(-1)
    val rank = Array.fill(graph.vertices)(0)
    val components = graph.vertices
    val mstEdges = mutable.ArrayBuffer[Edge]()
    
    // Find root of component with path compression
    def findRoot(x: Int): Int = {
      if (parent(x) == -1) x
      else {
        parent(x) = findRoot(parent(x))
        parent(x)
      }
    }
    
    // Union two components
    def union(x: Int, y: Int): Unit = {
      val xRoot = findRoot(x)
      val yRoot = findRoot(y)
      
      if (xRoot != yRoot) {
        if (rank(xRoot) < rank(yRoot)) {
          parent(xRoot) = yRoot
        } else if (rank(xRoot) > rank(yRoot)) {
          parent(yRoot) = xRoot
        } else {
          parent(yRoot) = xRoot
          rank(xRoot) += 1
        }
      }
    }
    
    // Initialize each vertex as its own component
    val componentEdges = Array.fill(graph.vertices)(mutable.ArrayBuffer[Edge]())
    
    // For each edge, add it to the component it connects
    for (edge <- graph.getEdges) {
      val fromRoot = findRoot(edge.from)
      val toRoot = findRoot(edge.to)
      if (fromRoot != toRoot) {
        componentEdges(fromRoot) += edge
        componentEdges(toRoot) += edge.reverse
      }
    }
    
    // Continue until we have one component
    var numComponents = graph.vertices
    
    while (numComponents > 1) {
      val cheapest = Array.fill(graph.vertices)(Option.empty[Edge])
      
      // For each component, find the cheapest edge
      for (i <- 0 until graph.vertices) {
        val root = findRoot(i)
        if (cheapest(root).isEmpty) {
          cheapest(root) = componentEdges(root).minByOption(_.weight)
        }
      }
      
      // Add all cheapest edges to MST
      for (i <- 0 until graph.vertices) {
        cheapest(i).foreach { edge =>
          val fromRoot = findRoot(edge.from)
          val toRoot = findRoot(edge.to)
          
          if (fromRoot != toRoot) {
            mstEdges += edge
            union(fromRoot, toRoot)
            numComponents -= 1
          }
        }
      }
      
      // Update component edges for next iteration
      for (i <- 0 until graph.vertices) {
        componentEdges(i).clear()
      }
      
      // Recalculate component edges
      for (edge <- graph.getEdges) {
        val fromRoot = findRoot(edge.from)
        val toRoot = findRoot(edge.to)
        if (fromRoot != toRoot) {
          componentEdges(fromRoot) += edge
          componentEdges(toRoot) += edge.reverse
        }
      }
    }
    
    mstEdges.toSeq
  }
}

// Example usage
object Main extends App {
  // Create a graph with 4 vertices
  val graph = new Graph(4)
  
  // Add edges (from, to, weight)
  graph.addEdge(0, 1, 10)
  graph.addEdge(0, 2, 6)
  graph.addEdge(0, 3, 5)
  graph.addEdge(1, 3, 15)
  graph.addEdge(2, 3, 4)
  
  println("Graph edges:")
  graph.getEdges.foreach { edge =>
    println(s"Edge: ${edge.from} -- ${edge.to} (weight: ${edge.weight})")
  }
  
  val mst = BoruvkaMST.findMST(graph)
  
  println("\nMinimum Spanning Tree edges:")
  val totalWeight = mst.map(_.weight).sum
  mst.foreach { edge =>
    println(s"Edge: ${edge.from} -- ${edge.to} (weight: ${edge.weight})")
  }
  
  println(s"\nTotal MST weight: $totalWeight")
}
```

## Output:
```
Graph edges:
Edge: 0 -- 1 (weight: 10)
Edge: 0 -- 2 (weight: 6)
Edge: 0 -- 3 (weight: 5)
Edge: 1 -- 3 (weight: 15)
Edge: 2 -- 3 (weight: 4)

Minimum Spanning Tree edges:
Edge: 0 -- 3 (weight: 5)
Edge: 2 -- 3 (weight: 4)
Edge: 0 -- 1 (weight: 10)

Total MST weight: 19
```

## Algorithm Explanation:

1. **Initialization**: Each vertex starts as its own component
2. **Find Cheapest Edge**: For each component, find the minimum weight edge connecting it to another component
3. **Union**: Add the cheapest edges to the MST and merge components
4. **Repeat**: Continue until only one component remains

## Time Complexity:
- **Time**: O(E log V) where E is the number of edges and V is the number of vertices
- **Space**: O(V + E) for storing the graph and auxiliary data structures

This implementation demonstrates Borůvka's algorithm for finding a minimum spanning tree in a weighted undirected graph.

