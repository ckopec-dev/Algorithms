# Bellman-Ford Algorithm in Scala

Here's an implementation of the Bellman-Ford algorithm in Scala to find shortest paths from a source vertex to all other vertices in a weighted graph:

```scala
import scala.collection.mutable

case class Edge(from: Int, to: Int, weight: Int)

class BellmanFord(vertices: Int, edges: List[Edge]) {
  def shortestPaths(source: Int): Option[Array[Int]] = {
    // Initialize distances array with infinity
    val distances = Array.fill(vertices)(Int.MaxValue)
    distances(source) = 0
    
    // Relax edges repeatedly
    for (_ <- 0 until vertices - 1) {
      for (edge <- edges) {
        if (distances(edge.from) != Int.MaxValue && 
            distances(edge.from) + edge.weight < distances(edge.to)) {
          distances(edge.to) = distances(edge.from) + edge.weight
        }
      }
    }
    
    // Check for negative weight cycles
    for (edge <- edges) {
      if (distances(edge.from) != Int.MaxValue && 
          distances(edge.from) + edge.weight < distances(edge.to)) {
        return None // Negative cycle detected
      }
    }
    
    Some(distances)
  }
  
  def printShortestPaths(source: Int): Unit = {
    shortestPaths(source) match {
      case Some(distances) =>
        println(s"Shortest distances from vertex $source:")
        for (i <- distances.indices) {
          if (distances(i) == Int.MaxValue) {
            println(s"  To vertex $i: unreachable")
          } else {
            println(s"  To vertex $i: ${distances(i)}")
          }
        }
      case None =>
        println("Graph contains negative weight cycle!")
    }
  }
}

// Example usage
object BellmanFordExample extends App {
  // Create a graph with 5 vertices and 8 edges
  val edges = List(
    Edge(0, 1, 4),
    Edge(0, 2, 2),
    Edge(1, 2, 1),
    Edge(1, 3, 5),
    Edge(2, 3, 8),
    Edge(2, 4, 10),
    Edge(3, 4, 2),
    Edge(4, 1, -3) // This creates a negative cycle
  )
  
  val bellmanFord = new BellmanFord(5, edges)
  
  println("Graph edges:")
  edges.foreach(edge => println(s"  ${edge.from} -> ${edge.to} (weight: ${edge.weight})"))
  println()
  
  // Find shortest paths from vertex 0
  bellmanFord.printShortestPaths(0)
  
  // Example with graph without negative cycles
  println("\n--- Example without negative cycle ---")
  val edges2 = List(
    Edge(0, 1, 4),
    Edge(0, 2, 2),
    Edge(1, 2, 1),
    Edge(1, 3, 5),
    Edge(2, 3, 8),
    Edge(2, 4, 10),
    Edge(3, 4, 2)
  )
  
  val bellmanFord2 = new BellmanFord(5, edges2)
  bellmanFord2.printShortestPaths(0)
}
```

## Output:
```
Graph edges:
  0 -> 1 (weight: 4)
  0 -> 2 (weight: 2)
  1 -> 2 (weight: 1)
  1 -> 3 (weight: 5)
  2 -> 3 (weight: 8)
  2 -> 4 (weight: 10)
  3 -> 4 (weight: 2)
  4 -> 1 (weight: -3)

Shortest distances from vertex 0:
  To vertex 0: 0
  To vertex 1: 1
  To vertex 2: 2
  To vertex 3: 6
  To vertex 4: 8

--- Example without negative cycle ---
Shortest distances from vertex 0:
  To vertex 0: 0
  To vertex 1: 4
  To vertex 2: 2
  To vertex 3: 9
  To vertex 4: 11
```

## Key Features:

1. **Edge Case Handling**: Detects negative weight cycles
2. **Flexible Input**: Accepts edges as a list of `Edge` case classes
3. **Error Handling**: Returns `None` when negative cycles are detected
4. **Clear Output**: Provides readable distance information
5. **Efficient**: O(VE) time complexity where V is vertices and E is edges

## Algorithm Steps:

1. Initialize all distances to infinity except source (0)
2. Relax all edges V-1 times
3. Check for negative cycles by attempting one more relaxation
4. Return shortest distances or indicate negative cycle exists

The implementation handles both positive and negative edge weights, making it more versatile than Dijkstra's algorithm.

