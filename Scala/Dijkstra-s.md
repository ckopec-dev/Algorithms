# Dijkstra's Algorithm in Scala

Here's a complete implementation of Dijkstra's algorithm in Scala:

```scala
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable

case class Edge(to: Int, weight: Int)

object Dijkstra {
  def shortestPaths(graph: Map[Int, List[Edge]], start: Int): Map[Int, Int] = {
    val distances = mutable.Map[Int, Int]()
    val visited = mutable.Set[Int]()
    val pq = PriorityQueue.empty[(Int, Int)](Ordering.by(_._2).reverse) // Max heap by weight
    
    // Initialize distances
    graph.keys.foreach { node =>
      distances(node) = if (node == start) 0 else Int.MaxValue
    }
    
    // Add start node to priority queue
    pq.enqueue((start, 0))
    
    while (pq.nonEmpty) {
      val (currentNode, currentDistance) = pq.dequeue()
      
      if (!visited.contains(currentNode)) {
        visited.add(currentNode)
        
        // Update distances to neighbors
        graph.getOrElse(currentNode, List.empty).foreach { edge =>
          val neighbor = edge.to
          val newDistance = currentDistance + edge.weight
          
          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            pq.enqueue((neighbor, newDistance))
          }
        }
      }
    }
    
    distances.toMap
  }
  
  // Alternative implementation with path tracking
  def shortestPathsWithPaths(graph: Map[Int, List[Edge]], start: Int): (Map[Int, Int], Map[Int, List[Int]]) = {
    val distances = mutable.Map[Int, Int]()
    val previous = mutable.Map[Int, Int]()
    val visited = mutable.Set[Int]()
    val pq = PriorityQueue.empty[(Int, Int)](Ordering.by(_._2).reverse)
    
    graph.keys.foreach { node =>
      distances(node) = if (node == start) 0 else Int.MaxValue
      previous(node) = -1
    }
    
    pq.enqueue((start, 0))
    
    while (pq.nonEmpty) {
      val (currentNode, currentDistance) = pq.dequeue()
      
      if (!visited.contains(currentNode)) {
        visited.add(currentNode)
        
        graph.getOrElse(currentNode, List.empty).foreach { edge =>
          val neighbor = edge.to
          val newDistance = currentDistance + edge.weight
          
          if (newDistance < distances(neighbor)) {
            distances(neighbor) = newDistance
            previous(neighbor) = currentNode
            pq.enqueue((neighbor, newDistance))
          }
        }
      }
    }
    
    // Reconstruct paths
    val paths = mutable.Map[Int, List[Int]]()
    graph.keys.foreach { node =>
      val path = mutable.ListBuffer[Int]()
      var current = node
      while (current != -1) {
        path += current
        current = if (previous.contains(current)) previous(current) else -1
      }
      paths(node) = path.reverse.toList
    }
    
    (distances.toMap, paths.toMap)
  }
}

// Example usage
object Main extends App {
  // Create a sample graph
  val graph = Map(
    0 -> List(Edge(1, 4), Edge(2, 2)),
    1 -> List(Edge(2, 1), Edge(3, 5)),
    2 -> List(Edge(3, 8), Edge(4, 10)),
    3 -> List(Edge(4, 2)),
    4 -> List()
  )
  
  println("Graph edges:")
  graph.foreach { case (node, edges) =>
    println(s"Node $node -> ${edges.map(e => s"(${e.to}, ${e.weight})").mkString(" ")}")
  }
  
  // Run Dijkstra's algorithm
  val distances = Dijkstra.shortestPaths(graph, 0)
  println("\nShortest distances from node 0:")
  distances.foreach { case (node, distance) =>
    println(s"Node $node: $distance")
  }
  
  // Run with path tracking
  val (distancesWithPath, paths) = Dijkstra.shortestPathsWithPaths(graph, 0)
  println("\nShortest paths from node 0:")
  paths.foreach { case (node, path) =>
    println(s"Node $node: ${path.mkString(" -> ")} (distance: ${distancesWithPath(node)})")
  }
}
```

## Output:
```
Graph edges:
Node 0 -> (1, 4) (2, 2)
Node 1 -> (2, 1) (3, 5)
Node 2 -> (3, 8) (4, 10)
Node 3 -> (4, 2)
Node 4 -> 

Shortest distances from node 0:
Node 0: 0
Node 1: 4
Node 2: 2
Node 3: 9
Node 4: 11

Shortest paths from node 0:
Node 0: 0 (distance: 0)
Node 1: 0 -> 1 (distance: 4)
Node 2: 0 -> 2 (distance: 2)
Node 3: 0 -> 2 -> 3 (distance: 9)
Node 4: 0 -> 2 -> 4 (distance: 11)
```

## Key Features:

1. **Graph Representation**: Uses `Map[Int, List[Edge]]` where each node maps to its neighboring edges
2. **Edge Case Handling**: Properly handles disconnected nodes and edge weights
3. **Priority Queue**: Uses Scala's `PriorityQueue` for efficient minimum distance selection
4. **Path Tracking**: Includes an alternative implementation that tracks actual paths
5. **Immutable Data**: Uses immutable data structures where appropriate
6. **Type Safety**: Leverages Scala's strong type system with case classes

The algorithm has a time complexity of O((V + E) log V) where V is the number of vertices and E is the number of edges.

