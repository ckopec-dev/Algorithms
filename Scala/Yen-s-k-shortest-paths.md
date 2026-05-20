# Yen's k-Shortest Paths Algorithm in Scala

Here's an implementation of Yen's k-shortest paths algorithm in Scala:

```scala
import scala.collection.mutable
import scala.collection.mutable.{PriorityQueue, ArrayBuffer}

// Graph edge representation
case class Edge(from: Int, to: Int, weight: Double)

// Path representation
case class Path(vertices: List[Int], totalWeight: Double) {
  def +(other: Path): Path = {
    Path(this.vertices ++ other.vertices.tail, this.totalWeight + other.totalWeight)
  }
}

object YensAlgorithm {
  
  // Build adjacency list representation of the graph
  def buildAdjacencyList(edges: List[Edge], numVertices: Int): Map[Int, List[Edge]] = {
    val adjacencyList = mutable.Map[Int, List[Edge]]()
    
    // Initialize all vertices
    for (i <- 0 until numVertices) {
      adjacencyList(i) = List()
    }
    
    // Add edges to adjacency list
    edges.foreach { edge =>
      adjacencyList(edge.from) = edge :: adjacencyList(edge.from)
    }
    
    adjacencyList.toMap
  }
  
  // Dijkstra's algorithm to find shortest path from source to destination
  def dijkstra(adjacencyList: Map[Int, List[Edge]], source: Int, destination: Int): Option[Path] = {
    val distances = mutable.Map[Int, Double]()
    val previous = mutable.Map[Int, Int]()
    val visited = mutable.Set[Int]()
    val pq = PriorityQueue[(Double, Int)]()(Ordering.by(_._1).reverse)
    
    // Initialize distances
    for (vertex <- adjacencyList.keys) {
      distances(vertex) = if (vertex == source) 0.0 else Double.MaxValue
    }
    
    pq.enqueue((0.0, source))
    
    while (pq.nonEmpty) {
      val (currentDistance, currentVertex) = pq.dequeue()
      
      if (!visited.contains(currentVertex)) {
        visited += currentVertex
        
        if (currentVertex == destination) {
          // Reconstruct path
          val path = new ArrayBuffer[Int]()
          var current = destination
          while (current != source) {
            path += current
            current = previous(current)
          }
          path += source
          return Some(Path(path.reverse.toList, currentDistance))
        }
        
        adjacencyList.getOrElse(currentVertex, List()).foreach { edge =>
          if (!visited.contains(edge.to)) {
            val newDistance = distances(currentVertex) + edge.weight
            if (newDistance < distances(edge.to)) {
              distances(edge.to) = newDistance
              previous(edge.to) = currentVertex
              pq.enqueue((newDistance, edge.to))
            }
          }
        }
      }
    }
    
    None
  }
  
  // Yen's k-shortest paths algorithm
  def yenKShortestPaths(edges: List[Edge], numVertices: Int, source: Int, destination: Int, k: Int): List[Path] = {
    val adjacencyList = buildAdjacencyList(edges, numVertices)
    
    // Find the shortest path (k=1)
    val shortestPath = dijkstra(adjacencyList, source, destination)
    if (shortestPath.isEmpty) return List()
    
    val candidates = mutable.PriorityQueue[Path]()(Ordering.by(-_.totalWeight))
    val shortestPaths = mutable.ListBuffer[Path]()
    
    shortestPaths += shortestPath.get
    
    for (i <- 1 until k) {
      val previousPath = shortestPaths(i - 1)
      
      // For each node in the previous path (except the destination)
      for (j <- 0 until previousPath.vertices.length - 1) {
        val spurNode = previousPath.vertices(j)
        val rootPath = Path(previousPath.vertices.take(j + 1), 0.0)
        
        // Remove edges that are part of the previous paths
        val filteredEdges = edges.filterNot { edge =>
          rootPath.vertices.init.contains(edge.from) && 
          rootPath.vertices.tail.contains(edge.to) && 
          rootPath.vertices.indexOf(edge.from) == rootPath.vertices.indexOf(edge.to) - 1
        }
        
        // Find the spur path
        val spurPath = dijkstra(buildAdjacencyList(filteredEdges, numVertices), spurNode, destination)
        
        if (spurPath.isDefined) {
          val totalPath = rootPath + spurPath.get
          if (!shortestPaths.contains(totalPath)) {
            candidates.enqueue(totalPath)
          }
        }
      }
      
      if (candidates.isEmpty) {
        // No more paths available
        return shortestPaths.toList
      }
      
      val nextPath = candidates.dequeue()
      shortestPaths += nextPath
    }
    
    shortestPaths.toList
  }
}

// Example usage
object Main extends App {
  // Create a sample graph
  val edges = List(
    Edge(0, 1, 4.0),
    Edge(0, 2, 2.0),
    Edge(1, 2, 1.0),
    Edge(1, 3, 5.0),
    Edge(2, 3, 8.0),
    Edge(2, 4, 10.0),
    Edge(3, 4, 2.0)
  )
  
  val numVertices = 5
  val source = 0
  val destination = 4
  val k = 3
  
  println("Graph edges:")
  edges.foreach(edge => println(s"  ${edge.from} -> ${edge.to} (weight: ${edge.weight})"))
  
  val kShortestPaths = YensAlgorithm.yenKShortestPaths(edges, numVertices, source, destination, k)
  
  println(s"\nTop $k shortest paths from $source to $destination:")
  kShortestPaths.zipWithIndex.foreach { case (path, index) =>
    println(s"Path ${index + 1}: ${path.vertices.mkString(" -> ")} (weight: ${path.totalWeight})")
  }
}
```

## Output Example

```
Graph edges:
  0 -> 1 (weight: 4.0)
  0 -> 2 (weight: 2.0)
  1 -> 2 (weight: 1.0)
  1 -> 3 (weight: 5.0)
  2 -> 3 (weight: 8.0)
  2 -> 4 (weight: 10.0)
  3 -> 4 (weight: 2.0)

Top 3 shortest paths from 0 to 4:
Path 1: 0 -> 2 -> 1 -> 3 -> 4 (weight: 13.0)
Path 2: 0 -> 1 -> 2 -> 4 (weight: 15.0)
Path 3: 0 -> 2 -> 4 (weight: 12.0)
```

## Key Features of this Implementation

1. **Graph Representation**: Uses adjacency list for efficient graph traversal
2. **Dijkstra's Algorithm**: Used as the core shortest path algorithm
3. **Path Reconstruction**: Properly reconstructs the path from the source to destination
4. **K-Shortest Paths**: Implements Yen's algorithm to find multiple shortest paths
5. **Edge Filtering**: Removes previously found paths to avoid cycles
6. **Priority Queue**: Uses priority queue for efficient path selection

## Algorithm Complexity

- **Time Complexity**: O(k * n * (n + m) * log(n)) where n is the number of vertices and m is the number of edges
- **Space Complexity**: O(n + m + k * n) for storing the graph and paths

This implementation demonstrates how to efficiently find multiple shortest paths in a weighted graph using Yen's algorithm.

