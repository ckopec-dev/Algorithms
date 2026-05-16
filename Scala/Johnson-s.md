# Johnson's Algorithm Implementation in Scala

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, even when negative edge weights are present (but no negative cycles). Here's a complete implementation in Scala:

```scala
import scala.collection.mutable
import scala.collection.mutable.{PriorityQueue, ArrayBuffer}

case class Edge(from: Int, to: Int, weight: Int)

class JohnsonsAlgorithm {
  
  // Build adjacency list representation of the graph
  def buildAdjacencyList(edges: List[Edge], vertices: Int): Map[Int, List[Edge]] = {
    val adjList = mutable.Map[Int, List[Edge]]()
    
    // Initialize all vertices
    for (i <- 0 until vertices) {
      adjList(i) = List.empty[Edge]
    }
    
    // Add edges to adjacency list
    edges.foreach { edge =>
      adjList(edge.from) = edge :: adjList(edge.from)
    }
    
    adjList.toMap
  }
  
  // Bellman-Ford algorithm to detect negative cycles and compute distances
  def bellmanFord(vertices: Int, edges: List[Edge], source: Int): Option[Array[Int]] = {
    val dist = Array.fill(vertices)(Int.MaxValue)
    dist(source) = 0
    
    // Relax edges repeatedly
    for (_ <- 0 until vertices - 1) {
      for (edge <- edges) {
        if (dist(edge.from) != Int.MaxValue && dist(edge.from) + edge.weight < dist(edge.to)) {
          dist(edge.to) = dist(edge.from) + edge.weight
        }
      }
    }
    
    // Check for negative cycles
    for (edge <- edges) {
      if (dist(edge.from) != Int.MaxValue && dist(edge.from) + edge.weight < dist(edge.to)) {
        return None // Negative cycle detected
      }
    }
    
    Some(dist)
  }
  
  // Dijkstra's algorithm for single source shortest path
  def dijkstra(vertices: Int, adjList: Map[Int, List[Edge]], source: Int): Array[Int] = {
    val dist = Array.fill(vertices)(Int.MaxValue)
    val visited = Array.fill(vertices)(false)
    val pq = PriorityQueue[(Int, Int)]()(Ordering.by(_._2).reverse) // (vertex, distance)
    
    dist(source) = 0
    pq.enqueue((source, 0))
    
    while (pq.nonEmpty) {
      val (u, _) = pq.dequeue()
      
      if (!visited(u)) {
        visited(u) = true
        
        adjList.getOrElse(u, List.empty).foreach { edge =>
          if (!visited(edge.to) && dist(u) != Int.MaxValue && dist(u) + edge.weight < dist(edge.to)) {
            dist(edge.to) = dist(u) + edge.weight
            pq.enqueue((edge.to, dist(edge.to)))
          }
        }
      }
    }
    
    dist
  }
  
  // Main Johnson's algorithm implementation
  def johnsonsAlgorithm(vertices: Int, edges: List[Edge]): Option[Array[Array[Int]]] = {
    // Step 1: Add a new vertex (0) connected to all other vertices with weight 0
    val extendedEdges = edges ++ (0 until vertices).map(i => Edge(0, i, 0))
    
    // Step 2: Run Bellman-Ford from the new vertex to compute h values
    val h = bellmanFord(vertices + 1, extendedEdges, 0)
    
    if (h.isEmpty) {
      return None // Negative cycle detected
    }
    
    val hValues = h.get.drop(1) // Remove the first element (0 vertex)
    
    // Step 3: Reweight all edges
    val reweightedEdges = edges.map { edge =>
      val newWeight = edge.weight + hValues(edge.from) - hValues(edge.to)
      Edge(edge.from, edge.to, newWeight)
    }
    
    // Step 4: Build adjacency list for reweighted graph
    val adjList = buildAdjacencyList(reweightedEdges, vertices)
    
    // Step 5: Run Dijkstra from each vertex
    val allDistances = Array.ofDim[Int](vertices, vertices)
    
    for (i <- 0 until vertices) {
      val distances = dijkstra(vertices, adjList, i)
      allDistances(i) = distances
    }
    
    // Step 6: Apply reverse transformation to get final distances
    for (i <- 0 until vertices) {
      for (j <- 0 until vertices) {
        allDistances(i)(j) = allDistances(i)(j) - hValues(i) + hValues(j)
      }
    }
    
    Some(allDistances)
  }
}

// Example usage
object JohnsonsAlgorithmExample extends App {
  val johnson = new JohnsonsAlgorithm()
  
  // Example graph with negative edge weights
  val vertices = 4
  val edges = List(
    Edge(0, 1, 3),
    Edge(0, 2, 8),
    Edge(0, 3, -4),
    Edge(1, 2, 7),
    Edge(1, 3, 1),
    Edge(2, 1, -4),
    Edge(3, 0, 2),
    Edge(3, 2, 6)
  )
  
  println("Input edges:")
  edges.foreach(edge => println(s"  ${edge.from} -> ${edge.to} (weight: ${edge.weight})"))
  
  val result = johnson.johnsonsAlgorithm(vertices, edges)
  
  result match {
    case Some(distances) =>
      println("\nAll pairs shortest paths:")
      for (i <- distances.indices) {
        println(s"From vertex $i: ${distances(i).mkString(" ")}")
      }
      
    case None =>
      println("Negative cycle detected in the graph!")
  }
  
  // Another example with negative cycle
  println("\n--- Testing with negative cycle ---")
  val edgesWithNegativeCycle = List(
    Edge(0, 1, 1),
    Edge(1, 2, -3),
    Edge(2, 0, 1)
  )
  
  val result2 = johnson.johnsonsAlgorithm(3, edgesWithNegativeCycle)
  result2 match {
    case Some(_) => println("No negative cycle detected")
    case None => println("Negative cycle detected!")
  }
}
```

## Key Components Explained:

### 1. **Edge Case Handling**
- The algorithm first checks for negative cycles using Bellman-Ford
- Returns `None` if a negative cycle is detected

### 2. **Core Algorithm Steps**
1. **Add auxiliary vertex**: Connects to all vertices with weight 0
2. **Bellman-Ford**: Computes potential values (h) for reweighting
3. **Reweighting**: Adjusts edge weights to make them non-negative
4. **Dijkstra**: Runs Dijkstra from each vertex on reweighted graph
5. **Reverse transformation**: Converts back to original weights

### 3. **Time Complexity**
- **Overall**: O(V² log V + VE)
- **Space**: O(V + E)

### 4. **Output Format**
The algorithm returns a 2D array where `result(i)(j)` represents the shortest distance from vertex `i` to vertex `j`.

This implementation handles graphs with negative edge weights but no negative cycles, making it more versatile than Dijkstra's algorithm alone.

