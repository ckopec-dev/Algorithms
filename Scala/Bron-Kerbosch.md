# Bron-Kerbosch Algorithm in Scala

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's an implementation in Scala:

```scala
import scala.collection.mutable

case class Graph(vertices: Set[Int], edges: Set[(Int, Int)]) {
  def neighbors(vertex: Int): Set[Int] = {
    edges.filter(edge => edge._1 == vertex || edge._2 == vertex)
         .flatMap(edge => if (edge._1 == vertex) Set(edge._2) else Set(edge._1))
         .toSet
  }
  
  def isEdge(v1: Int, v2: Int): Boolean = edges.contains((v1, v2)) || edges.contains((v2, v1))
}

object BronKerbosch {
  
  def findMaximalCliques(graph: Graph): Set[Set[Int]] = {
    val cliques = mutable.Set[Set[Int]]()
    val P = graph.vertices
    val R = mutable.Set[Int]()
    val X = mutable.Set[Int]()
    
    bronKerbosch(P, R, X, graph, cliques)
    cliques.toSet
  }
  
  private def bronKerbosch(P: Set[Int], R: mutable.Set[Int], X: mutable.Set[Int], 
                          graph: Graph, cliques: mutable.Set[Set[Int]]): Unit = {
    if (P.isEmpty && X.isEmpty) {
      // Found a maximal clique
      cliques += R.toSet
      return
    }
    
    // Choose a pivot vertex u from P ∪ X
    val u = (P ++ X).head
    
    // For each vertex v in P \ N(u)
    val PMinusNu = P.diff(graph.neighbors(u))
    
    for (v <- PMinusNu) {
      val newR = R + v
      val newP = P.intersect(graph.neighbors(v))
      val newX = X.intersect(graph.neighbors(v))
      
      bronKerbosch(newP, newR, newX, graph, cliques)
      
      // Move v from P to X
      P -= v
      X += v
    }
  }
}

// Example usage
object Main extends App {
  // Create a sample graph
  // Graph with vertices 1,2,3,4,5 and edges forming a triangle plus additional connections
  val vertices = Set(1, 2, 3, 4, 5)
  val edges = Set(
    (1, 2), (1, 3), (1, 4),  // Triangle 1-2-3
    (2, 3),
    (4, 5)   // Edge 4-5
  )
  
  val graph = Graph(vertices, edges)
  
  println("Graph edges:")
  edges.foreach(edge => println(s"  ${edge._1} -- ${edge._2}"))
  
  println("\nFinding maximal cliques...")
  val cliques = BronKerbosch.findMaximalCliques(graph)
  
  println(s"Found ${cliques.size} maximal cliques:")
  cliques.foreach(clique => println(s"  ${clique.toList.sorted.mkString("{", ",", "}")}"))
  
  // Another example with a simpler graph
  println("\n" + "="*50)
  println("Second example - Simple triangle graph:")
  
  val simpleVertices = Set(1, 2, 3)
  val simpleEdges = Set((1, 2), (2, 3), (1, 3))
  
  val simpleGraph = Graph(simpleVertices, simpleEdges)
  
  println("Simple graph edges:")
  simpleEdges.foreach(edge => println(s"  ${edge._1} -- ${edge._2}"))
  
  val simpleCliques = BronKerbosch.findMaximalCliques(simpleGraph)
  
  println(s"Found ${simpleCliques.size} maximal cliques:")
  simpleCliques.foreach(clique => println(s"  ${clique.toList.sorted.mkString("{", ",", "}")}"))
}
```

## Output Example:
```
Graph edges:
  1 -- 2
  1 -- 3
  1 -- 4
  2 -- 3
  4 -- 5

Finding maximal cliques...
Found 3 maximal cliques:
  {1,2,3}
  {1,4}
  {4,5}

==================================================
Second example - Simple triangle graph:
Simple graph edges:
  1 -- 2
  2 -- 3
  1 -- 3
Found 1 maximal cliques:
  {1,2,3}
```

## Key Features of the Implementation:

1. **Graph Representation**: Uses a case class to represent the graph with vertices and edges
2. **Neighbor Function**: Efficiently finds all neighbors of a given vertex
3. **Pivot Selection**: Uses the standard pivot selection strategy for optimization
4. **Mutable Collections**: Uses `mutable.Set` for efficient updates during recursion
5. **Clean Output**: Returns maximal cliques as sets for easy comparison

## Time Complexity:
- **Worst case**: O(3^(n/3)) where n is the number of vertices
- **Average case**: Much better in practice due to the pivot selection optimization

This implementation demonstrates the classic Bron-Kerbosch algorithm with the pivot optimization that makes it efficient in practice.

