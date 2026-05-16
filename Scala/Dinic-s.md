# Dinic's Algorithm Implementation in Scala

Dinic's algorithm is a fast algorithm for computing maximum flow in a flow network. Here's a complete implementation in Scala:

```scala
import scala.collection.mutable

class FlowEdge(val from: Int, val to: Int, var capacity: Long, var flow: Long = 0) {
  def residualCapacityTo(to: Int): Long = {
    if (to == this.to) capacity - flow
    else if (to == this.from) flow
    else 0
  }
  
  def addFlowTo(to: Int, amount: Long): Unit = {
    if (to == this.to) flow += amount
    else if (to == this.from) flow -= amount
  }
  
  override def toString: String = s"Edge($from -> $to, capacity=$capacity, flow=$flow)"
}

class DinicSolver(vertices: Int) {
  private val graph = mutable.Map[Int, mutable.ListBuffer[FlowEdge]]()
  private val level = new Array[Int](vertices)
  private val iter = new Array[Int](vertices)
  
  // Add edge to the graph
  def addEdge(from: Int, to: Int, capacity: Long): Unit = {
    graph.getOrElseUpdate(from, mutable.ListBuffer[FlowEdge]()) += new FlowEdge(from, to, capacity)
    graph.getOrElseUpdate(to, mutable.ListBuffer[FlowEdge]()) += new FlowEdge(to, from, 0)
  }
  
  // Build level graph using BFS
  private def buildLevelGraph(source: Int, sink: Int): Boolean = {
    java.util.Arrays.fill(level, -1)
    val queue = mutable.Queue[Int]()
    level(source) = 0
    queue.enqueue(source)
    
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      graph.getOrElse(current, mutable.ListBuffer[FlowEdge]()).foreach { edge =>
        val next = edge.to
        if (level(next) == -1 && edge.residualCapacityTo(next) > 0) {
          level(next) = level(current) + 1
          queue.enqueue(next)
        }
      }
    }
    
    level(sink) != -1
  }
  
  // Find blocking flow using DFS
  private def findBlockingFlow(source: Int, sink: Int, minCapacity: Long): Long = {
    if (source == sink) return minCapacity
    
    var currentFlow = 0L
    val currentIter = iter(source)
    
    while (currentIter < graph.getOrElse(source, mutable.ListBuffer[FlowEdge]()).size) {
      val edge = graph(source)(currentIter)
      val next = edge.to
      
      if (level(next) == level(source) + 1 && edge.residualCapacityTo(next) > 0) {
        val bottleneck = findBlockingFlow(next, sink, math.min(minCapacity, edge.residualCapacityTo(next)))
        
        if (bottleneck > 0) {
          edge.addFlowTo(next, bottleneck)
          currentFlow += bottleneck
          minCapacity -= bottleneck
          
          if (minCapacity == 0) {
            iter(source) = currentIter
            return currentFlow
          }
        }
      }
      
      iter(source) += 1
    }
    
    currentFlow
  }
  
  // Main Dinic's algorithm
  def maxFlow(source: Int, sink: Int): Long = {
    var maxFlow = 0L
    
    while (buildLevelGraph(source, sink)) {
      java.util.Arrays.fill(iter, 0)
      var flow = findBlockingFlow(source, sink, Long.MaxValue)
      while (flow > 0) {
        maxFlow += flow
        flow = findBlockingFlow(source, sink, Long.MaxValue)
      }
    }
    
    maxFlow
  }
  
  // Get all edges in the graph
  def getEdges: List[FlowEdge] = {
    graph.values.flatten.toList
  }
}

// Example usage
object DinicExample extends App {
  // Create a graph with 6 vertices (0 to 5)
  val solver = new DinicSolver(6)
  
  // Add edges with capacities
  solver.addEdge(0, 1, 10)
  solver.addEdge(0, 2, 10)
  solver.addEdge(1, 2, 2)
  solver.addEdge(1, 3, 4)
  solver.addEdge(1, 4, 8)
  solver.addEdge(2, 4, 9)
  solver.addEdge(3, 5, 10)
  solver.addEdge(4, 5, 10)
  
  val source = 0
  val sink = 5
  
  val maxFlow = solver.maxFlow(source, sink)
  
  println(s"Maximum flow from vertex $source to vertex $sink: $maxFlow")
  
  // Print the edges with their flows
  println("\nEdges with flows:")
  solver.getEdges.foreach { edge =>
    if (edge.flow > 0) {
      println(s"${edge.from} -> ${edge.to}: capacity=${edge.capacity}, flow=${edge.flow}")
    }
  }
}
```

## How it works:

1. **Graph Representation**: Uses adjacency lists with `FlowEdge` objects that track capacity and current flow
2. **Level Graph**: BFS is used to build a level graph where each node is assigned a level based on distance from source
3. **Blocking Flow**: DFS is used to find a blocking flow in the level graph
4. **Repeat**: Continue until no augmenting path exists

## Time Complexity:
- **Time**: O(V²E) in worst case, but typically much better in practice
- **Space**: O(V + E)

## Key Features:
- Handles multiple edges between same vertices
- Supports both directed and undirected graphs
- Efficient implementation using level graph
- Returns maximum flow value and can be extended to track actual flow

The algorithm is particularly efficient for sparse graphs and is widely used in practice for maximum flow problems.

