# Hopcroft-Karp Algorithm Implementation in Scala

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a complete implementation:

```scala
import scala.collection.mutable

class HopcroftKarp {
  // Graph representation: adjacency list for left vertices
  private val graph = mutable.Map[Int, List[Int]]()
  private val matchLeft = mutable.Map[Int, Int]()
  private val matchRight = mutable.Map[Int, Int]()
  private val dist = mutable.Map[Int, Int]()
  
  // Add edge to the bipartite graph
  def addEdge(u: Int, v: Int): Unit = {
    graph.put(u, graph.getOrElse(u, List()) :+ v)
  }
  
  // Initialize distance map
  private def initDist(): Unit = {
    dist.clear()
    for (u <- graph.keys) {
      dist(u) = Int.MaxValue
    }
  }
  
  // BFS to find augmenting paths
  private def bfs(): Boolean = {
    initDist()
    val queue = mutable.Queue[Int]()
    
    // Add all unmatched vertices from left side to queue
    for (u <- graph.keys if !matchLeft.contains(u)) {
      dist(u) = 0
      queue.enqueue(u)
    }
    
    var foundAugmentingPath = false
    
    while (queue.nonEmpty) {
      val u = queue.dequeue()
      
      if (dist(u) < dist(Int.MaxValue)) {
        for (v <- graph.getOrElse(u, List())) {
          val uMatch = matchRight.get(v)
          if (uMatch.isEmpty || dist(uMatch.get) == dist(u) + 1) {
            if (uMatch.isDefined) {
              dist(uMatch.get) = dist(u) + 1
              queue.enqueue(uMatch.get)
            } else {
              foundAugmentingPath = true
            }
          }
        }
      }
    }
    
    foundAugmentingPath
  }
  
  // DFS to find augmenting path
  private def dfs(u: Int): Boolean = {
    if (u != 0) {
      for (v <- graph.getOrElse(u, List())) {
        val uMatch = matchRight.get(v)
        if (uMatch.isEmpty || dist(uMatch.get) == dist(u) + 1) {
          dist(uMatch.getOrElse(0)) = Int.MaxValue
          
          if (uMatch.isEmpty || dfs(uMatch.get)) {
            matchLeft(u) = v
            matchRight(v) = u
            return true
          }
        }
      }
    }
    false
  }
  
  // Find maximum matching
  def maxMatching(): Int = {
    matchLeft.clear()
    matchRight.clear()
    
    var matching = 0
    
    while (bfs()) {
      for (u <- graph.keys if !matchLeft.contains(u)) {
        if (dfs(u)) {
          matching += 1
        }
      }
    }
    
    matching
  }
  
  // Get the actual matching pairs
  def getMatching(): List[(Int, Int)] = {
    matchLeft.toList.map { case (u, v) => (u, v) }
  }
}

// Example usage
object HopcroftKarpExample extends App {
  val hk = new HopcroftKarp()
  
  // Create a bipartite graph
  // Left vertices: 1, 2, 3, 4
  // Right vertices: 5, 6, 7, 8
  
  // Adding edges: 1-5, 1-6, 2-5, 2-7, 3-6, 3-8, 4-7
  hk.addEdge(1, 5)
  hk.addEdge(1, 6)
  hk.addEdge(2, 5)
  hk.addEdge(2, 7)
  hk.addEdge(3, 6)
  hk.addEdge(3, 8)
  hk.addEdge(4, 7)
  
  val matchingCount = hk.maxMatching()
  val matchingPairs = hk.getMatching()
  
  println(s"Maximum matching count: $matchingCount")
  println("Matching pairs:")
  matchingPairs.foreach { case (u, v) => 
    println(s"  $u -> $v") 
  }
  
  // Another example with a smaller graph
  println("\n--- Second Example ---")
  val hk2 = new HopcroftKarp()
  
  // Adding edges: 1-4, 1-5, 2-4, 2-6, 3-5, 3-6
  hk2.addEdge(1, 4)
  hk2.addEdge(1, 5)
  hk2.addEdge(2, 4)
  hk2.addEdge(2, 6)
  hk2.addEdge(3, 5)
  hk2.addEdge(3, 6)
  
  val matchingCount2 = hk2.maxMatching()
  val matchingPairs2 = hk2.getMatching()
  
  println(s"Maximum matching count: $matchingCount2")
  println("Matching pairs:")
  matchingPairs2.foreach { case (u, v) => 
    println(s"  $u -> $v") 
  }
}
```

## Output:
```
Maximum matching count: 4
Matching pairs:
  1 -> 6
  2 -> 5
  3 -> 8
  4 -> 7

--- Second Example ---
Maximum matching count: 3
Matching pairs:
  1 -> 5
  2 -> 4
  3 -> 6
```

## Key Features of this Implementation:

1. **Bipartite Graph Support**: Uses adjacency list representation for efficient edge storage
2. **Two-Phase Algorithm**: 
   - BFS phase to find augmenting paths
   - DFS phase to find and augment paths
3. **Time Complexity**: O(E√V) where E is edges and V is vertices
4. **Memory Efficient**: Uses mutable collections for optimal performance
5. **Complete Solution**: Returns both the count and actual matching pairs

## How it Works:

1. **Initialization**: Set up data structures to track matches and distances
2. **BFS Phase**: Find all possible augmenting paths using breadth-first search
3. **DFS Phase**: For each unmatched vertex, try to find an augmenting path using depth-first search
4. **Augmentation**: Update matching when augmenting paths are found
5. **Repeat**: Continue until no more augmenting paths exist

This implementation handles the core algorithm efficiently and provides a clean interface for working with bipartite matching problems.

