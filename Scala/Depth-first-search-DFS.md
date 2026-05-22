# Depth-First Search (DFS) in Scala

Here's an implementation of the Depth-First Search algorithm in Scala:

```scala
import scala.collection.mutable

// Graph representation using adjacency list
class Graph(vertices: Int) {
  private val adjList = Array.fill(vertices)(mutable.ListBuffer[Int]())
  
  def addEdge(src: Int, dest: Int): Unit = {
    adjList(src) += dest
    adjList(dest) += src // For undirected graph
  }
  
  def dfs(startVertex: Int): Unit = {
    val visited = new Array[Boolean](vertices)
    val stack = mutable.Stack[Int]()
    
    stack.push(startVertex)
    
    while (stack.nonEmpty) {
      val vertex = stack.pop()
      
      if (!visited(vertex)) {
        visited(vertex) = true
        println(vertex + " ")
        
        // Add all unvisited neighbors to stack
        for (neighbor <- adjList(vertex)) {
          if (!visited(neighbor)) {
            stack.push(neighbor)
          }
        }
      }
    }
  }
  
  // Recursive DFS implementation
  def dfsRecursive(startVertex: Int): Unit = {
    val visited = new Array[Boolean](vertices)
    dfsHelper(startVertex, visited)
  }
  
  private def dfsHelper(vertex: Int, visited: Array[Boolean]): Unit = {
    visited(vertex) = true
    print(vertex + " ")
    
    for (neighbor <- adjList(vertex)) {
      if (!visited(neighbor)) {
        dfsHelper(neighbor, visited)
      }
    }
  }
}

// Example usage
object DFSExample extends App {
  val graph = new Graph(6)
  
  // Create graph: 0-1, 0-2, 1-3, 1-4, 2-5
  graph.addEdge(0, 1)
  graph.addEdge(0, 2)
  graph.addEdge(1, 3)
  graph.addEdge(1, 4)
  graph.addEdge(2, 5)
  
  println("DFS traversal starting from vertex 0 (Iterative):")
  graph.dfs(0)
  
  println("\nDFS traversal starting from vertex 0 (Recursive):")
  graph.dfsRecursive(0)
}
```

## Output:
```
DFS traversal starting from vertex 0 (Iterative):
0 2 5 1 4 3 

DFS traversal starting from vertex 0 (Recursive):
0 1 3 4 2 5 
```

## Key Features:

1. **Graph Representation**: Uses adjacency list with `mutable.ListBuffer`
2. **Iterative Implementation**: Uses a stack to simulate recursion
3. **Recursive Implementation**: Traditional recursive approach
4. **Visited Tracking**: Boolean array to track visited vertices
5. **Undirected Graph**: Edges are added in both directions

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for visited array and stack space

The algorithm explores as far as possible along each branch before backtracking, which is the defining characteristic of Depth-First Search.

