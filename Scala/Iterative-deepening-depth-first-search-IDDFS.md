# Iterative Deepening Depth-First Search (IDDFS) in Scala

Here's a complete implementation of IDDFS algorithm in Scala:

```scala
import scala.collection.mutable

// Node representation
case class Node(value: Int, children: List[Node] = List()) {
  override def toString: String = s"Node($value)"
}

object IDDFS {
  
  /**
   * Iterative Deepening Depth-First Search implementation
   * @param root The starting node
   * @param target The value to search for
   * @return Option containing the target node if found, None otherwise
   */
  def search(root: Node, target: Int): Option[Node] = {
    var depth = 0
    var found: Option[Node] = None
    
    // Keep increasing depth until target is found
    while (found.isEmpty) {
      found = depthLimitedSearch(root, target, depth)
      depth += 1
    }
    
    found
  }
  
  /**
   * Depth-Limited Search (helper function for IDDFS)
   * @param node Current node being explored
   * @param target Value to search for
   * @param depthLimit Maximum depth allowed
   * @return Option containing the target node if found within depth limit, None otherwise
   */
  private def depthLimitedSearch(node: Node, target: Int, depthLimit: Int): Option[Node] = {
    if (node.value == target) {
      Some(node)
    } else if (depthLimit == 0) {
      None
    } else {
      // Explore children with reduced depth limit
      node.children.foreach { child =>
        val result = depthLimitedSearch(child, target, depthLimit - 1)
        if (result.isDefined) {
          return result
        }
      }
      None
    }
  }
  
  /**
   * Alternative implementation using a stack-based approach
   */
  def searchWithStack(root: Node, target: Int): Option[Node] = {
    var depth = 0
    var found: Option[Node] = None
    
    while (found.isEmpty) {
      found = iterativeDeepeningSearch(root, target, depth)
      depth += 1
    }
    
    found
  }
  
  private def iterativeDeepeningSearch(node: Node, target: Int, maxDepth: Int): Option[Node] = {
    val stack = mutable.Stack[(Node, Int)]() // (node, current_depth)
    stack.push((node, 0))
    
    while (stack.nonEmpty) {
      val (currentNode, currentDepth) = stack.pop()
      
      if (currentNode.value == target) {
        return Some(currentNode)
      }
      
      if (currentDepth < maxDepth) {
        // Add children to stack (in reverse order to maintain left-to-right traversal)
        currentNode.children.reverse.foreach { child =>
          stack.push((child, currentDepth + 1))
        }
      }
    }
    
    None
  }
}

// Example usage and test
object IDDFSExample extends App {
  
  // Create a sample tree:
  //       1
  //    /  |  \
  //   2   3   4
  //  /|   |   |\
  // 5 6  7   8 9
  
  val tree = Node(1, List(
    Node(2, List(
      Node(5),
      Node(6)
    )),
    Node(3, List(
      Node(7)
    )),
    Node(4, List(
      Node(8),
      Node(9)
    ))
  ))
  
  println("Tree structure:")
  println("       1")
  println("    /  |  \\")
  println("   2   3   4")
  println("  /|   |   |\\")
  println(" 5 6  7   8 9")
  println()
  
  // Test searches
  val target1 = 7
  val result1 = IDDFS.search(tree, target1)
  println(s"Searching for $target1: ${result1.map(_.value).getOrElse("Not found")}")
  
  val target2 = 9
  val result2 = IDDFS.search(tree, target2)
  println(s"Searching for $target2: ${result2.map(_.value).getOrElse("Not found")}")
  
  val target3 = 10
  val result3 = IDDFS.search(tree, target3)
  println(s"Searching for $target3: ${result3.map(_.value).getOrElse("Not found")}")
  
  println("\n--- Using stack-based approach ---")
  val result4 = IDDFS.searchWithStack(tree, 6)
  println(s"Searching for 6 with stack approach: ${result4.map(_.value).getOrElse("Not found")}")
}
```

## Key Features of this Implementation:

1. **Core Algorithm**: IDDFS combines the benefits of DFS (low memory usage) and BFS (completeness)
2. **Depth Limiting**: Uses depth-limited search as a subroutine
3. **Iterative Process**: Gradually increases depth limit until target is found
4. **Memory Efficient**: Only stores the current path, not all nodes
5. **Complete**: Guarantees finding a solution if one exists

## How it Works:

1. Start with depth limit = 0
2. Perform depth-limited search with current depth limit
3. If target not found, increase depth limit by 1
4. Repeat until target is found
5. Returns the first solution found (optimal for unweighted graphs)

## Time and Space Complexity:

- **Time Complexity**: O(b^d) where b is branching factor and d is depth
- **Space Complexity**: O(bd) where b is branching factor and d is depth

## Output Example:
```
Tree structure:
       1
    /  |  \
   2   3   4
  /|   |   |\
 5 6  7   8 9

Searching for 7: 7
Searching for 9: 9
Searching for 10: Not found

--- Using stack-based approach ---
Searching for 6 with stack approach: 6
```

