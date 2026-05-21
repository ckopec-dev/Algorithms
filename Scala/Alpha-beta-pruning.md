# Alpha-Beta Pruning Algorithm in Scala

Here's a complete implementation of the Alpha-Beta pruning algorithm in Scala:

```scala
// Define the game tree node structure
case class GameNode(value: Int, children: List[GameNode] = List()) {
  def isLeaf: Boolean = children.isEmpty
}

// Alpha-Beta pruning implementation
object AlphaBeta {
  
  /**
   * Alpha-Beta pruning algorithm for minimax decision making
   * @param node Current game node
   * @param depth Remaining depth to search
   * @param alpha Best value maximizer can guarantee
   * @param beta Best value minimizer can guarantee
   * @param maximizingPlayer True if current player is maximizing
   * @return Best value for the current player
   */
  def alphaBeta(node: GameNode, depth: Int, alpha: Int, beta: Int, maximizingPlayer: Boolean): Int = {
    // Base case: reached maximum depth or leaf node
    if (depth == 0 || node.isLeaf) {
      return node.value
    }
    
    if (maximizingPlayer) {
      var maxEval = Int.MinValue
      for (child <- node.children) {
        val eval = alphaBeta(child, depth - 1, alpha, beta, false)
        maxEval = math.max(maxEval, eval)
        alpha = math.max(alpha, eval)
        
        // Alpha-Beta pruning: if alpha >= beta, prune remaining branches
        if (alpha >= beta) {
          // println("Pruning branches for maximizing player")
          return maxEval
        }
      }
      maxEval
    } else {
      var minEval = Int.MaxValue
      for (child <- node.children) {
        val eval = alphaBeta(child, depth - 1, alpha, beta, true)
        minEval = math.min(minEval, eval)
        beta = math.min(beta, eval)
        
        // Alpha-Beta pruning: if alpha >= beta, prune remaining branches
        if (alpha >= beta) {
          // println("Pruning branches for minimizing player")
          return minEval
        }
      }
      minEval
    }
  }
  
  /**
   * Convenience method to start the alpha-beta search
   */
  def findBestValue(node: GameNode, depth: Int, maximizingPlayer: Boolean): Int = {
    alphaBeta(node, depth, Int.MinValue, Int.MaxValue, maximizingPlayer)
  }
}

// Example usage and test cases
object AlphaBetaExample extends App {
  
  // Create a sample game tree
  //       3
  //    /  |  \
  //   5   2   9
  //  /|   |   |\
  // 1 2   8   4 7
  val sampleTree = GameNode(3,
    List(
      GameNode(5, List(GameNode(1), GameNode(2))),
      GameNode(2, List(GameNode(8))),
      GameNode(9, List(GameNode(4), GameNode(7)))
    )
  )
  
  println("Sample Game Tree:")
  println("Root value: 3")
  println("Maximizing player's turn")
  println("Depth: 3")
  
  val bestValue = AlphaBeta.findBestValue(sampleTree, 3, true)
  println(s"\nBest value found: $bestValue")
  
  // Another example with deeper tree
  //       10
  //    /  |  |  \
  //   3   2  5   8
  //  /|   |  |   |\
  // 1 2   3  4   5 6
  val deeperTree = GameNode(10,
    List(
      GameNode(3, List(GameNode(1), GameNode(2))),
      GameNode(2, List(GameNode(3))),
      GameNode(5, List(GameNode(4))),
      GameNode(8, List(GameNode(5), GameNode(6)))
    )
  )
  
  println("\n" + "="*50)
  println("Deeper Game Tree:")
  println("Root value: 10")
  println("Maximizing player's turn")
  println("Depth: 3")
  
  val bestValue2 = AlphaBeta.findBestValue(deeperTree, 3, true)
  println(s"\nBest value found: $bestValue2")
  
  // Demonstrate pruning by showing what happens with a simple case
  println("\n" + "="*50)
  println("Demonstrating Alpha-Beta Pruning:")
  
  // Simple case: 3-level tree where pruning occurs
  val pruningExample = GameNode(1,
    List(
      GameNode(3, List(GameNode(5), GameNode(2))),
      GameNode(2, List(GameNode(8), GameNode(1))),
      GameNode(4, List(GameNode(3), GameNode(7)))
    )
  )
  
  val result = AlphaBeta.findBestValue(pruningExample, 3, true)
  println(s"Result for pruning example: $result")
}
```

## Key Features of this Implementation:

1. **GameNode Case Class**: Represents nodes in the game tree with value and children
2. **Alpha-Beta Function**: Core algorithm implementation with proper pruning logic
3. **Parameter Handling**: 
   - `alpha`: Best value maximizer can guarantee
   - `beta`: Best value minimizer can guarantee
   - `maximizingPlayer`: Boolean flag for current player type
4. **Pruning Logic**: 
   - When `alpha >= beta`, remaining branches are pruned
   - Efficiently reduces search space
5. **Base Cases**: Handles leaf nodes and maximum depth reached

## How Alpha-Beta Pruning Works:

1. **Maximizing Player**: Tries to maximize the value
2. **Minimizing Player**: Tries to minimize the value
3. **Pruning**: When a branch cannot possibly affect the final decision, it's pruned
4. **Bounds**: `alpha` and `beta` represent the best values found so far

## Time Complexity:
- **Best Case**: O(b^(d/2)) where b is branching factor and d is depth
- **Worst Case**: O(b^d) - same as minimax but with much better constants
- **Space Complexity**: O(b × d) for the recursion stack

This implementation demonstrates the core principles of Alpha-Beta pruning while maintaining clean, functional Scala code style.

