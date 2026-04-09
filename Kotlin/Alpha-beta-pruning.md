# Alpha-Beta Pruning Algorithm in Kotlin

Here's a complete implementation of the Alpha-Beta pruning algorithm for a minimax game tree in Kotlin:

```kotlin
import kotlin.math.max
import kotlin.math.min

// Game tree node representation
data class TreeNode(val value: Int? = null, val children: MutableList<TreeNode> = mutableListOf()) {
    val isLeaf: Boolean get() = value != null
}

// Alpha-Beta pruning implementation
fun alphaBeta(node: TreeNode, depth: Int, alpha: Int, beta: Int, isMaximizing: Boolean): Int {
    // Base case: reached maximum depth or leaf node
    if (depth == 0 || node.isLeaf) {
        return node.value ?: 0
    }
    
    if (isMaximizing) {
        var maxEval = Int.MIN_VALUE
        for (child in node.children) {
            val eval = alphaBeta(child, depth - 1, alpha, beta, false)
            maxEval = max(maxEval, eval)
            alpha = max(alpha, eval)
            
            // Alpha-Beta pruning
            if (beta <= alpha) {
                break // Beta cut-off
            }
        }
        return maxEval
    } else {
        var minEval = Int.MAX_VALUE
        for (child in node.children) {
            val eval = alphaBeta(child, depth - 1, alpha, beta, true)
            minEval = min(minEval, eval)
            beta = min(beta, eval)
            
            // Alpha-Beta pruning
            if (beta <= alpha) {
                break // Alpha cut-off
            }
        }
        return minEval
    }
}

// Alternative implementation with move selection
fun alphaBetaWithMoves(node: TreeNode, depth: Int, alpha: Int, beta: Int, isMaximizing: Boolean): Pair<Int, TreeNode?> {
    if (depth == 0 || node.isLeaf) {
        return Pair(node.value ?: 0, null)
    }
    
    var bestMove: TreeNode? = null
    
    if (isMaximizing) {
        var maxEval = Int.MIN_VALUE
        for (child in node.children) {
            val (eval, _) = alphaBetaWithMoves(child, depth - 1, alpha, beta, false)
            if (eval > maxEval) {
                maxEval = eval
                bestMove = child
            }
            alpha = max(alpha, eval)
            if (beta <= alpha) {
                break
            }
        }
        return Pair(maxEval, bestMove)
    } else {
        var minEval = Int.MAX_VALUE
        for (child in node.children) {
            val (eval, _) = alphaBetaWithMoves(child, depth - 1, alpha, beta, true)
            if (eval < minEval) {
                minEval = eval
                bestMove = child
            }
            beta = min(beta, eval)
            if (beta <= alpha) {
                break
            }
        }
        return Pair(minEval, bestMove)
    }
}

// Example usage
fun main() {
    // Create a sample game tree
    //        3
    //    /   |   \
    //   5    2    9
    //  / \  / \  / \
    // 1  2 3  4 5  6
    
    val tree = TreeNode()
    
    // Level 1
    val node1 = TreeNode(value = 3)
    val node2 = TreeNode(value = 5)
    val node3 = TreeNode(value = 2)
    val node4 = TreeNode(value = 9)
    
    // Level 2
    val leaf1 = TreeNode(value = 1)
    val leaf2 = TreeNode(value = 2)
    val leaf3 = TreeNode(value = 3)
    val leaf4 = TreeNode(value = 4)
    val leaf5 = TreeNode(value = 5)
    val leaf6 = TreeNode(value = 6)
    
    // Build the tree
    node1.children.add(leaf1)
    node1.children.add(leaf2)
    
    node2.children.add(leaf3)
    node2.children.add(leaf4)
    
    node3.children.add(leaf5)
    node3.children.add(leaf6)
    
    tree.children.add(node1)
    tree.children.add(node2)
    tree.children.add(node3)
    
    // Apply Alpha-Beta pruning
    val result = alphaBeta(tree, 3, Int.MIN_VALUE, Int.MAX_VALUE, true)
    println("Best value using Alpha-Beta pruning: $result")
    
    // With move selection
    val (bestValue, bestMove) = alphaBetaWithMoves(tree, 3, Int.MIN_VALUE, Int.MAX_VALUE, true)
    println("Best value with move selection: $bestValue")
    println("Best move: ${bestMove?.value ?: "None"}")
}
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components**
- **Alpha-Beta Pruning**: Efficiently reduces the number of nodes evaluated
- **Minimax Logic**: Alternates between maximizing and minimizing players
- **Depth Limiting**: Controls search depth to prevent infinite recursion

### 2. **Pruning Mechanisms**
- **Alpha Cut-off**: When alpha ≥ beta, further siblings can be ignored
- **Beta Cut-off**: When beta ≤ alpha, further siblings can be ignored
- **Optimization**: Reduces time complexity from O(b^d) to O(b^(d/2))

### 3. **Usage Examples**
```kotlin
// Basic usage
val result = alphaBeta(tree, depth = 4, alpha = Int.MIN_VALUE, beta = Int.MAX_VALUE, isMaximizing = true)

// With move selection
val (value, move) = alphaBetaWithMoves(tree, depth = 4, alpha = Int.MIN_VALUE, beta = Int.MAX_VALUE, isMaximizing = true)
```

### 4. **Time Complexity**
- **Best Case**: O(b^(d/2)) with perfect ordering
- **Worst Case**: O(b^d) - same as minimax
- **Space Complexity**: O(b × d) for the recursion stack

This implementation demonstrates how Alpha-Beta pruning significantly improves the efficiency of game tree search algorithms by eliminating branches that will not influence the final decision.

