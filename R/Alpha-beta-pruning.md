# Alpha-Beta Pruning Algorithm in R

Here's a complete implementation of the Alpha-Beta pruning algorithm in R for a minimax game tree:

```r
# Alpha-Beta Pruning Implementation in R

# Node structure for game tree
Node <- function(value = NULL, children = NULL, is_terminal = FALSE) {
  list(
    value = value,
    children = children,
    is_terminal = is_terminal
  )
}

# Alpha-Beta Pruning Function
alpha_beta_pruning <- function(node, depth, alpha, beta, maximizing_player) {
  # Base case: if we've reached maximum depth or terminal node
  if (depth == 0 || node$is_terminal) {
    return(node$value)
  }
  
  if (maximizing_player) {
    # Maximizing player - try to maximize the value
    max_eval <- -Inf
    
    for (child in node$children) {
      eval <- alpha_beta_pruning(child, depth - 1, alpha, beta, FALSE)
      max_eval <- max(max_eval, eval)
      alpha <- max(alpha, eval)
      
      # Alpha-beta pruning: if alpha >= beta, prune remaining branches
      if (alpha >= beta) {
        break  # Prune remaining children
      }
    }
    
    return(max_eval)
  } else {
    # Minimizing player - try to minimize the value
    min_eval <- Inf
    
    for (child in node$children) {
      eval <- alpha_beta_pruning(child, depth - 1, alpha, beta, TRUE)
      min_eval <- min(min_eval, eval)
      beta <- min(beta, eval)
      
      # Alpha-beta pruning: if alpha >= beta, prune remaining branches
      if (alpha >= beta) {
        break  # Prune remaining children
      }
    }
    
    return(min_eval)
  }
}

# Example usage with a sample game tree
# Create a simple game tree for demonstration
create_sample_tree <- function() {
  # Leaf nodes with values
  leaf1 <- Node(value = 3, is_terminal = TRUE)
  leaf2 <- Node(value = 5, is_terminal = TRUE)
  leaf3 <- Node(value = 2, is_terminal = TRUE)
  leaf4 <- Node(value = 9, is_terminal = TRUE)
  leaf5 <- Node(value = 1, is_terminal = TRUE)
  leaf6 <- Node(value = 8, is_terminal = TRUE)
  leaf7 <- Node(value = 4, is_terminal = TRUE)
  leaf8 <- Node(value = 7, is_terminal = TRUE)
  
  # Intermediate nodes
  node_c <- Node(children = list(leaf3, leaf4), is_terminal = FALSE)
  node_d <- Node(children = list(leaf5, leaf6), is_terminal = FALSE)
  node_e <- Node(children = list(leaf7, leaf8), is_terminal = FALSE)
  
  node_a <- Node(children = list(leaf1, leaf2), is_terminal = FALSE)
  node_b <- Node(children = list(node_c, node_d), is_terminal = FALSE)
  root <- Node(children = list(node_a, node_b, node_e), is_terminal = FALSE)
  
  return(root)
}

# Main execution
cat("Alpha-Beta Pruning Algorithm Example\n")
cat("====================================\n\n")

# Create sample game tree
tree <- create_sample_tree()

# Perform alpha-beta pruning
result <- alpha_beta_pruning(tree, depth = 3, alpha = -Inf, beta = Inf, maximizing_player = TRUE)

cat("Optimal value found:", result, "\n")
cat("This represents the best outcome for the maximizing player\n\n")

# Let's trace through a simpler example to show pruning in action
cat("Simple 3-level tree with pruning demonstration:\n")
cat("Tree structure:\n")
cat("        Root\n")
cat("    /   |   \\\n")
cat("   A    B    C\n")
cat("  /|   /|   /\\\n")
cat(" 1 2  3 4  5 6\n\n")

# Create a simpler tree for demonstration
simple_tree <- Node(
  children = list(
    Node(children = list(
      Node(value = 1, is_terminal = TRUE),
      Node(value = 2, is_terminal = TRUE)
    ), is_terminal = FALSE),
    Node(children = list(
      Node(value = 3, is_terminal = TRUE),
      Node(value = 4, is_terminal = TRUE)
    ), is_terminal = FALSE),
    Node(children = list(
      Node(value = 5, is_terminal = TRUE),
      Node(value = 6, is_terminal = TRUE)
    ), is_terminal = FALSE)
  ),
  is_terminal = FALSE
)

# Perform alpha-beta pruning on simple tree
simple_result <- alpha_beta_pruning(simple_tree, depth = 2, alpha = -Inf, beta = Inf, maximizing_player = TRUE)
cat("Result of alpha-beta pruning on simple tree:", simple_result, "\n")

# Show the pruning process
cat("\nPruning Process:\n")
cat("1. Evaluate leftmost branch (A): max(1,2) = 2\n")
cat("2. Update alpha = 2\n")
cat("3. Evaluate middle branch (B): max(3,4) = 4\n")
cat("4. Since 4 > 2, we continue\n")
cat("5. Update alpha = 4\n")
cat("6. Evaluate rightmost branch (C): max(5,6) = 6\n")
cat("7. Since 6 > 4, we continue\n")
cat("8. Final result: max(2,4,6) = 6\n\n")

cat("Note: In a real implementation, when alpha >= beta, branches are pruned\n")
cat("and not evaluated further, saving computation time.\n")
```

## Key Features of this Implementation:

1. **Node Structure**: Creates tree nodes with values, children, and terminal status
2. **Recursive Algorithm**: Implements the core alpha-beta pruning logic
3. **Pruning Logic**: Stops evaluating branches when `alpha >= beta`
4. **Minimax Integration**: Works with both maximizing and minimizing players
5. **Depth Control**: Limits search depth to prevent infinite recursion

## How Alpha-Beta Pruning Works:

- **Alpha**: Best value that the maximizing player can guarantee
- **Beta**: Best value that the minimizing player can guarantee
- **Pruning**: When `alpha >= beta`, further branches are not evaluated
- **Efficiency**: Reduces time complexity from O(b^d) to O(b^(d/2))

This implementation demonstrates the core principles of alpha-beta pruning while showing how it can be applied to game tree search problems.

