# Alpha-Beta Pruning Algorithm Implementation

Here's a complete implementation of the Alpha-Beta pruning algorithm in Python:

```python
import math

class GameNode:
    def __init__(self, value=None, children=None):
        self.value = value  # For leaf nodes
        self.children = children or []  # For internal nodes
    
    def is_terminal(self):
        return self.value is not None
    
    def get_value(self):
        return self.value

def alpha_beta_pruning(node, depth, alpha, beta, maximizing_player):
    """
    Alpha-Beta Pruning Algorithm
    
    Args:
        node: Current game node
        depth: Current depth in the tree
        alpha: Best value maximizer can guarantee
        beta: Best value minimizer can guarantee
        maximizing_player: True if current player is maximizing
    
    Returns:
        Best value for the current player
    """
    
    # Base case: if we've reached maximum depth or terminal node
    if depth == 0 or node.is_terminal():
        return node.get_value()
    
    if maximizing_player:
        # Maximizer's turn
        max_eval = -math.inf
        for child in node.children:
            eval_score = alpha_beta_pruning(child, depth - 1, alpha, beta, False)
            max_eval = max(max_eval, eval_score)
            alpha = max(alpha, eval_score)
            
            # Alpha-Beta pruning
            if beta <= alpha:
                print(f"Pruned at node with alpha={alpha}, beta={beta}")
                break  # Beta cut-off
                
        return max_eval
    
    else:
        # Minimizer's turn
        min_eval = math.inf
        for child in node.children:
            eval_score = alpha_beta_pruning(child, depth - 1, alpha, beta, True)
            min_eval = min(min_eval, eval_score)
            beta = min(beta, eval_score)
            
            # Alpha-Beta pruning
            if beta <= alpha:
                print(f"Pruned at node with alpha={alpha}, beta={beta}")
                break  # Alpha cut-off
                
        return min_eval

# Example usage with a sample game tree
def create_sample_tree():
    """
    Create a sample game tree for demonstration:
    
              Root
            /  |  \
           3   5   2
          /|   |   |\
         1 2   3   4 5
    """
    
    # Leaf nodes
    leaf1 = GameNode(value=1)
    leaf2 = GameNode(value=2)
    leaf3 = GameNode(value=3)
    leaf4 = GameNode(value=4)
    leaf5 = GameNode(value=5)
    
    # Intermediate nodes
    node1 = GameNode(children=[leaf1, leaf2])
    node2 = GameNode(children=[leaf3])
    node3 = GameNode(children=[leaf4, leaf5])
    
    # Root node
    root = GameNode(children=[node1, node2, node3])
    
    return root

# Demonstration of Alpha-Beta Pruning
def demonstrate_alpha_beta():
    print("=== Alpha-Beta Pruning Demonstration ===\n")
    
    # Create sample tree
    root = create_sample_tree()
    
    print("Game Tree Structure:")
    print("              Root")
    print("            /  |  \\")
    print("           3   5   2")
    print("          /|   |   |\\")
    print("         1 2   3   4 5")
    print()
    
    # Apply alpha-beta pruning
    result = alpha_beta_pruning(root, depth=3, alpha=-math.inf, beta=math.inf, maximizing_player=True)
    
    print(f"Final result (optimal value): {result}")
    print("\n=== Algorithm Analysis ===")
    print("Time Complexity: O(b^(d/2)) where b is branching factor and d is depth")
    print("Space Complexity: O(bd) for the recursion stack")
    print("Pruning reduces the number of nodes evaluated significantly")

# More complex example with detailed tracing
def detailed_alpha_beta_example():
    print("\n=== Detailed Example with Tracing ===")
    
    # Create a more complex tree
    #         A
    #     /   |   \
    #    B    C    D
    #   /|   /|   /|\
    #  E F  G H  I J K
    
    # Leaf nodes
    leaves = [GameNode(value=i) for i in [3, 5, 2, 9, 1, 7, 4, 6]]
    
    # Intermediate nodes
    node_f = GameNode(children=[leaves[0], leaves[1]])  # B
    node_g = GameNode(children=[leaves[2], leaves[3]])  # C
    node_h = GameNode(children=[leaves[4], leaves[5]])  # D
    
    # Root node
    root = GameNode(children=[node_f, node_g, node_h])
    
    print("Computing optimal value with Alpha-Beta pruning...")
    result = alpha_beta_pruning(root, depth=3, alpha=-math.inf, beta=math.inf, maximizing_player=True)
    print(f"Optimal value: {result}")

if __name__ == "__main__":
    demonstrate_alpha_beta()
    detailed_alpha_beta_example()
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components:**
- **GameNode class**: Represents nodes in the game tree
- **Alpha-Beta pruning function**: Implements the main algorithm logic
- **Minimax framework**: Supports both maximizing and minimizing players

### 2. **Pruning Mechanism:**
- **Alpha**: Best value the maximizing player can guarantee
- **Beta**: Best value the minimizing player can guarantee
- **Cut-off conditions**: When `beta ≤ alpha`, branches are pruned

### 3. **Algorithm Benefits:**
- **Time Complexity**: Reduces from O(b^d) to O(b^(d/2))
- **Space Complexity**: O(bd) for recursion stack
- **Optimal Solution**: Guarantees the same result as minimax

### 4. **Usage Example:**
The code demonstrates with a simple game tree and shows how pruning works by printing when branches are cut off.

This implementation can be easily adapted for specific game applications like chess, tic-tac-toe, or other minimax-style games.

