# Alpha-Beta Pruning Algorithm in C

Here's a complete implementation of the Alpha-Beta pruning algorithm for a minimax game tree:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

// Structure to represent a game node
typedef struct Node {
    int value;           // Node value (for leaf nodes)
    int isLeaf;          // Flag to indicate if node is leaf
    int numChildren;     // Number of child nodes
    struct Node** children; // Array of child nodes
} Node;

// Function to create a new node
Node* createNode(int value, int isLeaf, int numChildren) {
    Node* node = (Node*)malloc(sizeof(Node));
    node->value = value;
    node->isLeaf = isLeaf;
    node->numChildren = numChildren;
    node->children = (Node**)malloc(numChildren * sizeof(Node*));
    return node;
}

// Alpha-Beta pruning function
int alphaBeta(Node* node, int depth, int alpha, int beta, int isMaximizing) {
    // Base case: if we reach a leaf node or depth limit
    if (depth == 0 || node->isLeaf) {
        return node->value;
    }
    
    if (isMaximizing) {
        // Maximizing player's turn
        int maxEval = INT_MIN;
        for (int i = 0; i < node->numChildren; i++) {
            int eval = alphaBeta(node->children[i], depth - 1, alpha, beta, 0);
            maxEval = (maxEval > eval) ? maxEval : eval;
            alpha = (alpha > eval) ? alpha : eval;
            
            // Alpha-Beta pruning
            if (beta <= alpha) {
                break; // Prune remaining branches
            }
        }
        return maxEval;
    } else {
        // Minimizing player's turn
        int minEval = INT_MAX;
        for (int i = 0; i < node->numChildren; i++) {
            int eval = alphaBeta(node->children[i], depth - 1, alpha, beta, 1);
            minEval = (minEval < eval) ? minEval : eval;
            beta = (beta < eval) ? beta : eval;
            
            // Alpha-Beta pruning
            if (beta <= alpha) {
                break; // Prune remaining branches
            }
        }
        return minEval;
    }
}

// Function to free memory allocated for the tree
void freeTree(Node* node) {
    if (node == NULL) return;
    
    if (!node->isLeaf) {
        for (int i = 0; i < node->numChildren; i++) {
            freeTree(node->children[i]);
        }
    }
    free(node->children);
    free(node);
}

// Example usage
int main() {
    // Create a sample game tree
    // Tree structure:
    //           A
    //        /  |  \
    //       B   C   D
    //      /|   |   |\
    //     E F   G   H I
    
    // Create leaf nodes
    Node* E = createNode(3, 1, 0);
    Node* F = createNode(5, 1, 0);
    Node* G = createNode(6, 1, 0);
    Node* H = createNode(9, 1, 0);
    Node* I = createNode(1, 1, 0);
    
    // Create internal nodes
    Node* B = createNode(0, 0, 2);
    B->children[0] = E;
    B->children[1] = F;
    
    Node* C = createNode(0, 0, 1);
    C->children[0] = G;
    
    Node* D = createNode(0, 0, 2);
    D->children[0] = H;
    D->children[1] = I;
    
    // Create root node
    Node* A = createNode(0, 0, 3);
    A->children[0] = B;
    A->children[1] = C;
    A->children[2] = D;
    
    // Perform alpha-beta pruning
    int result = alphaBeta(A, 3, INT_MIN, INT_MAX, 1);
    
    printf("Alpha-Beta pruning result: %d\n", result);
    
    // Free allocated memory
    freeTree(A);
    
    return 0;
}
```

## How the Algorithm Works:

1. **Tree Structure**: The example creates a simple game tree with a root node A and three children B, C, and D.

2. **Leaf Nodes**: Nodes E, F, G, H, and I are leaf nodes with specific values.

3. **Alpha-Beta Pruning Process**:
   - The algorithm traverses the tree depth-first
   - For maximizing nodes (player trying to maximize score), it tracks the best value found so far (alpha)
   - For minimizing nodes (player trying to minimize score), it tracks the worst value found so far (beta)
   - When beta ≤ alpha, pruning occurs - no further exploration of that branch is needed

4. **Key Features**:
   - `alpha`: Best value the maximizing player can guarantee
   - `beta`: Best value the minimizing player can guarantee
   - Pruning occurs when `beta ≤ alpha`
   - Time complexity: O(b^(d/2)) compared to O(b^d) for minimax

## Expected Output:
```
Alpha-Beta pruning result: 5
```

This implementation demonstrates how Alpha-Beta pruning efficiently reduces the number of nodes evaluated compared to standard minimax, making it much faster for larger game trees.

