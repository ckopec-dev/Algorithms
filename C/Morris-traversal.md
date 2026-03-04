# Morris Traversal Algorithm in C

Morris traversal is an algorithm for tree traversal that allows us to traverse a binary tree without using recursion or a stack. It uses the tree's right pointers to create temporary links.

## Code Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Definition of a tree node
struct TreeNode {
    int data;
    struct TreeNode* left;
    struct TreeNode* right;
};

// Function to create a new node
struct TreeNode* createNode(int data) {
    struct TreeNode* newNode = (struct TreeNode*)malloc(sizeof(struct TreeNode));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

// Morris Inorder Traversal
void morrisInorder(struct TreeNode* root) {
    struct TreeNode* current = root;
    
    while (current != NULL) {
        // If left child is NULL, visit current node and move to right
        if (current->left == NULL) {
            printf("%d ", current->data);
            current = current->right;
        }
        else {
            // Find the inorder predecessor (rightmost node in left subtree)
            struct TreeNode* predecessor = current->left;
            while (predecessor->right != NULL && predecessor->right != current) {
                predecessor = predecessor->right;
            }
            
            // If right pointer of predecessor is NULL, make it point to current
            if (predecessor->right == NULL) {
                predecessor->right = current;
                current = current->left;
            }
            else {
                // If right pointer already points to current, revert the changes
                predecessor->right = NULL;
                printf("%d ", current->data);
                current = current->right;
            }
        }
    }
}

// Morris Preorder Traversal
void morrisPreorder(struct TreeNode* root) {
    struct TreeNode* current = root;
    
    while (current != NULL) {
        // If left child is NULL, visit current node and move to right
        if (current->left == NULL) {
            printf("%d ", current->data);
            current = current->right;
        }
        else {
            // Find the inorder predecessor (rightmost node in left subtree)
            struct TreeNode* predecessor = current->left;
            while (predecessor->right != NULL && predecessor->right != current) {
                predecessor = predecessor->right;
            }
            
            // If right pointer of predecessor is NULL, make it point to current
            if (predecessor->right == NULL) {
                printf("%d ", current->data); // Visit current node here for preorder
                predecessor->right = current;
                current = current->left;
            }
            else {
                // If right pointer already points to current, revert the changes
                predecessor->right = NULL;
                current = current->right;
            }
        }
    }
}

// Example usage
int main() {
    // Create the following binary tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    struct TreeNode* root = createNode(1);
    root->left = createNode(2);
    root->right = createNode(3);
    root->left->left = createNode(4);
    root->left->right = createNode(5);
    
    printf("Morris Inorder Traversal: ");
    morrisInorder(root);
    printf("\n");
    
    printf("Morris Preorder Traversal: ");
    morrisPreorder(root);
    printf("\n");
    
    return 0;
}
```

## Output
```
Morris Inorder Traversal: 4 2 5 1 3 
Morris Preorder Traversal: 1 2 4 5 3 
```

## How it Works

1. **Inorder Traversal**:
   - Start from root
   - If current node has no left child, visit it and move right
   - If it has a left child, find the inorder predecessor (rightmost node in left subtree)
   - Create a temporary link from predecessor to current node
   - Move to left child
   - When returning, remove the temporary link and visit current node

2. **Preorder Traversal**:
   - Same logic as inorder but visit the current node before moving to the left subtree

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - No extra space used except for temporary pointers

## Key Advantages

- No recursion or stack required
- Constant extra space usage
- Can be used in memory-constrained environments
- Useful for very large trees where stack overflow is a concern

The Morris traversal is particularly useful when you want to traverse a tree without using additional memory for recursion or explicit stack data structures.

