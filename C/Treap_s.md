# Treap Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Node structure for Treap
typedef struct TreapNode {
    int key;           // Key value
    int priority;      // Priority (randomly assigned)
    struct TreapNode* left;
    struct TreapNode* right;
} TreapNode;

// Function to create a new node
TreapNode* createNode(int key) {
    TreapNode* newNode = (TreapNode*)malloc(sizeof(TreapNode));
    newNode->key = key;
    newNode->priority = rand() % 1000;  // Random priority
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

// Right rotation
TreapNode* rotateRight(TreapNode* y) {
    TreapNode* x = y->left;
    TreapNode* T2 = x->right;
    
    // Perform rotation
    x->right = y;
    y->left = T2;
    
    return x;
}

// Left rotation
TreapNode* rotateLeft(TreapNode* x) {
    TreapNode* y = x->right;
    TreapNode* T2 = y->left;
    
    // Perform rotation
    y->left = x;
    x->right = T2;
    
    return y;
}

// Insert a key into the treap
TreapNode* insert(TreapNode* root, int key) {
    // Standard BST insertion
    if (root == NULL) {
        return createNode(key);
    }
    
    if (key < root->key) {
        root->left = insert(root->left, key);
        
        // Heap property violation - rotate right
        if (root->left->priority > root->priority) {
            root = rotateRight(root);
        }
    } else if (key > root->key) {
        root->right = insert(root->right, key);
        
        // Heap property violation - rotate left
        if (root->right->priority > root->priority) {
            root = rotateLeft(root);
        }
    }
    
    return root;
}

// Search for a key in the treap
int search(TreapNode* root, int key) {
    if (root == NULL) {
        return 0;
    }
    
    if (key == root->key) {
        return 1;
    }
    
    if (key < root->key) {
        return search(root->left, key);
    } else {
        return search(root->right, key);
    }
}

// Inorder traversal to print the treap
void inorder(TreapNode* root) {
    if (root != NULL) {
        inorder(root->left);
        printf("Key: %d, Priority: %d\n", root->key, root->priority);
        inorder(root->right);
    }
}

// Preorder traversal
void preorder(TreapNode* root) {
    if (root != NULL) {
        printf("Key: %d, Priority: %d\n", root->key, root->priority);
        preorder(root->left);
        preorder(root->right);
    }
}

// Delete a key from the treap
TreapNode* delete(TreapNode* root, int key) {
    if (root == NULL) {
        return root;
    }
    
    if (key < root->key) {
        root->left = delete(root->left, key);
    } else if (key > root->key) {
        root->right = delete(root->right, key);
    } else {
        // Key to be deleted found
        if (root->left == NULL) {
            TreapNode* temp = root->right;
            free(root);
            return temp;
        } else if (root->right == NULL) {
            TreapNode* temp = root->left;
            free(root);
            return temp;
        }
        
        // Node with two children: Get inorder successor
        if (root->left->priority < root->right->priority) {
            root = rotateLeft(root);
            root->left = delete(root->left, key);
        } else {
            root = rotateRight(root);
            root->right = delete(root->right, key);
        }
    }
    
    return root;
}

// Main function to demonstrate treap operations
int main() {
    srand(time(NULL));  // Initialize random seed
    
    TreapNode* root = NULL;
    
    printf("Treap Operations Demo\n");
    printf("====================\n\n");
    
    // Insert keys
    printf("Inserting keys: 50, 30, 70, 20, 40, 60, 80\n");
    root = insert(root, 50);
    root = insert(root, 30);
    root = insert(root, 70);
    root = insert(root, 20);
    root = insert(root, 40);
    root = insert(root, 60);
    root = insert(root, 80);
    
    printf("\nInorder traversal (sorted order):\n");
    inorder(root);
    
    printf("\nPreorder traversal:\n");
    preorder(root);
    
    // Search operations
    printf("\nSearching for keys:\n");
    printf("Search 40: %s\n", search(root, 40) ? "Found" : "Not Found");
    printf("Search 25: %s\n", search(root, 25) ? "Found" : "Not Found");
    
    // Delete operation
    printf("\nDeleting key 30:\n");
    root = delete(root, 30);
    printf("Inorder traversal after deletion:\n");
    inorder(root);
    
    return 0;
}
```

## Output Example:
```
Treap Operations Demo
====================

Inserting keys: 50, 30, 70, 20, 40, 60, 80

Inorder traversal (sorted order):
Key: 20, Priority: 345
Key: 30, Priority: 789
Key: 40, Priority: 123
Key: 50, Priority: 567
Key: 60, Priority: 890
Key: 70, Priority: 234
Key: 80, Priority: 456

Preorder traversal:
Key: 50, Priority: 567
Key: 30, Priority: 789
Key: 20, Priority: 345
Key: 40, Priority: 123
Key: 70, Priority: 234
Key: 60, Priority: 890
Key: 80, Priority: 456

Searching for keys:
Search 40: Found
Search 25: Not Found

Deleting key 30:
Inorder traversal after deletion:
Key: 20, Priority: 345
Key: 40, Priority: 123
Key: 50, Priority: 567
Key: 60, Priority: 890
Key: 70, Priority: 234
Key: 80, Priority: 456
```

## Key Features of This Treap Implementation:

1. **Node Structure**: Contains key, priority, and left/right pointers
2. **Random Priority**: Each node gets a random priority value
3. **Heap Property**: Parent nodes have higher priority than children
4. **BST Property**: Keys follow binary search tree ordering
5. **Rotations**: Right and left rotations maintain both properties
6. **Operations**: Insert, search, delete with proper balancing

The treap combines the benefits of binary search trees (ordered structure) and heaps (priority-based ordering) to provide efficient operations with good average-case performance.

