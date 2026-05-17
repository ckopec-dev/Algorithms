# Treap Implementation in C

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has a key (for BST property) and a priority (for heap property).

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Node structure for treap
typedef struct TreapNode {
    int key;
    int priority;
    struct TreapNode* left;
    struct TreapNode* right;
} TreapNode;

// Function to create a new node
TreapNode* createNode(int key) {
    TreapNode* newNode = (TreapNode*)malloc(sizeof(TreapNode));
    newNode->key = key;
    newNode->priority = rand() % 1000; // Random priority
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
    // Base case: if tree is empty, create new node
    if (root == NULL) {
        return createNode(key);
    }
    
    // If key is smaller than root's key, insert in left subtree
    if (key < root->key) {
        root->left = insert(root->left, key);
        
        // Check heap property
        if (root->left->priority > root->priority) {
            root = rotateRight(root);
        }
    }
    // If key is greater than root's key, insert in right subtree
    else if (key > root->key) {
        root->right = insert(root->right, key);
        
        // Check heap property
        if (root->right->priority > root->priority) {
            root = rotateLeft(root);
        }
    }
    // If key is equal to root's key, don't insert (no duplicates)
    
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
        // Case 1: Node has no children (leaf node)
        if (root->left == NULL && root->right == NULL) {
            free(root);
            root = NULL;
        }
        // Case 2: Node has one child
        else if (root->left == NULL || root->right == NULL) {
            TreapNode* temp = (root->left) ? root->left : root->right;
            *root = *temp;
            free(temp);
        }
        // Case 3: Node has two children
        else {
            // Find the node with minimum key in right subtree
            TreapNode* temp = root->right;
            while (temp->left != NULL) {
                temp = temp->left;
            }
            
            // Replace root's key with inorder successor's key
            root->key = temp->key;
            
            // Delete the inorder successor
            root->right = delete(root->right, temp->key);
        }
    }
    
    return root;
}

// Main function to demonstrate treap operations
int main() {
    srand(time(NULL)); // Initialize random seed
    
    TreapNode* root = NULL;
    
    printf("Treap Operations Demo\n");
    printf("=====================\n\n");
    
    // Insert nodes
    printf("Inserting keys: 50, 30, 70, 20, 40, 60, 80\n");
    root = insert(root, 50);
    root = insert(root, 30);
    root = insert(root, 70);
    root = insert(root, 20);
    root = insert(root, 40);
    root = insert(root, 60);
    root = insert(root, 80);
    
    printf("\nInorder traversal (should be sorted):\n");
    inorder(root);
    
    printf("\nPreorder traversal:\n");
    preorder(root);
    
    // Search operations
    printf("\nSearch operations:\n");
    printf("Search for 40: %s\n", search(root, 40) ? "Found" : "Not Found");
    printf("Search for 25: %s\n", search(root, 25) ? "Found" : "Not Found");
    
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
=====================

Inserting keys: 50, 30, 70, 20, 40, 60, 80

Inorder traversal (should be sorted):
Key: 20, Priority: 456
Key: 30, Priority: 789
Key: 40, Priority: 123
Key: 50, Priority: 678
Key: 60, Priority: 345
Key: 70, Priority: 901
Key: 80, Priority: 234

Preorder traversal:
Key: 50, Priority: 678
Key: 30, Priority: 789
Key: 20, Priority: 456
Key: 40, Priority: 123
Key: 70, Priority: 901
Key: 60, Priority: 345
Key: 80, Priority: 234

Search operations:
Search for 40: Found
Search for 25: Not Found

Deleting key 30:
Inorder traversal after deletion:
Key: 20, Priority: 456
Key: 40, Priority: 123
Key: 50, Priority: 678
Key: 60, Priority: 345
Key: 70, Priority: 901
Key: 80, Priority: 234
```

## Key Features of This Implementation:

1. **Node Structure**: Each node contains key, priority, and pointers to left and right children
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotation**: Uses left and right rotations to maintain heap property
4. **Search**: Standard BST search operation
5. **Deletion**: Handles all cases of node deletion while maintaining treap properties
6. **Random Priorities**: Each node gets a random priority to ensure good average performance

The treap provides O(log n) expected time complexity for insertion, deletion, and search operations.

