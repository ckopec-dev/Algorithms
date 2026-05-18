# AVL Tree Balancing Algorithm in C

Here's a complete implementation of an AVL tree with balancing operations:

```c
#include <stdio.h>
#include <stdlib.h>

// AVL Tree Node Structure
struct Node {
    int data;
    struct Node* left;
    struct Node* right;
    int height;
};

// Function to get height of a node
int getHeight(struct Node* node) {
    if (node == NULL)
        return 0;
    return node->height;
}

// Function to get balance factor
int getBalance(struct Node* node) {
    if (node == NULL)
        return 0;
    return getHeight(node->left) - getHeight(node->right);
}

// Function to create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    newNode->height = 1;
    return newNode;
}

// Right rotation
struct Node* rotateRight(struct Node* y) {
    struct Node* x = y->left;
    struct Node* T2 = x->right;
    
    // Perform rotation
    x->right = y;
    y->left = T2;
    
    // Update heights
    y->height = 1 + (getHeight(y->left) > getHeight(y->right) ? 
                     getHeight(y->left) : getHeight(y->right));
    x->height = 1 + (getHeight(x->left) > getHeight(x->right) ? 
                     getHeight(x->left) : getHeight(x->right));
    
    return x;
}

// Left rotation
struct Node* rotateLeft(struct Node* x) {
    struct Node* y = x->right;
    struct Node* T2 = y->left;
    
    // Perform rotation
    y->left = x;
    x->right = T2;
    
    // Update heights
    x->height = 1 + (getHeight(x->left) > getHeight(x->right) ? 
                     getHeight(x->left) : getHeight(x->right));
    y->height = 1 + (getHeight(y->left) > getHeight(y->right) ? 
                     getHeight(y->left) : getHeight(y->right));
    
    return y;
}

// Insert function with balancing
struct Node* insert(struct Node* node, int data) {
    // Step 1: Perform normal BST insertion
    if (node == NULL)
        return createNode(data);
    
    if (data < node->data)
        node->left = insert(node->left, data);
    else if (data > node->data)
        node->right = insert(node->right, data);
    else
        return node; // Duplicate values not allowed
    
    // Step 2: Update height of current node
    node->height = 1 + (getHeight(node->left) > getHeight(node->right) ? 
                        getHeight(node->left) : getHeight(node->right));
    
    // Step 3: Get balance factor
    int balance = getBalance(node);
    
    // Step 4: Perform rotations if node becomes unbalanced
    
    // Left Left Case
    if (balance > 1 && data < node->left->data)
        return rotateRight(node);
    
    // Right Right Case
    if (balance < -1 && data > node->right->data)
        return rotateLeft(node);
    
    // Left Right Case
    if (balance > 1 && data > node->left->data) {
        node->left = rotateLeft(node->left);
        return rotateRight(node);
    }
    
    // Right Left Case
    if (balance < -1 && data < node->right->data) {
        node->right = rotateRight(node->right);
        return rotateLeft(node);
    }
    
    return node;
}

// Function to print tree in-order
void inorder(struct Node* root) {
    if (root != NULL) {
        inorder(root->left);
        printf("%d ", root->data);
        inorder(root->right);
    }
}

// Function to print tree structure (for visualization)
void printTree(struct Node* root, int space) {
    if (root == NULL)
        return;
    
    space += 10;
    printTree(root->right, space);
    
    printf("\n");
    for (int i = 10; i < space; i++)
        printf(" ");
    printf("%d\n", root->data);
    
    printTree(root->left, space);
}

// Main function to demonstrate AVL tree
int main() {
    struct Node* root = NULL;
    
    printf("AVL Tree Balancing Example\n");
    printf("==========================\n\n");
    
    // Insert nodes
    int keys[] = {10, 20, 30, 40, 50, 25};
    int n = sizeof(keys) / sizeof(keys[0]);
    
    printf("Inserting keys: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", keys[i]);
        root = insert(root, keys[i]);
    }
    printf("\n\n");
    
    printf("In-order traversal: ");
    inorder(root);
    printf("\n\n");
    
    printf("Tree structure:\n");
    printTree(root, 0);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Node Structure**
- Contains data, left/right pointers, and height field

### 2. **Balance Operations**
- **getHeight()**: Returns height of a node
- **getBalance()**: Calculates balance factor (left height - right height)
- **rotateLeft()**: Performs left rotation
- **rotateRight()**: Performs right rotation

### 3. **Insertion with Balancing**
- Normal BST insertion
- Height update after insertion
- Balance factor calculation
- Four rotation cases:
  - Left-Left (rotate right)
  - Right-Right (rotate left)
  - Left-Right (double rotation)
  - Right-Left (double rotation)

### 4. **Time Complexity**
- Insertion: O(log n)
- Search: O(log n)
- Height balancing: O(1) per operation

This implementation demonstrates how AVL trees maintain balance automatically through rotations whenever insertion operations cause the balance factor to exceed ±1.

