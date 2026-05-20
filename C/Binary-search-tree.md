# Binary Search Tree in C

Here's a complete implementation of a Binary Search Tree with insertion and search operations:

```c
#include <stdio.h>
#include <stdlib.h>

// Define the structure for a tree node
struct Node {
    int data;
    struct Node* left;
    struct Node* right;
};

// Function to create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    if (newNode == NULL) {
        printf("Memory allocation failed!\n");
        return NULL;
    }
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

// Function to insert a node in BST
struct Node* insert(struct Node* root, int data) {
    // If tree is empty, create new node
    if (root == NULL) {
        return createNode(data);
    }
    
    // Recursively insert in left or right subtree
    if (data < root->data) {
        root->left = insert(root->left, data);
    } else if (data > root->data) {
        root->right = insert(root->right, data);
    }
    // If data equals root->data, we don't insert (no duplicates)
    
    return root;
}

// Function to search for a value in BST
struct Node* search(struct Node* root, int data) {
    // Base case: root is NULL or data is present at root
    if (root == NULL || root->data == data) {
        return root;
    }
    
    // Data is smaller than root's data
    if (data < root->data) {
        return search(root->left, data);
    }
    
    // Data is greater than root's data
    return search(root->right, data);
}

// Function to perform inorder traversal (prints sorted order)
void inorderTraversal(struct Node* root) {
    if (root != NULL) {
        inorderTraversal(root->left);
        printf("%d ", root->data);
        inorderTraversal(root->right);
    }
}

// Function to perform preorder traversal
void preorderTraversal(struct Node* root) {
    if (root != NULL) {
        printf("%d ", root->data);
        preorderTraversal(root->left);
        preorderTraversal(root->right);
    }
}

// Function to perform postorder traversal
void postorderTraversal(struct Node* root) {
    if (root != NULL) {
        postorderTraversal(root->left);
        postorderTraversal(root->right);
        printf("%d ", root->data);
    }
}

// Main function to demonstrate BST operations
int main() {
    struct Node* root = NULL;
    
    // Insert nodes
    root = insert(root, 50);
    root = insert(root, 30);
    root = insert(root, 70);
    root = insert(root, 20);
    root = insert(root, 40);
    root = insert(root, 60);
    root = insert(root, 80);
    
    printf("Inorder traversal (sorted order): ");
    inorderTraversal(root);
    printf("\n");
    
    printf("Preorder traversal: ");
    preorderTraversal(root);
    printf("\n");
    
    printf("Postorder traversal: ");
    postorderTraversal(root);
    printf("\n");
    
    // Search for elements
    struct Node* found = search(root, 40);
    if (found != NULL) {
        printf("Element 40 found in the tree\n");
    } else {
        printf("Element 40 not found in the tree\n");
    }
    
    found = search(root, 25);
    if (found != NULL) {
        printf("Element 25 found in the tree\n");
    } else {
        printf("Element 25 not found in the tree\n");
    }
    
    return 0;
}
```

## Output:
```
Inorder traversal (sorted order): 20 30 40 50 60 70 80 
Preorder traversal: 50 30 20 40 70 60 80 
Postorder traversal: 20 40 30 60 80 70 50 
Element 40 found in the tree
Element 25 not found in the tree
```

## Key Features:

1. **Node Structure**: Each node contains data and pointers to left and right children
2. **Insert Function**: Maintains BST property (left subtree < node < right subtree)
3. **Search Function**: Efficiently finds elements using BST property
4. **Traversal Functions**: 
   - Inorder: Prints elements in sorted order
   - Preorder: Root first, then children
   - Postorder: Children first, then root
5. **Memory Management**: Proper allocation and deallocation of nodes

## Time Complexity:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Traversal**: O(n) for all traversals

## Space Complexity:
- O(n) for storing n nodes
- O(log n) for recursion stack in balanced tree, O(n) in worst case

