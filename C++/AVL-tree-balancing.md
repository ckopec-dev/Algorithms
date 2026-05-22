# AVL Tree Balancing Algorithm in C++

Here's a complete implementation of an AVL tree with balancing operations:

```cpp
#include <iostream>
#include <algorithm>
using namespace std;

// AVL Tree Node Structure
struct Node {
    int data;
    Node* left;
    Node* right;
    int height;
    
    Node(int val) : data(val), left(nullptr), right(nullptr), height(1) {}
};

class AVLTree {
private:
    Node* root;
    
    // Get height of node
    int getHeight(Node* node) {
        if (node == nullptr) return 0;
        return node->height;
    }
    
    // Get balance factor of node
    int getBalance(Node* node) {
        if (node == nullptr) return 0;
        return getHeight(node->left) - getHeight(node->right);
    }
    
    // Update height of node
    void updateHeight(Node* node) {
        if (node != nullptr) {
            node->height = 1 + max(getHeight(node->left), getHeight(node->right));
        }
    }
    
    // Right rotate
    Node* rotateRight(Node* y) {
        Node* x = y->left;
        Node* T2 = x->right;
        
        // Perform rotation
        x->right = y;
        y->left = T2;
        
        // Update heights
        updateHeight(y);
        updateHeight(x);
        
        return x;
    }
    
    // Left rotate
    Node* rotateLeft(Node* x) {
        Node* y = x->right;
        Node* T2 = y->left;
        
        // Perform rotation
        y->left = x;
        x->right = T2;
        
        // Update heights
        updateHeight(x);
        updateHeight(y);
        
        return y;
    }
    
    // Insert node recursively
    Node* insert(Node* node, int data) {
        // 1. Perform normal BST insertion
        if (node == nullptr) {
            return new Node(data);
        }
        
        if (data < node->data) {
            node->left = insert(node->left, data);
        } else if (data > node->data) {
            node->right = insert(node->right, data);
        } else {
            // Duplicate values not allowed
            return node;
        }
        
        // 2. Update height of current node
        updateHeight(node);
        
        // 3. Get balance factor
        int balance = getBalance(node);
        
        // 4. Perform rotations if node becomes unbalanced
        
        // Left Left Case
        if (balance > 1 && data < node->left->data) {
            return rotateRight(node);
        }
        
        // Right Right Case
        if (balance < -1 && data > node->right->data) {
            return rotateLeft(node);
        }
        
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
    
    // Inorder traversal
    void inorder(Node* node) {
        if (node != nullptr) {
            inorder(node->left);
            cout << node->data << " ";
            inorder(node->right);
        }
    }
    
public:
    AVLTree() : root(nullptr) {}
    
    // Public insert function
    void insert(int data) {
        root = insert(root, data);
    }
    
    // Public inorder traversal
    void inorder() {
        inorder(root);
        cout << endl;
    }
    
    // Get root height
    int getRootHeight() {
        return getHeight(root);
    }
};

// Example usage
int main() {
    AVLTree avl;
    
    cout << "Inserting elements: 10, 20, 30, 40, 50, 25" << endl;
    
    // Insert elements
    avl.insert(10);
    avl.insert(20);
    avl.insert(30);
    avl.insert(40);
    avl.insert(50);
    avl.insert(25);
    
    cout << "Inorder traversal of AVL tree: ";
    avl.inorder();
    
    cout << "Root height: " << avl.getRootHeight() << endl;
    
    return 0;
}
```

## Key Balancing Operations Explained:

### 1. **Rotation Types:**
- **Right Rotation (LL Case)**: When left child's left subtree is heavier
- **Left Rotation (RR Case)**: When right child's right subtree is heavier  
- **Left-Right Rotation (LR Case)**: When left child's right subtree is heavier
- **Right-Left Rotation (RL Case)**: When right child's left subtree is heavier

### 2. **Balance Factor Calculation:**
```
Balance Factor = Height(Left Subtree) - Height(Right Subtree)
```

### 3. **Balancing Conditions:**
- If balance factor > 1: Left heavy → perform right rotation
- If balance factor < -1: Right heavy → perform left rotation
- If balance factor is between -1 and 1: Balanced → no rotation needed

### 4. **Time Complexity:**
- **Insertion**: O(log n)
- **Deletion**: O(log n)  
- **Search**: O(log n)

### 5. **Space Complexity:**
- O(n) for storing n nodes

This implementation maintains the AVL property after every insertion, ensuring the tree remains balanced with a height of O(log n).

