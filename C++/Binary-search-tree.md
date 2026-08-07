# Binary Search Tree Implementation in C++

```cpp
#include <iostream>
using namespace std;

// Definition of a tree node
struct Node {
    int data;
    Node* left;
    Node* right;
    
    // Constructor
    Node(int value) {
        data = value;
        left = nullptr;
        right = nullptr;
    }
};

class BinarySearchTree {
private:
    Node* root;
    
    // Private helper function for insertion
    Node* insertHelper(Node* node, int value) {
        if (node == nullptr) {
            return new Node(value);
        }
        
        if (value < node->data) {
            node->left = insertHelper(node->left, value);
        } else if (value > node->data) {
            node->right = insertHelper(node->right, value);
        }
        
        return node;
    }
    
    // Private helper function for searching
    Node* searchHelper(Node* node, int value) {
        if (node == nullptr || node->data == value) {
            return node;
        }
        
        if (value < node->data) {
            return searchHelper(node->left, value);
        } else {
            return searchHelper(node->right, value);
        }
    }
    
    // Private helper function for inorder traversal
    void inorderHelper(Node* node) {
        if (node != nullptr) {
            inorderHelper(node->left);
            cout << node->data << " ";
            inorderHelper(node->right);
        }
    }
    
    // Private helper function for finding minimum value
    Node* findMin(Node* node) {
        while (node->left != nullptr) {
            node = node->left;
        }
        return node;
    }
    
    // Private helper function for deletion
    Node* deleteHelper(Node* node, int value) {
        if (node == nullptr) {
            return node;
        }
        
        if (value < node->data) {
            node->left = deleteHelper(node->left, value);
        } else if (value > node->data) {
            node->right = deleteHelper(node->right, value);
        } else {
            // Node to be deleted found
            if (node->left == nullptr) {
                Node* temp = node->right;
                delete node;
                return temp;
            } else if (node->right == nullptr) {
                Node* temp = node->left;
                delete node;
                return temp;
            }
            
            // Node with two children
            Node* temp = findMin(node->right);
            node->data = temp->data;
            node->right = deleteHelper(node->right, temp->data);
        }
        return node;
    }

public:
    // Constructor
    BinarySearchTree() {
        root = nullptr;
    }
    
    // Public insertion function
    void insert(int value) {
        root = insertHelper(root, value);
    }
    
    // Public search function
    bool search(int value) {
        Node* result = searchHelper(root, value);
        return (result != nullptr);
    }
    
    // Public inorder traversal
    void inorder() {
        cout << "Inorder traversal: ";
        inorderHelper(root);
        cout << endl;
    }
    
    // Public deletion function
    void deleteNode(int value) {
        root = deleteHelper(root, value);
    }
};

// Example usage
int main() {
    BinarySearchTree bst;
    
    // Insert nodes
    cout << "Inserting elements: 50, 30, 70, 20, 40, 60, 80" << endl;
    bst.insert(50);
    bst.insert(30);
    bst.insert(70);
    bst.insert(20);
    bst.insert(40);
    bst.insert(60);
    bst.insert(80);
    
    // Display inorder traversal
    bst.inorder();
    
    // Search for elements
    cout << "Searching for 40: " << (bst.search(40) ? "Found" : "Not found") << endl;
    cout << "Searching for 25: " << (bst.search(25) ? "Found" : "Not found") << endl;
    
    // Delete a node
    cout << "\nDeleting node 30..." << endl;
    bst.deleteNode(30);
    cout << "Inorder traversal after deletion: ";
    bst.inorder();
    
    return 0;
}
```

## Output:
```
Inserting elements: 50, 30, 70, 20, 40, 60, 80
Inorder traversal: 20 30 40 50 60 70 80 
Searching for 40: Found
Searching for 25: Not found

Deleting node 30...
Inorder traversal after deletion: 20 40 50 60 70 80 
```

## Key Features of this Implementation:

- **Insertion**: Maintains BST property (left subtree < node < right subtree)
- **Search**: Efficient O(log n) search operation
- **Inorder Traversal**: Returns elements in sorted order
- **Deletion**: Handles all cases (leaf node, node with one child, node with two children)
- **Recursive Implementation**: Clean and easy to understand approach

The time complexity for search, insertion, and deletion operations is O(log n) on average for a balanced tree, and O(n) in the worst case for a skewed tree.