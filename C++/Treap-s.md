# Treap Implementation in C++

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has a key (for BST property) and a priority (for heap property).

```cpp
#include <iostream>
#include <random>
#include <algorithm>

struct TreapNode {
    int key;
    int priority;
    TreapNode* left;
    TreapNode* right;
    
    TreapNode(int k) : key(k), priority(rand() % 1000), left(nullptr), right(nullptr) {}
};

class Treap {
private:
    TreapNode* root;
    
    // Right rotation
    TreapNode* rotateRight(TreapNode* y) {
        TreapNode* x = y->left;
        TreapNode* T2 = x->right;
        
        x->right = y;
        y->left = T2;
        
        return x;
    }
    
    // Left rotation
    TreapNode* rotateLeft(TreapNode* x) {
        TreapNode* y = x->right;
        TreapNode* T2 = y->left;
        
        y->left = x;
        x->right = T2;
        
        return y;
    }
    
    // Insert a node recursively
    TreapNode* insert(TreapNode* node, int key) {
        if (node == nullptr) {
            return new TreapNode(key);
        }
        
        if (key < node->key) {
            node->left = insert(node->left, key);
            
            // Heap property violation - rotate right
            if (node->left->priority > node->priority) {
                node = rotateRight(node);
            }
        } else if (key > node->key) {
            node->right = insert(node->right, key);
            
            // Heap property violation - rotate left
            if (node->right->priority > node->priority) {
                node = rotateLeft(node);
            }
        }
        // If key equals node->key, we don't insert duplicates
        
        return node;
    }
    
    // Search for a key
    bool search(TreapNode* node, int key) {
        if (node == nullptr) {
            return false;
        }
        
        if (key == node->key) {
            return true;
        }
        
        if (key < node->key) {
            return search(node->left, key);
        } else {
            return search(node->right, key);
        }
    }
    
    // Inorder traversal
    void inorder(TreapNode* node) {
        if (node != nullptr) {
            inorder(node->left);
            std::cout << "Key: " << node->key << ", Priority: " << node->priority << std::endl;
            inorder(node->right);
        }
    }
    
    // Delete a node
    TreapNode* remove(TreapNode* node, int key) {
        if (node == nullptr) {
            return node;
        }
        
        if (key < node->key) {
            node->left = remove(node->left, key);
        } else if (key > node->key) {
            node->right = remove(node->right, key);
        } else {
            // Node to be deleted found
            if (node->left == nullptr) {
                TreapNode* temp = node->right;
                delete node;
                return temp;
            } else if (node->right == nullptr) {
                TreapNode* temp = node->left;
                delete node;
                return temp;
            }
            
            // Node with two children
            if (node->left->priority > node->right->priority) {
                node = rotateRight(node);
                node->right = remove(node->right, key);
            } else {
                node = rotateLeft(node);
                node->left = remove(node->left, key);
            }
        }
        
        return node;
    }
    
    // Helper function to delete all nodes
    void destroyTree(TreapNode* node) {
        if (node != nullptr) {
            destroyTree(node->left);
            destroyTree(node->right);
            delete node;
        }
    }
    
public:
    Treap() : root(nullptr) {
        srand(time(nullptr)); // Initialize random seed
    }
    
    ~Treap() {
        destroyTree(root);
    }
    
    void insert(int key) {
        root = insert(root, key);
    }
    
    bool search(int key) {
        return search(root, key);
    }
    
    void remove(int key) {
        root = remove(root, key);
    }
    
    void inorderTraversal() {
        std::cout << "Treap Inorder Traversal:" << std::endl;
        inorder(root);
        std::cout << std::endl;
    }
};

// Example usage
int main() {
    Treap treap;
    
    // Insert elements
    std::cout << "Inserting elements: 10, 20, 5, 15, 30, 8" << std::endl;
    treap.insert(10);
    treap.insert(20);
    treap.insert(5);
    treap.insert(15);
    treap.insert(30);
    treap.insert(8);
    
    // Display treap
    treap.inorderTraversal();
    
    // Search for elements
    std::cout << "Searching for 15: " << (treap.search(15) ? "Found" : "Not Found") << std::endl;
    std::cout << "Searching for 25: " << (treap.search(25) ? "Found" : "Not Found") << std::endl;
    
    // Remove an element
    std::cout << "\nRemoving element 15" << std::endl;
    treap.remove(15);
    
    std::cout << "After removal:" << std::endl;
    treap.inorderTraversal();
    
    return 0;
}
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, and pointers to left and right children
2. **Insert Operation**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotations**: Right and left rotations to maintain heap property
4. **Search Operation**: Standard BST search
5. **Delete Operation**: Complex deletion that maintains both properties
6. **Random Priority**: Each node gets a random priority for heap property

## Time Complexity:
- **Average Case**: O(log n) for insert, search, and delete
- **Worst Case**: O(n) for insert, search, and delete (when tree becomes skewed)

## Output Example:
```
Inserting elements: 10, 20, 5, 15, 30, 8
Treap Inorder Traversal:
Key: 5, Priority: 234
Key: 8, Priority: 567
Key: 10, Priority: 890
Key: 15, Priority: 123
Key: 20, Priority: 456
Key: 30, Priority: 789

Searching for 15: Found
Searching for 25: Not Found

Removing element 15
After removal:
Key: 5, Priority: 234
Key: 8, Priority: 567
Key: 10, Priority: 890
Key: 20, Priority: 456
Key: 30, Priority: 789
```

