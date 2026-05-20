# Morris Traversal Algorithm in C++

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's right pointers to create temporary links and then restores them.

## Code Implementation

```cpp
#include <iostream>
using namespace std;

// Definition for a binary tree node
struct TreeNode {
    int val;
    TreeNode* left;
    TreeNode* right;
    TreeNode() : val(0), left(nullptr), right(nullptr) {}
    TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
    TreeNode(int x, TreeNode* left, TreeNode* right) : val(x), left(left), right(right) {}
};

class MorrisTraversal {
public:
    // Morris traversal for inorder traversal
    void morrisInorder(TreeNode* root) {
        TreeNode* current = root;
        
        while (current != nullptr) {
            // If left is null, visit current node and go to right
            if (current->left == nullptr) {
                cout << current->val << " ";
                current = current->right;
            } else {
                // Find the inorder predecessor (rightmost node in left subtree)
                TreeNode* predecessor = current->left;
                while (predecessor->right != nullptr && predecessor->right != current) {
                    predecessor = predecessor->right;
                }
                
                // If right of predecessor is null, make current as right child
                if (predecessor->right == nullptr) {
                    predecessor->right = current;
                    current = current->left;
                } else {
                    // If right of predecessor is current, restore tree and visit current
                    predecessor->right = nullptr;
                    cout << current->val << " ";
                    current = current->right;
                }
            }
        }
    }
    
    // Morris traversal for preorder traversal
    void morrisPreorder(TreeNode* root) {
        TreeNode* current = root;
        
        while (current != nullptr) {
            // If left is null, visit current node and go to right
            if (current->left == nullptr) {
                cout << current->val << " ";
                current = current->right;
            } else {
                // Find the inorder predecessor
                TreeNode* predecessor = current->left;
                while (predecessor->right != nullptr && predecessor->right != current) {
                    predecessor = predecessor->right;
                }
                
                // If right of predecessor is null, make current as right child
                if (predecessor->right == nullptr) {
                    predecessor->right = current;
                    cout << current->val << " "; // Visit current node
                    current = current->left;
                } else {
                    // If right of predecessor is current, restore tree
                    predecessor->right = nullptr;
                    current = current->right;
                }
            }
        }
    }
};

// Example usage
int main() {
    // Create a sample binary tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    TreeNode* root = new TreeNode(1);
    root->left = new TreeNode(2);
    root->right = new TreeNode(3);
    root->left->left = new TreeNode(4);
    root->left->right = new TreeNode(5);
    
    MorrisTraversal mt;
    
    cout << "Morris Inorder Traversal: ";
    mt.morrisInorder(root);
    cout << endl;
    
    cout << "Morris Preorder Traversal: ";
    mt.morrisPreorder(root);
    cout << endl;
    
    return 0;
}
```

## Output
```
Morris Inorder Traversal: 4 2 5 1 3 
Morris Preorder Traversal: 1 2 4 5 3 
```

## How it works:

1. **Inorder Traversal**:
   - When we encounter a node with no left child, we visit it and move to the right
   - When we have a left child, we find its inorder predecessor and create a temporary link
   - After visiting the left subtree, we restore the tree and continue with the right subtree

2. **Preorder Traversal**:
   - Similar logic but we visit the node immediately when we create the temporary link
   - This ensures we visit nodes in preorder sequence

## Time and Space Complexity:
- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - no extra space used except for temporary pointers

## Key Advantages:
- No recursion or stack needed
- Constant extra space usage
- Works for both inorder and preorder traversals
- Maintains the original tree structure during traversal

