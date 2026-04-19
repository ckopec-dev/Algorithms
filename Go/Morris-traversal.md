# Morris Traversal Algorithm in Go

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses threading to traverse the tree in O(1) space complexity.

```go
package main

import "fmt"

// TreeNode represents a node in a binary tree
type TreeNode struct {
    Val   int
    Left  *TreeNode
    Right *TreeNode
}

// morrisTraversal performs inorder traversal using Morris algorithm
func morrisTraversal(root *TreeNode) []int {
    var result []int
    current := root
    
    for current != nil {
        // If left is null, visit current node and go to right
        if current.Left == nil {
            result = append(result, current.Val)
            current = current.Right
        } else {
            // Find the inorder predecessor (rightmost node in left subtree)
            predecessor := current.Left
            for predecessor.Right != nil && predecessor.Right != current {
                predecessor = predecessor.Right
            }
            
            // If predecessor's right is null, make current as right child
            // and move to left subtree
            if predecessor.Right == nil {
                predecessor.Right = current
                current = current.Left
            } else {
                // If predecessor's right is current, revert the changes
                // Visit current node and move to right subtree
                predecessor.Right = nil
                result = append(result, current.Val)
                current = current.Right
            }
        }
    }
    
    return result
}

// morrisPreorderTraversal performs preorder traversal using Morris algorithm
func morrisPreorderTraversal(root *TreeNode) []int {
    var result []int
    current := root
    
    for current != nil {
        if current.Left == nil {
            result = append(result, current.Val)
            current = current.Right
        } else {
            predecessor := current.Left
            for predecessor.Right != nil && predecessor.Right != current {
                predecessor = predecessor.Right
            }
            
            if predecessor.Right == nil {
                predecessor.Right = current
                result = append(result, current.Val) // Visit current in preorder
                current = current.Left
            } else {
                predecessor.Right = nil
                current = current.Right
            }
        }
    }
    
    return result
}

// Helper function to create a new tree node
func newNode(val int) *TreeNode {
    return &TreeNode{Val: val}
}

func main() {
    // Create a sample binary tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    root := newNode(1)
    root.Left = newNode(2)
    root.Right = newNode(3)
    root.Left.Left = newNode(4)
    root.Left.Right = newNode(5)
    
    fmt.Println("Inorder traversal using Morris algorithm:")
    inorder := morrisTraversal(root)
    fmt.Println(inorder) // Output: [4 2 5 1 3]
    
    fmt.Println("Preorder traversal using Morris algorithm:")
    preorder := morrisPreorderTraversal(root)
    fmt.Println(preorder) // Output: [1 2 4 5 3]
    
    // Test with a more complex tree
    //       1
    //      / \
    //     2   3
    //    /   / \
    //   4  5   6
    
    root2 := newNode(1)
    root2.Left = newNode(2)
    root2.Right = newNode(3)
    root2.Left.Left = newNode(4)
    root2.Right.Left = newNode(5)
    root2.Right.Right = newNode(6)
    
    fmt.Println("\nComplex tree inorder traversal:")
    inorder2 := morrisTraversal(root2)
    fmt.Println(inorder2) // Output: [4 2 1 5 3 6]
}
```

## How Morris Traversal Works:

1. **Initialization**: Start with the root node
2. **Left Subtree Check**: If current node has no left child, visit it and move to right
3. **Find Predecessor**: If left child exists, find the inorder predecessor (rightmost node in left subtree)
4. **Thread Creation**: 
   - If predecessor's right is null, create a thread by pointing it to current node
   - Move to left subtree
5. **Thread Reversion**:
   - If predecessor's right points to current node, remove thread
   - Visit current node and move to right subtree

## Key Features:

- **Space Complexity**: O(1) - no recursion or stack used
- **Time Complexity**: O(n) - each edge visited at most twice
- **Traversal Types**: Can be adapted for inorder, preorder, and postorder
- **Threaded Tree**: Temporarily modifies tree structure to create threads

## Advantages:

- No stack or recursion needed
- Constant space usage
- Efficient for memory-constrained environments

## Disadvantages:

- Modifies the tree structure temporarily
- More complex than recursive approaches
- Not suitable for concurrent access without additional synchronization

