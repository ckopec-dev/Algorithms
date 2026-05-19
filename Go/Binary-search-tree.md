# Binary Search Tree in Go

Here's a complete implementation of a Binary Search Tree algorithm in Go:

```go
package main

import "fmt"

// TreeNode represents a node in the binary search tree
type TreeNode struct {
    val   int
    left  *TreeNode
    right *TreeNode
}

// BST represents a binary search tree
type BST struct {
    root *TreeNode
}

// Insert adds a new value to the BST
func (bst *BST) Insert(val int) {
    bst.root = bst.insertRecursive(bst.root, val)
}

func (bst *BST) insertRecursive(node *TreeNode, val int) *TreeNode {
    // Base case: if node is nil, create new node
    if node == nil {
        return &TreeNode{val: val}
    }
    
    // Recursive case: insert in left or right subtree
    if val < node.val {
        node.left = bst.insertRecursive(node.left, val)
    } else if val > node.val {
        node.right = bst.insertRecursive(node.right, val)
    }
    // If val == node.val, we don't insert (no duplicates)
    
    return node
}

// Search checks if a value exists in the BST
func (bst *BST) Search(val int) bool {
    return bst.searchRecursive(bst.root, val)
}

func (bst *BST) searchRecursive(node *TreeNode, val int) bool {
    // Base case: node not found
    if node == nil {
        return false
    }
    
    // Base case: node found
    if node.val == val {
        return true
    }
    
    // Recursive case: search in left or right subtree
    if val < node.val {
        return bst.searchRecursive(node.left, val)
    } else {
        return bst.searchRecursive(node.right, val)
    }
}

// InOrderTraversal prints the BST in sorted order
func (bst *BST) InOrderTraversal() {
    fmt.Print("In-order: ")
    bst.inOrderRecursive(bst.root)
    fmt.Println()
}

func (bst *BST) inOrderRecursive(node *TreeNode) {
    if node != nil {
        bst.inOrderRecursive(node.left)
        fmt.Print(node.val, " ")
        bst.inOrderRecursive(node.right)
    }
}

// PreOrderTraversal prints the BST in pre-order
func (bst *BST) PreOrderTraversal() {
    fmt.Print("Pre-order: ")
    bst.preOrderRecursive(bst.root)
    fmt.Println()
}

func (bst *BST) preOrderRecursive(node *TreeNode) {
    if node != nil {
        fmt.Print(node.val, " ")
        bst.preOrderRecursive(node.left)
        bst.preOrderRecursive(node.right)
    }
}

// PostOrderTraversal prints the BST in post-order
func (bst *BST) PostOrderTraversal() {
    fmt.Print("Post-order: ")
    bst.postOrderRecursive(bst.root)
    fmt.Println()
}

func (bst *BST) postOrderRecursive(node *TreeNode) {
    if node != nil {
        bst.postOrderRecursive(node.left)
        bst.postOrderRecursive(node.right)
        fmt.Print(node.val, " ")
    }
}

func main() {
    // Create a new BST
    bst := &BST{}
    
    // Insert values
    values := []int{50, 30, 70, 20, 40, 60, 80}
    fmt.Println("Inserting values:", values)
    
    for _, val := range values {
        bst.Insert(val)
    }
    
    // Display traversals
    bst.InOrderTraversal()    // Should print sorted order
    bst.PreOrderTraversal()   // Root first
    bst.PostOrderTraversal()  // Root last
    
    // Search for values
    fmt.Println("\nSearching for values:")
    fmt.Printf("Search 40: %t\n", bst.Search(40))
    fmt.Printf("Search 25: %t\n", bst.Search(25))
    fmt.Printf("Search 80: %t\n", bst.Search(80))
    fmt.Printf("Search 100: %t\n", bst.Search(100))
}
```

## Output:
```
Inserting values: [50 30 70 20 40 60 80]
In-order: 20 30 40 50 60 70 80 
Pre-order: 50 30 20 40 70 60 80 
Post-order: 20 40 30 60 80 70 50 

Searching for values:
Search 40: true
Search 25: false
Search 80: true
Search 100: false
```

## Key Features:

1. **TreeNode Structure**: Represents individual nodes with value and left/right pointers
2. **Insert Method**: Maintains BST property (left < parent < right)
3. **Search Method**: Efficient O(log n) search using BST property
4. **Traversal Methods**: 
   - In-order (sorted output)
   - Pre-order (root first)
   - Post-order (root last)
5. **No Duplicates**: Values equal to existing nodes are not inserted

## Time Complexities:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Space**: O(n) for storing n nodes

This implementation demonstrates the core principles of binary search trees with efficient searching and insertion operations.

