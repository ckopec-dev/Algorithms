# AVL Tree Balancing Algorithm in Go

Here's a complete implementation of an AVL tree with balancing operations in Go:

```go
package main

import (
    "fmt"
    "math"
)

// TreeNode represents a node in the AVL tree
type TreeNode struct {
    val   int
    left  *TreeNode
    right *TreeNode
    height int
}

// AVLTree represents the AVL tree structure
type AVLTree struct {
    root *TreeNode
}

// GetHeight returns the height of a node
func (node *TreeNode) GetHeight() int {
    if node == nil {
        return 0
    }
    return node.height
}

// GetBalanceFactor returns the balance factor of a node
func (node *TreeNode) GetBalanceFactor() int {
    if node == nil {
        return 0
    }
    return node.left.GetHeight() - node.right.GetHeight()
}

// UpdateHeight updates the height of a node
func (node *TreeNode) UpdateHeight() {
    if node != nil {
        node.height = 1 + int(math.Max(float64(node.left.GetHeight()), float64(node.right.GetHeight())))
    }
}

// RightRotate performs right rotation
func (tree *AVLTree) RightRotate(y *TreeNode) *TreeNode {
    x := y.left
    T2 := x.right

    // Perform rotation
    x.right = y
    y.left = T2

    // Update heights
    y.UpdateHeight()
    x.UpdateHeight()

    return x
}

// LeftRotate performs left rotation
func (tree *AVLTree) LeftRotate(x *TreeNode) *TreeNode {
    y := x.right
    T2 := y.left

    // Perform rotation
    y.left = x
    x.right = T2

    // Update heights
    x.UpdateHeight()
    y.UpdateHeight()

    return y
}

// Insert inserts a value into the AVL tree
func (tree *AVLTree) Insert(val int) *AVLTree {
    tree.root = tree.insertNode(tree.root, val)
    return tree
}

// insertNode recursively inserts a node and balances the tree
func (tree *AVLTree) insertNode(node *TreeNode, val int) *TreeNode {
    // Step 1: Perform normal BST insertion
    if node == nil {
        return &TreeNode{val: val, height: 1}
    }

    if val < node.val {
        node.left = tree.insertNode(node.left, val)
    } else if val > node.val {
        node.right = tree.insertNode(node.right, val)
    } else {
        // Duplicate values not allowed
        return node
    }

    // Step 2: Update height of current node
    node.UpdateHeight()

    // Step 3: Get balance factor
    balance := node.GetBalanceFactor()

    // Step 4: Perform rotations if needed

    // Left Left Case
    if balance > 1 && val < node.left.val {
        return tree.RightRotate(node)
    }

    // Right Right Case
    if balance < -1 && val > node.right.val {
        return tree.LeftRotate(node)
    }

    // Left Right Case
    if balance > 1 && val > node.left.val {
        node.left = tree.LeftRotate(node.left)
        return tree.RightRotate(node)
    }

    // Right Left Case
    if balance < -1 && val < node.right.val {
        node.right = tree.RightRotate(node.right)
        return tree.LeftRotate(node)
    }

    return node
}

// InOrderTraversal performs in-order traversal
func (tree *AVLTree) InOrderTraversal() []int {
    var result []int
    tree.inOrderHelper(tree.root, &result)
    return result
}

func (tree *AVLTree) inOrderHelper(node *TreeNode, result *[]int) {
    if node != nil {
        tree.inOrderHelper(node.left, result)
        *result = append(*result, node.val)
        tree.inOrderHelper(node.right, result)
    }
}

// PrintTree prints the tree structure
func (tree *AVLTree) PrintTree() {
    if tree.root != nil {
        tree.printHelper(tree.root, "", true)
    }
}

func (tree *AVLTree) printHelper(node *TreeNode, prefix string, isLast bool) {
    if node != nil {
        fmt.Printf("%s%s%d(h=%d)\n", prefix, getBranch(isLast), node.val, node.height)
        
        if node.left != nil || node.right != nil {
            if node.left != nil {
                tree.printHelper(node.left, prefix+getPadding(isLast), false)
            }
            if node.right != nil {
                tree.printHelper(node.right, prefix+getPadding(isLast), true)
            }
        }
    }
}

func getBranch(isLast bool) string {
    if isLast {
        return "└── "
    }
    return "├── "
}

func getPadding(isLast bool) string {
    if isLast {
        return "    "
    }
    return "│   "
}

// Example usage
func main() {
    avl := &AVLTree{}
    
    fmt.Println("Inserting values: 10, 20, 30, 40, 50, 25")
    
    // Insert values one by one
    values := []int{10, 20, 30, 40, 50, 25}
    for _, val := range values {
        avl.Insert(val)
        fmt.Printf("After inserting %d:\n", val)
        avl.PrintTree()
        fmt.Println()
    }
    
    fmt.Println("In-order traversal:", avl.InOrderTraversal())
    
    // Insert more values to trigger more rotations
    fmt.Println("\nInserting additional values: 5, 15, 35")
    additionalValues := []int{5, 15, 35}
    for _, val := range additionalValues {
        avl.Insert(val)
        fmt.Printf("After inserting %d:\n", val)
        avl.PrintTree()
        fmt.Println()
    }
    
    fmt.Println("Final in-order traversal:", avl.InOrderTraversal())
}
```

## Key Components of the AVL Tree Implementation:

### 1. **Node Structure**
```go
type TreeNode struct {
    val   int
    left  *TreeNode
    right *TreeNode
    height int
}
```

### 2. **Balance Operations**
- **Height Calculation**: `GetHeight()` and `UpdateHeight()`
- **Balance Factor**: `GetBalanceFactor()` 
- **Rotations**: `LeftRotate()` and `RightRotate()`

### 3. **Rotation Types**
- **LL Rotation**: Left-Left case (single right rotation)
- **RR Rotation**: Right-Right case (single left rotation)
- **LR Rotation**: Left-Right case (left rotation on left child, then right rotation)
- **RL Rotation**: Right-Left case (right rotation on right child, then left rotation)

### 4. **Insertion Process**
1. Perform normal BST insertion
2. Update heights of all ancestors
3. Calculate balance factor
4. Perform appropriate rotations if balance factor > 1 or < -1

## Sample Output:
```
Inserting values: 10, 20, 30, 40, 50, 25
After inserting 10:
└── 10(h=1)

After inserting 20:
└── 10(h=2)
    └── 20(h=1)

After inserting 30:
└── 20(h=2)
    ├── 10(h=1)
    └── 30(h=1)

After inserting 40:
└── 20(h=3)
    ├── 10(h=1)
    └── 30(h=2)
        └── 40(h=1)
```

This implementation demonstrates how AVL trees automatically maintain balance through rotations during insertion operations, ensuring O(log n) time complexity for all operations.

