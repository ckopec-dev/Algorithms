# AVL Tree Balancing Algorithm in Go

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

// RightRotate performs a right rotation
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

// LeftRotate performs a left rotation
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
    tree.root = tree.insertHelper(tree.root, val)
    return tree
}

// insertHelper is the recursive helper function for insertion
func (tree *AVLTree) insertHelper(node *TreeNode, val int) *TreeNode {
    // Step 1: Perform normal BST insertion
    if node == nil {
        newNode := &TreeNode{val: val, height: 1}
        return newNode
    }

    if val < node.val {
        node.left = tree.insertHelper(node.left, val)
    } else if val > node.val {
        node.right = tree.insertHelper(node.right, val)
    } else {
        // Duplicate values are not allowed
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
        fmt.Printf("%s", prefix)
        if isLast {
            fmt.Printf("└── ")
            prefix += "    "
        } else {
            fmt.Printf("├── ")
            prefix += "│   "
        }
        fmt.Printf("[%d:%d]\n", node.val, node.GetBalanceFactor())
        if node.left != nil || node.right != nil {
            if node.left != nil {
                tree.printHelper(node.left, prefix, node.right == nil)
            }
            if node.right != nil {
                tree.printHelper(node.right, prefix, true)
            }
        }
    }
}

// GetHeight returns the height of the entire tree
func (tree *AVLTree) GetHeight() int {
    return tree.root.GetHeight()
}

func main() {
    avl := &AVLTree{}
    
    fmt.Println("AVL Tree Balancing Example")
    fmt.Println("==========================")
    
    // Insert values that will require rotations
    values := []int{10, 20, 30, 40, 50, 25}
    
    fmt.Println("Inserting values:", values)
    
    for _, val := range values {
        fmt.Printf("\nInserting %d:\n", val)
        avl.Insert(val)
        fmt.Println("Tree structure:")
        avl.PrintTree()
        fmt.Printf("In-order traversal: %v\n", avl.InOrderTraversal())
        fmt.Printf("Tree height: %d\n", avl.GetHeight())
    }
    
    fmt.Println("\nFinal AVL Tree:")
    avl.PrintTree()
    fmt.Printf("Final in-order traversal: %v\n", avl.InOrderTraversal())
    
    // Demonstrate more rotations
    fmt.Println("\nDemonstrating more rotations:")
    avl2 := &AVLTree{}
    
    // Insert values that will cause Left-Right rotation
    moreValues := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    fmt.Println("Inserting values:", moreValues)
    
    for _, val := range moreValues {
        avl2.Insert(val)
    }
    
    fmt.Println("\nFinal tree after multiple insertions:")
    avl2.PrintTree()
    fmt.Printf("Final in-order traversal: %v\n", avl2.InOrderTraversal())
    fmt.Printf("Final tree height: %d\n", avl2.GetHeight())
}
```

## Key Components of the AVL Tree Implementation

### 1. **TreeNode Structure**
- `val`: The value stored in the node
- `left` and `right`: Pointers to left and right children
- `height`: Height of the node for balance calculations

### 2. **Core Functions**

#### **Rotation Operations**
- `RightRotate()`: Performs right rotation for Left-Left case
- `LeftRotate()`: Performs left rotation for Right-Right case

#### **Balance Operations**
- `GetBalanceFactor()`: Calculates balance factor (height of left - height of right)
- `UpdateHeight()`: Updates node height after operations

#### **Insertion with Balancing**
- `Insert()`: Main insertion function that maintains AVL property
- `insertHelper()`: Recursive helper that performs rotations when needed

### 3. **Rotation Cases**
1. **Left-Left Case**: Right rotation
2. **Right-Right Case**: Left rotation  
3. **Left-Right Case**: Left rotation on left child, then right rotation
4. **Right-Left Case**: Right rotation on right child, then left rotation

### 4. **Time Complexity**
- **Insertion**: O(log n)
- **Deletion**: O(log n)
- **Search**: O(log n)

### 5. **Space Complexity**
- O(n) where n is the number of nodes

This implementation demonstrates how the AVL tree automatically maintains its balanced property through rotations whenever insertions cause the balance factor to exceed ±1.

