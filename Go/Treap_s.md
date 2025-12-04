# Treap Implementation in Go

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has a key (for BST property) and a priority (for heap property).

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// Node represents a node in the treap
type Node struct {
    key      int
    priority int
    left     *Node
    right    *Node
}

// Treap represents the treap data structure
type Treap struct {
    root *Node
}

// NewTreap creates a new empty treap
func NewTreap() *Treap {
    return &Treap{}
}

// rotateRight performs a right rotation
func (t *Treap) rotateRight(y *Node) *Node {
    x := y.left
    y.left = x.right
    x.right = y
    return x
}

// rotateLeft performs a left rotation
func (t *Treap) rotateLeft(x *Node) *Node {
    y := x.right
    x.right = y.left
    y.left = x
    return y
}

// insert inserts a key into the treap
func (t *Treap) insert(key int) {
    t.root = t.insertHelper(t.root, key)
}

// insertHelper is the recursive helper function for insertion
func (t *Treap) insertHelper(node *Node, key int) *Node {
    if node == nil {
        // Create new node with random priority
        rand.Seed(time.Now().UnixNano())
        return &Node{
            key:      key,
            priority: rand.Intn(1000), // Random priority
            left:     nil,
            right:    nil,
        }
    }

    // Standard BST insertion
    if key < node.key {
        node.left = t.insertHelper(node.left, key)
        // Heap property: if priority of left child is less than parent, rotate right
        if node.left != nil && node.left.priority > node.priority {
            node = t.rotateRight(node)
        }
    } else if key > node.key {
        node.right = t.insertHelper(node.right, key)
        // Heap property: if priority of right child is less than parent, rotate left
        if node.right != nil && node.right.priority > node.priority {
            node = t.rotateLeft(node)
        }
    }
    // If key equals node.key, we don't insert (no duplicates)

    return node
}

// search searches for a key in the treap
func (t *Treap) search(key int) bool {
    return t.searchHelper(t.root, key)
}

// searchHelper is the recursive helper function for searching
func (t *Treap) searchHelper(node *Node, key int) bool {
    if node == nil {
        return false
    }
    
    if key == node.key {
        return true
    } else if key < node.key {
        return t.searchHelper(node.left, key)
    } else {
        return t.searchHelper(node.right, key)
    }
}

// delete removes a key from the treap
func (t *Treap) delete(key int) {
    t.root = t.deleteHelper(t.root, key)
}

// deleteHelper is the recursive helper function for deletion
func (t *Treap) deleteHelper(node *Node, key int) *Node {
    if node == nil {
        return nil
    }

    if key < node.key {
        node.left = t.deleteHelper(node.left, key)
    } else if key > node.key {
        node.right = t.deleteHelper(node.right, key)
    } else {
        // Key found, perform deletion
        if node.left == nil {
            return node.right
        } else if node.right == nil {
            return node.left
        }

        // Both children exist, find the minimum in right subtree
        if node.left.priority > node.right.priority {
            node = t.rotateRight(node)
            node.right = t.deleteHelper(node.right, key)
        } else {
            node = t.rotateLeft(node)
            node.left = t.deleteHelper(node.left, key)
        }
    }

    return node
}

// inorderTraversal performs inorder traversal (returns sorted keys)
func (t *Treap) inorderTraversal() []int {
    var result []int
    t.inorderHelper(t.root, &result)
    return result
}

// inorderHelper is the recursive helper for inorder traversal
func (t *Treap) inorderHelper(node *Node, result *[]int) {
    if node != nil {
        t.inorderHelper(node.left, result)
        *result = append(*result, node.key)
        t.inorderHelper(node.right, result)
    }
}

// printTree prints the treap structure (for debugging)
func (t *Treap) printTree() {
    t.printHelper(t.root, "", true)
}

// printHelper is the recursive helper for printing tree structure
func (t *Treap) printHelper(node *Node, prefix string, isLast bool) {
    if node != nil {
        fmt.Printf("%s%s%d(p:%d)\n", prefix, getBranch(isLast), node.key, node.priority)
        
        if node.left != nil || node.right != nil {
            if node.left != nil {
                t.printHelper(node.left, prefix+getConnector(isLast), false)
            }
            if node.right != nil {
                t.printHelper(node.right, prefix+getConnector(isLast), true)
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

func getConnector(isLast bool) string {
    if isLast {
        return "    "
    }
    return "│   "
}

// Example usage
func main() {
    treap := NewTreap()
    
    fmt.Println("=== Treap Example ===")
    
    // Insert keys
    keys := []int{50, 30, 70, 20, 40, 60, 80}
    fmt.Printf("Inserting keys: %v\n", keys)
    
    for _, key := range keys {
        treap.insert(key)
    }
    
    fmt.Println("\nTreap structure:")
    treap.printTree()
    
    fmt.Println("\nInorder traversal (sorted keys):")
    fmt.Println(treap.inorderTraversal())
    
    // Search for keys
    fmt.Println("\nSearching for keys:")
    fmt.Printf("Search 40: %t\n", treap.search(40))
    fmt.Printf("Search 25: %t\n", treap.search(25))
    
    // Delete a key
    fmt.Println("\nDeleting key 30:")
    treap.delete(30)
    
    fmt.Println("Treap structure after deletion:")
    treap.printTree()
    
    fmt.Println("\nInorder traversal after deletion:")
    fmt.Println(treap.inorderTraversal())
}
```

## Key Features of this Treap Implementation:

### 1. **Node Structure**
- `key`: The value for BST ordering
- `priority`: Random priority for heap property
- `left` and `right`: Pointers to child nodes

### 2. **Core Operations**
- **Insert**: Maintains both BST and heap properties
- **Search**: Standard BST search
- **Delete**: Handles rotation to maintain heap property

### 3. **Rotation Operations**
- **Right Rotation**: Used when left child has higher priority
- **Left Rotation**: Used when right child has higher priority

### 4. **Properties Maintained**
- **BST Property**: Left subtree contains smaller keys, right subtree contains larger keys
- **Heap Property**: Parent has higher priority than children

### 5. **Time Complexity**
- **Average**: O(log n) for insert, search, delete
- **Worst**: O(n) for unbalanced cases
- **Space**: O(n) for storing n nodes

This implementation demonstrates how treaps combine the advantages of binary search trees (efficient searching) and heaps (randomized balancing) to provide efficient operations with good average-case performance.

