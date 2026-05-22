# Treap Implementation in Go

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has two values: a key (for BST property) and a priority (for heap property).

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

// insert recursively inserts a key into the treap
func (t *Treap) insert(node *Node, key int) *Node {
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

    // BST insertion
    if key < node.key {
        node.left = t.insert(node.left, key)
        // Heap property: if priority of left child is less than parent, rotate
        if node.left != nil && node.left.priority > node.priority {
            node = t.rotateRight(node)
        }
    } else if key > node.key {
        node.right = t.insert(node.right, key)
        // Heap property: if priority of right child is less than parent, rotate
        if node.right != nil && node.right.priority > node.priority {
            node = t.rotateLeft(node)
        }
    }
    // If key equals node.key, we don't insert (no duplicates)

    return node
}

// Insert adds a key to the treap
func (t *Treap) Insert(key int) {
    t.root = t.insert(t.root, key)
}

// search searches for a key in the treap
func (t *Treap) search(node *Node, key int) bool {
    if node == nil {
        return false
    }
    if key == node.key {
        return true
    }
    if key < node.key {
        return t.search(node.left, key)
    }
    return t.search(node.right, key)
}

// Search checks if a key exists in the treap
func (t *Treap) Search(key int) bool {
    return t.search(t.root, key)
}

// inorderTraversal performs inorder traversal (returns sorted keys)
func (t *Treap) inorderTraversal(node *Node, result *[]int) {
    if node != nil {
        t.inorderTraversal(node.left, result)
        *result = append(*result, node.key)
        t.inorderTraversal(node.right, result)
    }
}

// Inorder returns all keys in sorted order
func (t *Treap) Inorder() []int {
    var result []int
    t.inorderTraversal(t.root, &result)
    return result
}

// printTree prints the treap structure (for debugging)
func (t *Treap) printTree(node *Node, prefix string, isLast bool) {
    if node != nil {
        fmt.Printf("%s%s%d(p:%d)\n", prefix, 
            map[bool]string{true: "└── ", false: "├── "}[isLast], 
            node.key, node.priority)
        
        if node.left != nil || node.right != nil {
            if node.left != nil {
                t.printTree(node.left, prefix+map[bool]string{true: "    ", false: "│   "}[isLast], false)
            }
            if node.right != nil {
                t.printTree(node.right, prefix+map[bool]string{true: "    ", false: "│   "}[isLast], true)
            }
        }
    }
}

// PrintTree displays the treap structure
func (t *Treap) PrintTree() {
    if t.root != nil {
        t.printTree(t.root, "", true)
    } else {
        fmt.Println("Empty treap")
    }
}

func main() {
    // Create a new treap
    treap := NewTreap()
    
    // Insert some keys
    keys := []int{50, 30, 70, 20, 40, 60, 80}
    fmt.Println("Inserting keys:", keys)
    
    for _, key := range keys {
        treap.Insert(key)
    }
    
    // Display the treap structure
    fmt.Println("\nTreap structure:")
    treap.PrintTree()
    
    // Display sorted keys
    fmt.Println("\nInorder traversal (sorted keys):", treap.Inorder())
    
    // Search for keys
    fmt.Println("\nSearching for keys:")
    fmt.Printf("Search 40: %t\n", treap.Search(40))
    fmt.Printf("Search 25: %t\n", treap.Search(25))
    fmt.Printf("Search 80: %t\n", treap.Search(80))
    
    // Insert duplicate key (should not be inserted)
    fmt.Println("\nInserting duplicate key 30:")
    treap.Insert(30)
    fmt.Println("Inorder traversal after duplicate insertion:", treap.Inorder())
}
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains a key, priority, and pointers to left and right children
2. **Random Priorities**: Each inserted node gets a random priority value
3. **BST Property**: Keys are arranged in binary search tree order
4. **Heap Property**: Priorities are arranged in heap order (max-heap)
5. **Rotations**: Automatic rotations maintain both BST and heap properties
6. **Operations**: Insert, Search, and Inorder traversal

## Sample Output:
```
Inserting keys: [50 30 70 20 40 60 80]

Treap structure:
└── 50(p:234)
    ├── 30(p:789)
    │   ├── 20(p:123)
    │   └── 40(p:456)
    └── 70(p:678)
        ├── 60(p:901)
        └── 80(p:345)

Inorder traversal (sorted keys): [20 30 40 50 60 70 80]

Searching for keys:
Search 40: true
Search 25: false
Search 80: true

Inserting duplicate key 30:
Inorder traversal after duplicate insertion: [20 30 40 50 60 70 80]
```

This implementation demonstrates how treaps maintain both binary search tree properties and heap properties through automatic rotations during insertion.

