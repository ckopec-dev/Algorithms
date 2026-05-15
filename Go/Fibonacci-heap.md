# Fibonacci Heap Implementation in Go

```go
package main

import (
    "fmt"
    "math"
)

// Node represents a node in the Fibonacci heap
type Node struct {
    key       int
    degree    int
    marked    bool
    child     *Node
    parent    *Node
    left      *Node
    right     *Node
}

// FibonacciHeap represents the Fibonacci heap structure
type FibonacciHeap struct {
    minNode *Node
    n       int
}

// NewFibonacciHeap creates a new empty Fibonacci heap
func NewFibonacciHeap() *FibonacciHeap {
    return &FibonacciHeap{
        minNode: nil,
        n:       0,
    }
}

// Insert inserts a new key into the Fibonacci heap
func (fh *FibonacciHeap) Insert(key int) *Node {
    node := &Node{
        key:    key,
        degree: 0,
        marked: false,
        child:  nil,
        parent: nil,
        left:   nil,
        right:  nil,
    }
    
    // Make node a root list
    if fh.minNode == nil {
        fh.minNode = node
        node.left = node
        node.right = node
    } else {
        // Insert into root list
        node.right = fh.minNode
        node.left = fh.minNode.left
        fh.minNode.left.right = node
        fh.minNode.left = node
        
        // Update minimum if necessary
        if key < fh.minNode.key {
            fh.minNode = node
        }
    }
    
    fh.n++
    return node
}

// Minimum returns the minimum key in the heap
func (fh *FibonacciHeap) Minimum() int {
    if fh.minNode == nil {
        return math.MaxInt32
    }
    return fh.minNode.key
}

// ExtractMin removes and returns the minimum key
func (fh *FibonacciHeap) ExtractMin() int {
    if fh.minNode == nil {
        return math.MaxInt32
    }
    
    z := fh.minNode
    minKey := z.key
    
    // Add children to root list
    if z.child != nil {
        child := z.child
        current := child
        
        for {
            next := current.right
            
            // Remove from child list
            current.left.right = current.right
            current.right.left = current.left
            
            // Add to root list
            current.right = fh.minNode
            current.left = fh.minNode.left
            fh.minNode.left.right = current
            fh.minNode.left = current
            
            current.parent = nil
            
            if next == child {
                break
            }
            current = next
        }
    }
    
    // Remove z from root list
    z.left.right = z.right
    z.right.left = z.left
    
    // Update minNode
    if z == z.right {
        fh.minNode = nil
    } else {
        fh.minNode = z.right
        fh.consolidate()
    }
    
    fh.n--
    return minKey
}

// consolidate consolidates the root list
func (fh *FibonacciHeap) consolidate() {
    // Create array to hold roots of same degree
    maxDegree := int(math.Log2(float64(fh.n))) + 1
    A := make([]*Node, maxDegree+1)
    
    // Initialize all slots to nil
    for i := range A {
        A[i] = nil
    }
    
    // Get all root nodes
    rootList := make([]*Node, 0)
    current := fh.minNode
    
    if current != nil {
        rootList = append(rootList, current)
        for next := current.right; next != current; next = next.right {
            rootList = append(rootList, next)
        }
    }
    
    // Consolidate
    for _, w := range rootList {
        x := w
        d := x.degree
        
        for A[d] != nil {
            y := A[d]
            
            // Ensure x has smaller key
            if x.key > y.key {
                x, y = y, x
            }
            
            // Make y a child of x
            fh.link(y, x)
            
            A[d] = nil
            d++
        }
        
        A[d] = x
    }
    
    // Find new minimum
    fh.minNode = nil
    for i := 0; i <= maxDegree; i++ {
        if A[i] != nil {
            if fh.minNode == nil {
                fh.minNode = A[i]
                A[i].left = A[i]
                A[i].right = A[i]
            } else {
                // Insert into root list
                A[i].right = fh.minNode
                A[i].left = fh.minNode.left
                fh.minNode.left.right = A[i]
                fh.minNode.left = A[i]
                
                if A[i].key < fh.minNode.key {
                    fh.minNode = A[i]
                }
            }
        }
    }
}

// link makes y a child of x
func (fh *FibonacciHeap) link(y, x *Node) {
    // Remove y from root list
    y.left.right = y.right
    y.right.left = y.left
    
    // Make y a child of x
    if x.child == nil {
        x.child = y
        y.left = y
        y.right = y
    } else {
        y.right = x.child
        y.left = x.child.left
        x.child.left.right = y
        x.child.left = y
    }
    
    y.parent = x
    x.degree++
    y.marked = false
}

// DecreaseKey decreases the key of a node
func (fh *FibonacciHeap) DecreaseKey(x *Node, key int) {
    if key > x.key {
        return // Cannot increase key
    }
    
    x.key = key
    
    y := x.parent
    if y != nil && x.key < y.key {
        fh.cut(x, y)
        fh.cascadingCut(y)
    }
    
    if x.key < fh.minNode.key {
        fh.minNode = x
    }
}

// cut removes x from y's child list
func (fh *FibonacciHeap) cut(x, y *Node) {
    // Remove x from y's child list
    if x.right != x {
        x.left.right = x.right
        x.right.left = x.left
    } else {
        y.child = nil
    }
    
    y.degree--
    
    // Add x to root list
    x.right = fh.minNode
    x.left = fh.minNode.left
    fh.minNode.left.right = x
    fh.minNode.left = x
    
    x.parent = nil
    x.marked = false
}

// cascadingCut performs cascading cut operation
func (fh *FibonacciHeap) cascadingCut(y *Node) {
    z := y.parent
    if z != nil {
        if !y.marked {
            y.marked = true
        } else {
            fh.cut(y, z)
            fh.cascadingCut(z)
        }
    }
}

// PrintHeap prints the heap structure (for debugging)
func (fh *FibonacciHeap) PrintHeap() {
    if fh.minNode == nil {
        fmt.Println("Empty heap")
        return
    }
    
    fmt.Print("Heap: ")
    current := fh.minNode
    fmt.Printf("%d ", current.key)
    
    for next := current.right; next != current; next = next.right {
        fmt.Printf("%d ", next.key)
    }
    fmt.Println()
}

// Example usage
func main() {
    // Create a new Fibonacci heap
    fh := NewFibonacciHeap()
    
    fmt.Println("=== Fibonacci Heap Demo ===")
    
    // Insert some elements
    fmt.Println("Inserting elements: 10, 20, 5, 15, 3, 25")
    fh.Insert(10)
    fh.Insert(20)
    fh.Insert(5)
    fh.Insert(15)
    fh.Insert(3)
    fh.Insert(25)
    
    fmt.Printf("Minimum element: %d\n", fh.Minimum())
    fh.PrintHeap()
    
    // Extract minimum
    fmt.Println("\nExtracting minimum elements:")
    for fh.minNode != nil {
        min := fh.ExtractMin()
        fmt.Printf("Extracted: %d, ", min)
        if fh.minNode != nil {
            fmt.Printf("New minimum: %d\n", fh.Minimum())
        } else {
            fmt.Println("Heap is now empty")
        }
    }
    
    // Test decrease key
    fmt.Println("\n=== Testing Decrease Key ===")
    fh = NewFibonacciHeap()
    nodes := make([]*Node, 5)
    for i, key := range []int{10, 20, 5, 15, 3} {
        nodes[i] = fh.Insert(key)
    }
    
    fmt.Printf("Original minimum: %d\n", fh.Minimum())
    fh.DecreaseKey(nodes[1], 1) // Decrease key of node with value 20 to 1
    fmt.Printf("New minimum after decrease: %d\n", fh.Minimum())
}
```

## Key Features of this Fibonacci Heap Implementation:

### **Core Operations:**
- **Insert**: O(1) amortized time
- **ExtractMin**: O(log n) amortized time
- **DecreaseKey**: O(1) amortized time
- **Minimum**: O(1) time

### **Data Structure Components:**
- **Node structure**: Contains key, degree, marked flag, and pointers to parent, child, left, and right nodes
- **Root list**: Circular doubly-linked list of root nodes
- **Consolidation**: Merges trees of same degree to maintain heap property

### **Advantages:**
- Excellent amortized time complexity for priority queue operations
- Efficient for applications with many decrease-key operations
- Useful in graph algorithms like Dijkstra's and Prim's

### **Applications:**
- Dijkstra's shortest path algorithm
- Prim's minimum spanning tree algorithm
- Heap-based algorithms requiring frequent decrease-key operations

This implementation provides a complete working Fibonacci heap with all essential operations for practical use.

