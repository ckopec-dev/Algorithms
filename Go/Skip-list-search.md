# Skip List Search Algorithm in Go

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// Node represents a node in the skip list
type Node struct {
    value int
    forward []*Node
}

// SkipList represents the skip list structure
type SkipList struct {
    head *Node
    maxLevel int
    currentLevel int
}

// NewSkipList creates a new skip list with given max level
func NewSkipList(maxLevel int) *SkipList {
    head := &Node{
        value:   0,
        forward: make([]*Node, maxLevel),
    }
    
    return &SkipList{
        head:         head,
        maxLevel:     maxLevel,
        currentLevel: 0,
    }
}

// randomLevel generates a random level for a new node
func (sl *SkipList) randomLevel() int {
    level := 1
    for rand.Float32() < 0.5 && level < sl.maxLevel {
        level++
    }
    return level
}

// Search searches for a value in the skip list
func (sl *SkipList) Search(value int) bool {
    // Start from the highest level
    current := sl.head
    
    // Traverse from top to bottom
    for i := sl.currentLevel - 1; i >= 0; i-- {
        // Move forward while the next node's value is less than target
        for current.forward[i] != nil && current.forward[i].value < value {
            current = current.forward[i]
        }
    }
    
    // Move to the next node (which should be the target or greater)
    current = current.forward[0]
    
    // Check if we found the exact value
    if current != nil && current.value == value {
        return true
    }
    
    return false
}

// Insert inserts a value into the skip list
func (sl *SkipList) Insert(value int) {
    update := make([]*Node, sl.maxLevel)
    current := sl.head
    
    // Find the position where the new node should be inserted
    for i := sl.currentLevel - 1; i >= 0; i-- {
        for current.forward[i] != nil && current.forward[i].value < value {
            current = current.forward[i]
        }
        update[i] = current
    }
    
    current = current.forward[0]
    
    // If value already exists, don't insert
    if current != nil && current.value == value {
        return
    }
    
    // Create new node with random level
    newLevel := sl.randomLevel()
    
    // Update the current level if needed
    if newLevel > sl.currentLevel {
        for i := sl.currentLevel; i < newLevel; i++ {
            update[i] = sl.head
        }
        sl.currentLevel = newLevel
    }
    
    // Create new node
    newNode := &Node{
        value:   value,
        forward: make([]*Node, newLevel),
    }
    
    // Insert the new node
    for i := 0; i < newLevel; i++ {
        newNode.forward[i] = update[i].forward[i]
        update[i].forward[i] = newNode
    }
}

// Print skips list structure (for visualization)
func (sl *SkipList) Print() {
    fmt.Println("Skip List Structure:")
    for i := sl.currentLevel - 1; i >= 0; i-- {
        current := sl.head.forward[i]
        fmt.Printf("Level %d: ", i)
        for current != nil {
            fmt.Printf("%d ", current.value)
            current = current.forward[i]
        }
        fmt.Println()
    }
    fmt.Println()
}

func main() {
    // Initialize random seed
    rand.Seed(time.Now().UnixNano())
    
    // Create a skip list with max level 4
    skipList := NewSkipList(4)
    
    // Insert some values
    values := []int{3, 6, 7, 9, 12, 19, 17, 26, 21, 25}
    fmt.Println("Inserting values:", values)
    
    for _, value := range values {
        skipList.Insert(value)
    }
    
    // Print the skip list structure
    skipList.Print()
    
    // Test search operations
    fmt.Println("Search operations:")
    searchValues := []int{7, 15, 21, 30}
    
    for _, value := range searchValues {
        found := skipList.Search(value)
        if found {
            fmt.Printf("Found %d in skip list\n", value)
        } else {
            fmt.Printf("Did not find %d in skip list\n", value)
        }
    }
    
    // Demonstrate search with a value that exists
    fmt.Println("\nDetailed search for value 19:")
    found := skipList.Search(19)
    fmt.Printf("Search result for 19: %t\n", found)
}
```

## Algorithm Explanation

The `Search` function in the skip list works as follows:

1. **Start from the highest level** - Begin at the topmost level of the skip list
2. **Traverse horizontally** - Move forward through the level until finding a node with a value greater than or equal to the target
3. **Drop down** - Move to the next lower level and repeat the process
4. **Final check** - When reaching level 0, check if the next node contains the target value

## Time Complexity

- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Key Features

- **Efficient search**: Much faster than linear search in sorted arrays
- **Dynamic structure**: Automatically maintains sorted order
- **Probabilistic approach**: Uses randomization to maintain balance
- **Multiple levels**: Higher levels provide faster access to distant elements

The skip list provides a good balance between simplicity and performance, making it an excellent alternative to balanced binary search trees.

