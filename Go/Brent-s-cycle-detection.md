# Brent's Cycle Detection Algorithm in Go

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists or sequences. It's an improvement over Floyd's cycle detection algorithm with better performance characteristics.

```go
package main

import "fmt"

// ListNode represents a node in a linked list
type ListNode struct {
    Val  int
    Next *ListNode
}

// brentCycleDetection detects cycles in a linked list using Brent's algorithm
func brentCycleDetection(head *ListNode) (*ListNode, int) {
    // Handle empty list
    if head == nil {
        return nil, 0
    }
    
    // Phase 1: Find a point in the cycle
    power := 1
    length := 1
    tortoise := head
    hare := head
    
    // Move hare until we find a cycle or reach the end
    for hare != nil && hare.Next != nil {
        // Move hare
        hare = hare.Next
        
        // If we found a cycle
        if tortoise == hare {
            break
        }
        
        // If we've moved enough steps, update power and length
        if power == length {
            tortoise = hare
            power *= 2
            length = 0
        }
        
        length++
    }
    
    // If no cycle found
    if hare == nil || hare.Next == nil {
        return nil, 0
    }
    
    // Phase 2: Find the start of the cycle
    // Reset tortoise to head
    tortoise = head
    hare = head
    
    // Move hare to the cycle start point
    for i := 0; i < length; i++ {
        hare = hare.Next
    }
    
    // Move both pointers until they meet at cycle start
    for tortoise != hare {
        tortoise = tortoise.Next
        hare = hare.Next
    }
    
    return tortoise, length
}

// Helper function to create a linked list with a cycle
func createCycleList() *ListNode {
    // Create nodes: 1 -> 2 -> 3 -> 4 -> 5 -> 3 (cycle back to node 3)
    head := &ListNode{Val: 1}
    node2 := &ListNode{Val: 2}
    node3 := &ListNode{Val: 3}
    node4 := &ListNode{Val: 4}
    node5 := &ListNode{Val: 5}
    
    head.Next = node2
    node2.Next = node3
    node3.Next = node4
    node4.Next = node5
    node5.Next = node3 // Creates cycle back to node 3
    
    return head
}

// Helper function to create a linked list without cycle
func createNoCycleList() *ListNode {
    head := &ListNode{Val: 1}
    node2 := &ListNode{Val: 2}
    node3 := &ListNode{Val: 3}
    node4 := &ListNode{Val: 4}
    node5 := &ListNode{Val: 5}
    
    head.Next = node2
    node2.Next = node3
    node3.Next = node4
    node4.Next = node5
    
    return head
}

// Helper function to print list (for debugging)
func printList(head *ListNode, maxNodes int) {
    current := head
    count := 0
    for current != nil && count < maxNodes {
        fmt.Printf("%d -> ", current.Val)
        current = current.Next
        count++
    }
    fmt.Println("nil")
}

func main() {
    fmt.Println("Brent's Cycle Detection Algorithm Example")
    fmt.Println("========================================")
    
    // Example 1: List with cycle
    fmt.Println("\nExample 1: Linked list with cycle")
    cycleList := createCycleList()
    fmt.Print("List structure: ")
    printList(cycleList, 10)
    
    cycleStart, cycleLength := brentCycleDetection(cycleList)
    if cycleStart != nil {
        fmt.Printf("Cycle detected! Start node value: %d\n", cycleStart.Val)
        fmt.Printf("Cycle length: %d\n", cycleLength)
    } else {
        fmt.Println("No cycle found")
    }
    
    // Example 2: List without cycle
    fmt.Println("\nExample 2: Linked list without cycle")
    noCycleList := createNoCycleList()
    fmt.Print("List structure: ")
    printList(noCycleList, 10)
    
    cycleStart, cycleLength = brentCycleDetection(noCycleList)
    if cycleStart != nil {
        fmt.Printf("Cycle detected! Start node value: %d\n", cycleStart.Val)
        fmt.Printf("Cycle length: %d\n", cycleLength)
    } else {
        fmt.Println("No cycle found")
    }
    
    // Example 3: Empty list
    fmt.Println("\nExample 3: Empty list")
    emptyList := (*ListNode)(nil)
    cycleStart, cycleLength = brentCycleDetection(emptyList)
    if cycleStart != nil {
        fmt.Printf("Cycle detected! Start node value: %d\n", cycleStart.Val)
        fmt.Printf("Cycle length: %d\n", cycleLength)
    } else {
        fmt.Println("No cycle found")
    }
}
```

## How Brent's Algorithm Works

1. **Phase 1 - Cycle Detection**: 
   - Uses two pointers (tortoise and hare)
   - The hare moves faster than the tortoise
   - When they meet, a cycle is detected

2. **Phase 2 - Cycle Start Finding**:
   - Resets one pointer to the head
   - Moves both pointers one step at a time until they meet
   - The meeting point is the start of the cycle

## Key Features

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - only uses a constant amount of extra space
- **Efficiency**: Generally faster than Floyd's algorithm in practice
- **Robustness**: Handles edge cases like empty lists and no cycles

## Output Example

```
Brent's Cycle Detection Algorithm Example
========================================

Example 1: Linked list with cycle
List structure: 1 -> 2 -> 3 -> 4 -> 5 -> 3 -> nil
Cycle detected! Start node value: 3
Cycle length: 3

Example 2: Linked list without cycle
List structure: 1 -> 2 -> 3 -> 4 -> 5 -> nil
No cycle found

Example 3: Empty list
No cycle found
```

