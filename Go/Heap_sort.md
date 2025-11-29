# Heap Sort Algorithm in Go

Here's an implementation of the heap sort algorithm in Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// heapify converts a subtree rooted at index i into a max heap
func heapify(arr []int, n, i int) {
    largest := i        // Initialize largest as root
    left := 2*i + 1     // Left child
    right := 2*i + 2    // Right child
    
    // If left child exists and is greater than root
    if left < n && arr[left] > arr[largest] {
        largest = left
    }
    
    // If right child exists and is greater than largest so far
    if right < n && arr[right] > arr[largest] {
        largest = right
    }
    
    // If largest is not root
    if largest != i {
        arr[i], arr[largest] = arr[largest], arr[i]
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest)
    }
}

// heapSort sorts an array using heap sort algorithm
func heapSort(arr []int) {
    n := len(arr)
    
    // Build max heap
    for i := n/2 - 1; i >= 0; i-- {
        heapify(arr, n, i)
    }
    
    // Extract elements from heap one by one
    for i := n - 1; i > 0; i-- {
        // Move current root to end
        arr[0], arr[i] = arr[i], arr[0]
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0)
    }
}

// Utility function to print array
func printArray(arr []int) {
    fmt.Println(arr)
}

func main() {
    // Generate random array for testing
    rand.Seed(time.Now().UnixNano())
    arr := make([]int, 10)
    for i := range arr {
        arr[i] = rand.Intn(100)
    }
    
    fmt.Println("Original array:")
    printArray(arr)
    
    heapSort(arr)
    
    fmt.Println("Sorted array:")
    printArray(arr)
    
    // Test with a known array
    testArr := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Println("\nTest array:")
    printArray(testArr)
    
    heapSort(testArr)
    
    fmt.Println("Sorted test array:")
    printArray(testArr)
}
```

## How it works:

1. **Build Max Heap**: First, we build a max heap from the input array by calling `heapify` on all non-leaf nodes starting from the last non-leaf node.

2. **Extract Elements**: Repeatedly extract the maximum element (root of the heap) and place it at the end of the array, then restore the heap property for the remaining elements.

3. **Heapify**: The `heapify` function ensures that the subtree rooted at index `i` satisfies the max heap property.

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

## Sample Output:
```
Original array:
[45 23 67 12 89 34 56 78 90 21]
Sorted array:
[12 21 23 34 45 56 67 78 89 90]

Test array:
[64 34 25 12 22 11 90]
Sorted test array:
[11 12 22 25 34 64 90]
```

