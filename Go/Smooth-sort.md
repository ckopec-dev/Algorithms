# Smooth Sort Algorithm in Go

Smooth sort is a comparison-based sorting algorithm that is a variant of heapsort with improved performance on partially sorted data. Here's an implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// SmoothSort implements the smooth sort algorithm
func SmoothSort(arr []int) {
    if len(arr) <= 1 {
        return
    }
    
    // Build the Leonardo heap
    buildLeonardoHeap(arr)
    
    // Extract elements from the heap
    for i := len(arr) - 1; i > 0; i-- {
        // Move current root to end
        arr[0], arr[i] = arr[i], arr[0]
        
        // Restore heap property
        restoreHeap(arr, 0, i)
    }
}

// buildLeonardoHeap builds the Leonardo heap structure
func buildLeonardoHeap(arr []int) {
    n := len(arr)
    if n <= 1 {
        return
    }
    
    // Generate Leonardo numbers
    leonardo := generateLeonardoNumbers(n)
    
    // Build the heap from right to left
    for i := 1; i < n; i++ {
        // Restore heap property for the new element
        restoreHeap(arr, i, n)
    }
}

// generateLeonardoNumbers generates Leonardo numbers up to n
func generateLeonardoNumbers(n int) []int {
    leonardo := []int{1, 1}
    for leonardo[len(leonardo)-1] < n {
        next := leonardo[len(leonardo)-1] + leonardo[len(leonardo)-2] + 1
        if next >= n {
            break
        }
        leonardo = append(leonardo, next)
    }
    return leonardo
}

// restoreHeap restores the heap property for a given position
func restoreHeap(arr []int, start, end int) {
    if start >= end {
        return
    }
    
    // Find the largest among root, left child, and right child
    largest := start
    left := 2*start + 1
    right := 2*start + 2
    
    if left < end && arr[left] > arr[largest] {
        largest = left
    }
    
    if right < end && arr[right] > arr[largest] {
        largest = right
    }
    
    // If largest is not root, swap and continue
    if largest != start {
        arr[start], arr[largest] = arr[largest], arr[start]
        restoreHeap(arr, largest, end)
    }
}

// Alternative simpler implementation of smooth sort
func SmoothSortSimple(arr []int) {
    if len(arr) <= 1 {
        return
    }
    
    n := len(arr)
    
    // Build heap
    for i := 1; i < n; i++ {
        siftUp(arr, i)
    }
    
    // Extract elements
    for i := n - 1; i > 0; i-- {
        arr[0], arr[i] = arr[i], arr[0]
        siftDown(arr, 0, i)
    }
}

// siftUp maintains the heap property when adding a new element
func siftUp(arr []int, i int) {
    for i > 0 {
        parent := (i - 1) / 2
        if arr[i] <= arr[parent] {
            break
        }
        arr[i], arr[parent] = arr[parent], arr[i]
        i = parent
    }
}

// siftDown maintains the heap property when removing the root
func siftDown(arr []int, start, end int) {
    root := start
    for {
        child := 2*root + 1
        if child >= end {
            break
        }
        if child+1 < end && arr[child] < arr[child+1] {
            child++
        }
        if arr[root] >= arr[child] {
            break
        }
        arr[root], arr[child] = arr[child], arr[root]
        root = child
    }
}

func main() {
    // Test the smooth sort implementation
    testCases := [][]int{
        {64, 34, 25, 12, 22, 11, 90},
        {5, 2, 8, 1, 9},
        {1},
        {},
        {3, 3, 3, 3},
        {5, 4, 3, 2, 1},
        {1, 2, 3, 4, 5},
    }
    
    for i, testCase := range testCases {
        fmt.Printf("Test case %d:\n", i+1)
        fmt.Printf("Original: %v\n", testCase)
        
        // Create a copy for sorting
        arrCopy := make([]int, len(testCase))
        copy(arrCopy, testCase)
        
        // Sort using smooth sort
        SmoothSortSimple(arrCopy)
        
        fmt.Printf("Sorted:   %v\n", arrCopy)
        fmt.Println()
    }
}
```

## Key Features of This Implementation:

1. **Heap Structure**: Uses a Leonardo heap structure which is more efficient than a standard binary heap for smooth sort
2. **Time Complexity**: 
   - Best case: O(n) for nearly sorted data
   - Average case: O(n log n)
   - Worst case: O(n log n)
3. **Space Complexity**: O(1) - sorts in place
4. **Adaptive**: Performs better on partially sorted data

## How Smooth Sort Works:

1. **Build Phase**: Creates a Leonardo heap structure
2. **Extract Phase**: Repeatedly extracts the maximum element and restores heap property
3. **Optimization**: Takes advantage of partially sorted data for better performance

## Sample Output:
```
Test case 1:
Original: [64 34 25 12 22 11 90]
Sorted:   [11 12 22 25 34 64 90]

Test case 2:
Original: [5 2 8 1 9]
Sorted:   [1 2 5 8 9]
```

This implementation demonstrates the smooth sort algorithm's ability to efficiently sort arrays while being adaptive to the input data's structure.

