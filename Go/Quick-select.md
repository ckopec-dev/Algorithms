# Quick Select Algorithm in Go

Quick Select is an efficient algorithm to find the kth smallest element in an unordered list. It's based on the quicksort algorithm but only recurses into one side of the partition.

## Implementation

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// QuickSelect finds the kth smallest element in the array
func quickSelect(arr []int, left, right, k int) int {
    if left == right {
        return arr[left]
    }
    
    // Partition the array and get the pivot index
    pivotIndex := partition(arr, left, right)
    
    if k == pivotIndex {
        return arr[k]
    } else if k < pivotIndex {
        // Search in the left subarray
        return quickSelect(arr, left, pivotIndex-1, k)
    } else {
        // Search in the right subarray
        return quickSelect(arr, pivotIndex+1, right, k)
    }
}

// partition partitions the array around a pivot element
func partition(arr []int, left, right int) int {
    // Choose random pivot to avoid worst-case performance
    rand.Seed(time.Now().UnixNano())
    pivotIndex := left + rand.Intn(right-left+1)
    
    // Move pivot to the end
    arr[pivotIndex], arr[right] = arr[right], arr[pivotIndex]
    
    pivot := arr[right]
    i := left
    
    // Partition the array
    for j := left; j < right; j++ {
        if arr[j] <= pivot {
            arr[i], arr[j] = arr[j], arr[i]
            i++
        }
    }
    
    // Move pivot to its correct position
    arr[i], arr[right] = arr[right], arr[i]
    return i
}

// Helper function to find kth smallest element (0-indexed)
func findKthSmallest(arr []int, k int) int {
    if k < 0 || k >= len(arr) {
        panic("k is out of bounds")
    }
    return quickSelect(arr, 0, len(arr)-1, k)
}

func main() {
    // Example usage
    arr := []int{3, 2, 1, 5, 6, 4}
    fmt.Printf("Original array: %v\n", arr)
    
    // Find 2nd smallest element (index 1)
    k := 1
    result := findKthSmallest(arr, k)
    fmt.Printf("The %dth smallest element is: %d\n", k+1, result)
    
    // Test with different positions
    fmt.Println("\nTesting different positions:")
    for i := 0; i < len(arr); i++ {
        result := findKthSmallest(arr, i)
        fmt.Printf("Element at position %d (0-indexed): %d\n", i, result)
    }
    
    // Example with duplicate elements
    fmt.Println("\nTesting with duplicates:")
    arr2 := []int{7, 10, 4, 3, 20, 15, 4, 10}
    fmt.Printf("Array: %v\n", arr2)
    fmt.Printf("3rd smallest element: %d\n", findKthSmallest(arr2, 2))
    fmt.Printf("5th smallest element: %d\n", findKthSmallest(arr2, 4))
}
```

## Output

```
Original array: [3 2 1 5 6 4]
The 2th smallest element is: 2

Testing different positions:
Element at position 0 (0-indexed): 1
Element at position 1 (0-indexed): 2
Element at position 2 (0-indexed): 3
Element at position 3 (0-indexed): 4
Element at position 4 (0-indexed): 5
Element at position 5 (0-indexed): 6

Testing with duplicates:
Array: [7 10 4 3 20 15 4 10]
3rd smallest element: 4
5th smallest element: 10
```

## Key Features

1. **Time Complexity**: 
   - Average case: O(n)
   - Worst case: O(n²) - but randomized pivot selection makes this rare
   - Best case: O(n)

2. **Space Complexity**: O(log n) due to recursion stack

3. **Algorithm Steps**:
   - Choose a random pivot element
   - Partition the array around the pivot
   - Recursively search in the appropriate subarray
   - Continue until the pivot is at the desired position

4. **Advantages**:
   - More efficient than sorting the entire array
   - In-place algorithm (minimal extra space)
   - Works well with random data

This implementation uses random pivot selection to avoid worst-case performance on already sorted arrays.

