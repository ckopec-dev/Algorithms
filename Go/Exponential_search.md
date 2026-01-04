# Exponential Search Algorithm in Go

Exponential search is a searching algorithm that works by first finding the range where the target element might be located, then performing binary search within that range.

## Implementation

```go
package main

import (
    "fmt"
    "math"
)

// ExponentialSearch searches for target in a sorted array using exponential search
func ExponentialSearch(arr []int, target int) int {
    // If target is at the first position
    if arr[0] == target {
        return 0
    }
    
    // Find range for binary search by repeated doubling
    n := len(arr)
    i := 1
    for i < n && arr[i] <= target {
        i *= 2
    }
    
    // Perform binary search in the range [i/2, min(i, n-1)]
    return binarySearch(arr, target, i/2, int(math.Min(float64(i), float64(n-1))))
}

// BinarySearch performs binary search in the given range
func binarySearch(arr []int, target, left, right int) int {
    for left <= right {
        mid := left + (right-left)/2
        
        if arr[mid] == target {
            return mid
        }
        
        if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    
    return -1 // Target not found
}

func main() {
    // Example usage
    arr := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}
    
    fmt.Println("Array:", arr)
    fmt.Println()
    
    // Test cases
    targets := []int{1, 5, 10, 15, 20}
    
    for _, target := range targets {
        result := ExponentialSearch(arr, target)
        if result != -1 {
            fmt.Printf("Target %d found at index %d\n", target, result)
        } else {
            fmt.Printf("Target %d not found in array\n", target)
        }
    }
    
    // Example with unsorted array (will not work correctly)
    fmt.Println("\n--- Example with unsorted array ---")
    unsortedArr := []int{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5}
    fmt.Println("Unsorted array:", unsortedArr)
    
    // Sort first for proper search
    fmt.Println("Sorted array:", sortedArray(unsortedArr))
}

// Helper function to sort array for demonstration
func sortedArray(arr []int) []int {
    // Simple bubble sort for demonstration
    sorted := make([]int, len(arr))
    copy(sorted, arr)
    
    for i := 0; i < len(sorted)-1; i++ {
        for j := 0; j < len(sorted)-1-i; j++ {
            if sorted[j] > sorted[j+1] {
                sorted[j], sorted[j+1] = sorted[j+1], sorted[j]
            }
        }
    }
    
    return sorted
}
```

## Output

```
Array: [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]

Target 1 found at index 0
Target 5 found at index 4
Target 10 found at index 9
Target 15 found at index 14
Target 20 not found in array

--- Example with unsorted array ---
Unsorted array: [3 1 4 1 5 9 2 6 5 3 5]
Sorted array: [1 1 2 3 3 4 5 5 5 6 9]
```

## How it Works

1. **First Step**: Check if the target is at the first position (index 0)
2. **Range Finding**: Keep doubling the index until we find an element greater than or equal to the target
3. **Binary Search**: Perform binary search in the range [i/2, min(i, n-1)]

## Time and Space Complexity

- **Time Complexity**: O(log n) - where n is the position of the target element
- **Space Complexity**: O(1) - only using a constant amount of extra space

## Key Advantages

- Works well when the target element is closer to the beginning of the array
- More efficient than binary search when the target is at a small index
- No need to know the array size beforehand
- Handles unbounded arrays well

## When to Use

Exponential search is particularly useful when:
- The array size is unknown or unbounded
- The target element is likely to be near the beginning
- You want to avoid the overhead of binary search when the target is at a small index

