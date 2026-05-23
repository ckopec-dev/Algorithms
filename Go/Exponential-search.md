# Exponential Search Algorithm in Go

Exponential search is a searching algorithm that finds the range where the target element exists, then performs binary search within that range.

## Implementation

```go
package main

import (
    "fmt"
    "math"
)

// ExponentialSearch searches for a target value in a sorted array
// Returns the index of the target if found, otherwise returns -1
func exponentialSearch(arr []int, target int) int {
    // If target is at the first position
    if arr[0] == target {
        return 0
    }
    
    // Find range for binary search by repeated doubling
    index := 1
    for index < len(arr) && arr[index] <= target {
        index *= 2
    }
    
    // Perform binary search in the found range
    return binarySearch(arr, target, index/2, int(math.Min(float64(index), float64(len(arr)-1))))
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
    
    return -1
}

func main() {
    // Example usage
    arr := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}
    
    fmt.Println("Array:", arr)
    fmt.Println()
    
    // Test cases
    testCases := []int{1, 5, 10, 15, 20, 0}
    
    for _, target := range testCases {
        result := exponentialSearch(arr, target)
        if result != -1 {
            fmt.Printf("Target %d found at index %d\n", target, result)
        } else {
            fmt.Printf("Target %d not found in array\n", target)
        }
    }
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
Target 0 not found in array
```

## How it works:

1. **First Step**: Check if the first element is the target
2. **Range Finding**: Keep doubling the index until we find an element greater than or equal to the target
3. **Binary Search**: Perform binary search in the range [prevIndex, currentIdx]

## Time Complexity:
- **Best Case**: O(1) - when target is at index 0
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Key Advantages:
- Works well for unbounded or infinite arrays
- More efficient than linear search for large sorted arrays
- No need to know the array size beforehand
- Better than binary search when the target is closer to the beginning

