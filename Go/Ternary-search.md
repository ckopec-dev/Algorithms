# Ternary Search Algorithm in Go

```go
package main

import (
    "fmt"
    "math"
)

// ternarySearch performs ternary search on a sorted array
// Returns the index of the target element, or -1 if not found
func ternarySearch(arr []int, target int) int {
    left := 0
    right := len(arr) - 1
    
    for left <= right {
        // Calculate two mid points to divide the array into three parts
        mid1 := left + int(math.Floor(float64((right-left)/3)))
        mid2 := right - int(math.Floor(float64((right-left)/3)))
        
        // If target is at either mid point
        if arr[mid1] == target {
            return mid1
        }
        if arr[mid2] == target {
            return mid2
        }
        
        // Determine which segment to search next
        if target < arr[mid1] {
            // Target is in the left segment
            right = mid1 - 1
        } else if target > arr[mid2] {
            // Target is in the right segment
            left = mid2 + 1
        } else {
            // Target is in the middle segment
            left = mid1 + 1
            right = mid2 - 1
        }
    }
    
    // Target not found
    return -1
}

// iterativeTernarySearch is an alternative implementation using iteration
func iterativeTernarySearch(arr []int, target int) int {
    left := 0
    right := len(arr) - 1
    
    for left <= right {
        // Calculate the two division points
        mid1 := left + (right-left)/3
        mid2 := right - (right-left)/3
        
        if arr[mid1] == target {
            return mid1
        }
        if arr[mid2] == target {
            return mid2
        }
        
        if target < arr[mid1] {
            right = mid1 - 1
        } else if target > arr[mid2] {
            left = mid2 + 1
        } else {
            left = mid1 + 1
            right = mid2 - 1
        }
    }
    
    return -1
}

func main() {
    // Example sorted array
    arr := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}
    
    fmt.Println("Array:", arr)
    fmt.Println()
    
    // Test cases
    testCases := []int{1, 5, 8, 15, 20, 0}
    
    for _, target := range testCases {
        result1 := ternarySearch(arr, target)
        result2 := iterativeTernarySearch(arr, target)
        
        fmt.Printf("Searching for %d:\n", target)
        fmt.Printf("  Ternary Search: index %d\n", result1)
        fmt.Printf("  Iterative Search: index %d\n", result2)
        
        if result1 != -1 {
            fmt.Printf("  Found at index %d with value %d\n", result1, arr[result1])
        } else {
            fmt.Printf("  Not found in array\n")
        }
        fmt.Println()
    }
    
    // Demonstrate the algorithm steps for a specific example
    fmt.Println("Step-by-step example for searching 7:")
    fmt.Println("Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]")
    fmt.Println("Left = 0, Right = 14")
    
    left, right := 0, 14
    mid1 := left + (right-left)/3
    mid2 := right - (right-left)/3
    
    fmt.Printf("Mid1 = %d, Mid2 = %d\n", mid1, mid2)
    fmt.Printf("arr[mid1] = %d, arr[mid2] = %d\n", arr[mid1], arr[mid2])
    
    if arr[mid1] < 7 && arr[mid2] > 7 {
        fmt.Println("7 is between mid1 and mid2, search middle segment")
    }
}
```

## How Ternary Search Works

The ternary search algorithm divides the sorted array into three parts and determines which segment contains the target element:

1. **Divide**: Calculate two mid points (`mid1` and `mid2`) that split the array into three segments
2. **Compare**: Check if the target equals either mid point
3. **Conquer**: Recursively search the appropriate segment based on comparisons
4. **Repeat**: Continue until the element is found or the search space is exhausted

## Time and Space Complexity

- **Time Complexity**: O(log₃ n) - Each step reduces the search space to 1/3 of its previous size
- **Space Complexity**: O(1) for iterative implementation, O(log₃ n) for recursive implementation

## Key Differences from Binary Search

- **Binary Search**: Divides array into 2 parts
- **Ternary Search**: Divides array into 3 parts
- **Advantage**: Slightly fewer comparisons in theory
- **Disadvantage**: More complex implementation and potentially more overhead

The ternary search is particularly useful when the cost of comparison is high, as it reduces the number of comparisons needed compared to binary search.

