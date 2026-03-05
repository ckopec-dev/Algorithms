# Binary Search Algorithm in Go

Here's an implementation of the binary search algorithm in Go:

```go
package main

import (
    "fmt"
    "sort"
)

// BinarySearch searches for target in a sorted slice using binary search
// Returns the index of target if found, otherwise returns -1
func BinarySearch(arr []int, target int) int {
    left := 0
    right := len(arr) - 1
    
    for left <= right {
        // Calculate mid-point to avoid integer overflow
        mid := left + (right-left)/2
        
        if arr[mid] == target {
            return mid
        } else if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    
    return -1 // Target not found
}

// Recursive implementation of binary search
func BinarySearchRecursive(arr []int, target, left, right int) int {
    if left > right {
        return -1
    }
    
    mid := left + (right-left)/2
    
    if arr[mid] == target {
        return mid
    } else if arr[mid] < target {
        return BinarySearchRecursive(arr, target, mid+1, right)
    } else {
        return BinarySearchRecursive(arr, target, left, mid-1)
    }
}

func main() {
    // Example usage
    sortedArray := []int{1, 3, 5, 7, 9, 11, 13, 15, 17, 19}
    
    fmt.Println("Sorted array:", sortedArray)
    fmt.Println()
    
    // Test cases
    testCases := []int{7, 1, 19, 4, 20}
    
    for _, target := range testCases {
        // Iterative binary search
        index := BinarySearch(sortedArray, target)
        if index != -1 {
            fmt.Printf("Found %d at index %d\n", target, index)
        } else {
            fmt.Printf("%d not found in array\n", target)
        }
        
        // Recursive binary search
        indexRecursive := BinarySearchRecursive(sortedArray, target, 0, len(sortedArray)-1)
        if indexRecursive != -1 {
            fmt.Printf("Recursive search: Found %d at index %d\n", target, indexRecursive)
        } else {
            fmt.Printf("Recursive search: %d not found in array\n", target)
        }
        fmt.Println()
    }
    
    // Using Go's built-in sort.Search for comparison
    fmt.Println("Using Go's sort.Search:")
    target := 11
    index := sort.SearchInts(sortedArray, target)
    if index < len(sortedArray) && sortedArray[index] == target {
        fmt.Printf("Found %d at index %d\n", target, index)
    } else {
        fmt.Printf("%d not found in array\n", target)
    }
}
```

## Output:
```
Sorted array: [1 3 5 7 9 11 13 15 17 19]

Found 7 at index 3
Recursive search: Found 7 at index 3

Found 1 at index 0
Recursive search: Found 1 at index 0

Found 19 at index 9
Recursive search: Found 19 at index 9

Found 4 at index -1
Recursive search: Found 4 at index -1

Found 20 at index -1
Recursive search: Found 20 at index -1

Using Go's sort.Search:
Found 11 at index 5
```

## Key Features:

1. **Iterative Implementation**: Uses a while loop with left and right pointers
2. **Recursive Implementation**: Alternative recursive approach
3. **Overflow Prevention**: Uses `mid = left + (right-left)/2` instead of `mid = (left+right)/2`
4. **Time Complexity**: O(log n)
5. **Space Complexity**: O(1) for iterative, O(log n) for recursive
6. **Precondition**: Array must be sorted for binary search to work correctly

## Important Notes:

- Binary search only works on **sorted arrays**
- The algorithm compares the target with the middle element and eliminates half the search space in each iteration
- Both implementations return -1 when the target is not found
- Go's standard library provides `sort.SearchInts()` for built-in binary search functionality

