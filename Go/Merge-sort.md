# Merge Sort Algorithm in Go

Here's a complete implementation of the merge sort algorithm in Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// mergeSort recursively sorts an array using merge sort algorithm
func mergeSort(arr []int) []int {
    // Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1 {
        return arr
    }
    
    // Divide the array into two halves
    mid := len(arr) / 2
    left := arr[:mid]
    right := arr[mid:]
    
    // Recursively sort both halves
    left = mergeSort(left)
    right = mergeSort(right)
    
    // Merge the sorted halves
    return merge(left, right)
}

// merge combines two sorted arrays into one sorted array
func merge(left, right []int) []int {
    result := make([]int, 0, len(left)+len(right))
    i, j := 0, 0
    
    // Compare elements from both arrays and merge in sorted order
    for i < len(left) && j < len(right) {
        if left[i] <= right[j] {
            result = append(result, left[i])
            i++
        } else {
            result = append(result, right[j])
            j++
        }
    }
    
    // Add remaining elements from left array (if any)
    for i < len(left) {
        result = append(result, left[i])
        i++
    }
    
    // Add remaining elements from right array (if any)
    for j < len(right) {
        result = append(result, right[j])
        j++
    }
    
    return result
}

func main() {
    // Generate a random array for testing
    rand.Seed(time.Now().UnixNano())
    arr := make([]int, 10)
    for i := range arr {
        arr[i] = rand.Intn(100)
    }
    
    fmt.Println("Original array:", arr)
    
    // Sort the array using merge sort
    sortedArr := mergeSort(arr)
    fmt.Println("Sorted array:  ", sortedArr)
    
    // Test with a known example
    testArr := []int{64, 34, 25, 12, 22, 11, 90}
    fmt.Println("\nTest array:", testArr)
    fmt.Println("Sorted array:", mergeSort(testArr))
}
```

## Output Example:
```
Original array: [45 23 67 12 89 34 56 78 90 21]
Sorted array:   [12 21 23 34 45 56 67 78 89 90]

Test array: [64 34 25 12 22 11 90]
Sorted array: [11 12 22 25 34 64 90]
```

## How it works:

1. **Divide**: Split the array into two halves recursively until each subarray has 1 or 0 elements
2. **Conquer**: Each single element is considered sorted
3. **Combine**: Merge the sorted subarrays back together in sorted order

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(n) - requires additional space for the temporary arrays during merging

The merge sort algorithm is stable and has consistent performance regardless of the input data distribution.

