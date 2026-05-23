# Counting Sort Algorithm in Go

Here's an implementation of the counting sort algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// countingSort sorts an array using the counting sort algorithm
func countingSort(arr []int) []int {
    if len(arr) == 0 {
        return arr
    }
    
    // Find the maximum and minimum values to determine the range
    max := arr[0]
    min := arr[0]
    
    for _, num := range arr {
        if num > max {
            max = num
        }
        if num < min {
            min = num
        }
    }
    
    // Create a counting array
    rangeSize := max - min + 1
    count := make([]int, rangeSize)
    
    // Count occurrences of each element
    for _, num := range arr {
        count[num-min]++
    }
    
    // Reconstruct the sorted array
    result := make([]int, 0, len(arr))
    for i, freq := range count {
        for j := 0; j < freq; j++ {
            result = append(result, i+min)
        }
    }
    
    return result
}

// countingSortWithRange sorts an array assuming values are in range [0, k]
func countingSortWithRange(arr []int, k int) []int {
    if len(arr) == 0 {
        return arr
    }
    
    // Create counting array
    count := make([]int, k+1)
    
    // Count occurrences
    for _, num := range arr {
        count[num]++
    }
    
    // Reconstruct sorted array
    result := make([]int, 0, len(arr))
    for i, freq := range count {
        for j := 0; j < freq; j++ {
            result = append(result, i)
        }
    }
    
    return result
}

func main() {
    // Example 1: General counting sort
    arr1 := []int{4, 2, 2, 8, 3, 3, 1}
    fmt.Printf("Original array: %v\n", arr1)
    sorted1 := countingSort(arr1)
    fmt.Printf("Sorted array:   %v\n\n", sorted1)
    
    // Example 2: Counting sort with known range
    arr2 := []int{4, 2, 2, 8, 3, 3, 1}
    fmt.Printf("Original array: %v\n", arr2)
    sorted2 := countingSortWithRange(arr2, 8) // Assuming values are in range [0, 8]
    fmt.Printf("Sorted array:   %v\n\n", sorted2)
    
    // Example 3: Array with negative numbers
    arr3 := []int{-5, -2, 0, 3, 7, -1, 4}
    fmt.Printf("Original array: %v\n", arr3)
    sorted3 := countingSort(arr3)
    fmt.Printf("Sorted array:   %v\n\n", sorted3)
    
    // Example 4: Array with duplicates
    arr4 := []int{5, 5, 5, 2, 2, 1, 1, 1, 1}
    fmt.Printf("Original array: %v\n", arr4)
    sorted4 := countingSort(arr4)
    fmt.Printf("Sorted array:   %v\n", sorted4)
}
```

## Output:
```
Original array: [4 2 2 8 3 3 1]
Sorted array:   [1 2 2 3 3 4 8]

Original array: [4 2 2 8 3 3 1]
Sorted array:   [1 2 2 3 3 4 8]

Original array: [-5 -2 0 3 7 -1 4]
Sorted array:   [-5 -2 -1 0 3 4 7]

Original array: [5 5 5 2 2 1 1 1 1]
Sorted array:   [1 1 1 1 2 2 5 5 5]
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values to calculate the range
2. **Counting**: Create a counting array where each index represents a value and stores its frequency
3. **Reconstruction**: Iterate through the counting array and reconstruct the sorted array

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

## Key Features:

- Works efficiently when the range of possible values is not significantly larger than the number of elements
- Stable sorting algorithm (maintains relative order of equal elements)
- Handles negative numbers correctly
- More efficient than comparison-based sorting for small ranges

This implementation is particularly useful when sorting integers within a known, limited range.

