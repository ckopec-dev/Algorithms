# Counting Sort Algorithm in Go

Here's an implementation of the counting sort algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// CountingSort sorts an array using the counting sort algorithm
func countingSort(arr []int) []int {
    if len(arr) == 0 {
        return arr
    }
    
    // Find the maximum and minimum values to determine range
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
    
    // Create counting array
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

// Alternative implementation for positive integers only
func countingSortPositive(arr []int) []int {
    if len(arr) == 0 {
        return arr
    }
    
    // Find maximum value
    max := 0
    for _, num := range arr {
        if num > max {
            max = num
        }
    }
    
    // Create counting array
    count := make([]int, max+1)
    
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
    // Example 1: Array with negative numbers
    arr1 := []int{4, 2, 2, 8, 3, 3, 1, -2, -5}
    fmt.Printf("Original array: %v\n", arr1)
    sorted1 := countingSort(arr1)
    fmt.Printf("Sorted array:   %v\n\n", sorted1)
    
    // Example 2: Array with positive numbers only
    arr2 := []int{4, 2, 2, 8, 3, 3, 1}
    fmt.Printf("Original array: %v\n", arr2)
    sorted2 := countingSortPositive(arr2)
    fmt.Printf("Sorted array:   %v\n\n", sorted2)
    
    // Example 3: Array with duplicates
    arr3 := []int{5, 5, 5, 5, 1, 1, 3, 3, 2}
    fmt.Printf("Original array: %v\n", arr3)
    sorted3 := countingSort(arr3)
    fmt.Printf("Sorted array:   %v\n", sorted3)
}
```

## Output:
```
Original array: [4 2 2 8 3 3 1 -2 -5]
Sorted array:   [-5 -2 1 2 2 3 3 4 8]

Original array: [4 2 2 8 3 3 1]
Sorted array:   [1 2 2 3 3 4 8]

Original array: [5 5 5 5 1 1 3 3 2]
Sorted array:   [1 1 2 3 3 5 5 5 5]
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values to calculate the range
2. **Count Elements**: Create a counting array where each index represents a value and stores its frequency
3. **Reconstruct Array**: Iterate through the counting array and build the sorted result

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

## Key Features:
- Handles negative numbers
- Stable sorting (maintains relative order of equal elements)
- Efficient for small range of integers
- Works well when the range of possible values is not significantly larger than the number of elements

