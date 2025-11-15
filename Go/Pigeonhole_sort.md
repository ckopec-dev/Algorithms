# Pigeonhole Sort in Go

Pigeonhole sort is a sorting algorithm that is suitable for sorting arrays of elements that have a small range of possible values. It works by placing elements into "pigeonholes" (buckets) and then collecting them in order.

## Implementation

```go
package main

import (
    "fmt"
    "math"
)

// PigeonholeSort sorts an array using the pigeonhole sort algorithm
func PigeonholeSort(arr []int) {
    if len(arr) <= 1 {
        return
    }
    
    // Find the minimum and maximum values
    min := arr[0]
    max := arr[0]
    
    for _, num := range arr {
        if num < min {
            min = num
        }
        if num > max {
            max = num
        }
    }
    
    // Calculate the range of values
    rangeSize := max - min + 1
    
    // Create pigeonholes (buckets)
    pigeonholes := make([]int, rangeSize)
    
    // Place elements into pigeonholes
    for _, num := range arr {
        pigeonholes[num-min]++
    }
    
    // Collect elements from pigeonholes back to array
    index := 0
    for i, count := range pigeonholes {
        for j := 0; j < count; j++ {
            arr[index] = i + min
            index++
        }
    }
}

// Alternative implementation that returns a new sorted array
func PigeonholeSortNew(arr []int) []int {
    if len(arr) <= 1 {
        return append([]int(nil), arr...)
    }
    
    // Find the minimum and maximum values
    min := arr[0]
    max := arr[0]
    
    for _, num := range arr {
        if num < min {
            min = num
        }
        if num > max {
            max = num
        }
    }
    
    // Calculate the range of values
    rangeSize := max - min + 1
    
    // Create pigeonholes (buckets)
    pigeonholes := make([]int, rangeSize)
    
    // Place elements into pigeonholes
    for _, num := range arr {
        pigeonholes[num-min]++
    }
    
    // Create result array
    result := make([]int, 0, len(arr))
    
    // Collect elements from pigeonholes
    for i, count := range pigeonholes {
        for j := 0; j < count; j++ {
            result = append(result, i+min)
        }
    }
    
    return result
}

func main() {
    // Example 1: Basic usage
    arr1 := []int{8, 3, 2, 7, 4, 6, 1}
    fmt.Println("Original array:", arr1)
    
    PigeonholeSort(arr1)
    fmt.Println("Sorted array:", arr1)
    
    // Example 2: Array with duplicates
    arr2 := []int{5, 2, 8, 2, 9, 1, 5, 5}
    fmt.Println("\nOriginal array:", arr2)
    
    sortedArr2 := PigeonholeSortNew(arr2)
    fmt.Println("Sorted array:", sortedArr2)
    
    // Example 3: Array with negative numbers
    arr3 := []int{-2, 3, -1, 0, 5, -3}
    fmt.Println("\nOriginal array:", arr3)
    
    PigeonholeSort(arr3)
    fmt.Println("Sorted array:", arr3)
    
    // Example 4: Already sorted array
    arr4 := []int{1, 2, 3, 4, 5}
    fmt.Println("\nOriginal array:", arr4)
    
    PigeonholeSort(arr4)
    fmt.Println("Sorted array:", arr4)
}
```

## Output
```
Original array: [8 3 2 7 4 6 1]
Sorted array: [1 2 3 4 6 7 8]

Original array: [5 2 8 2 9 1 5 5]
Sorted array: [1 2 2 5 5 5 8 9]

Original array: [-2 3 -1 0 5 -3]
Sorted array: [-3 -2 -1 0 3 5]

Original array: [1 2 3 4 5]
Sorted array: [1 2 3 4 5]
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the array
2. **Create pigeonholes**: Create an array of size (max - min + 1) to represent pigeonholes
3. **Place elements**: For each element, place it in the corresponding pigeonhole (element - min)
4. **Collect elements**: Go through the pigeonholes and collect elements back to the original array in order

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the pigeonhole array

## When to use:

Pigeonhole sort is most efficient when:
- The range of possible values (k) is not significantly larger than the number of elements (n)
- The elements are integers or can be easily mapped to integers
- Memory usage is not a major concern

The algorithm is particularly useful when you have a small range of values and want to achieve linear time complexity.

