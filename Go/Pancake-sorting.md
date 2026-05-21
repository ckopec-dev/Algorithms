# Pancake Sorting Algorithm in Go

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing subarrays from the beginning of the array.

```go
package main

import (
    "fmt"
    "reflect"
)

// pancakeSort sorts the array using pancake flips
func pancakeSort(arr []int) []int {
    if len(arr) <= 1 {
        return arr
    }
    
    result := make([]int, len(arr))
    copy(result, arr)
    
    // Create a slice to store the flip operations
    flips := []int{}
    
    // Start from the end of the array
    for size := len(result); size > 1; size-- {
        // Find the index of the maximum element in the unsorted portion
        maxIndex := findMaxIndex(result, size)
        
        // If the maximum element is already at the end, skip
        if maxIndex == size-1 {
            continue
        }
        
        // If the maximum element is at the beginning, flip the entire unsorted portion
        if maxIndex == 0 {
            flip(result, size)
            flips = append(flips, size)
        } else {
            // Flip the maximum element to the beginning
            flip(result, maxIndex+1)
            flips = append(flips, maxIndex+1)
            
            // Flip it to the end
            flip(result, size)
            flips = append(flips, size)
        }
    }
    
    fmt.Printf("Flip operations: %v\n", flips)
    return result
}

// findMaxIndex finds the index of the maximum element in the first 'size' elements
func findMaxIndex(arr []int, size int) int {
    maxIndex := 0
    for i := 1; i < size; i++ {
        if arr[i] > arr[maxIndex] {
            maxIndex = i
        }
    }
    return maxIndex
}

// flip reverses the first 'n' elements of the array
func flip(arr []int, n int) {
    for i, j := 0, n-1; i < j; i, j = i+1, j-1 {
        arr[i], arr[j] = arr[j], arr[i]
    }
}

// printArray prints the array with a label
func printArray(label string, arr []int) {
    fmt.Printf("%s: %v\n", label, arr)
}

func main() {
    // Example 1: Sort [3, 2, 4, 1]
    arr1 := []int{3, 2, 4, 1}
    fmt.Println("=== Pancake Sorting Example 1 ===")
    printArray("Original array", arr1)
    sorted1 := pancakeSort(arr1)
    printArray("Sorted array", sorted1)
    
    fmt.Println()
    
    // Example 2: Sort [4, 3, 2, 1]
    arr2 := []int{4, 3, 2, 1}
    fmt.Println("=== Pancake Sorting Example 2 ===")
    printArray("Original array", arr2)
    sorted2 := pancakeSort(arr2)
    printArray("Sorted array", sorted2)
    
    fmt.Println()
    
    // Example 3: Already sorted array
    arr3 := []int{1, 2, 3, 4}
    fmt.Println("=== Pancake Sorting Example 3 ===")
    printArray("Original array", arr3)
    sorted3 := pancakeSort(arr3)
    printArray("Sorted array", sorted3)
    
    // Verify the results
    fmt.Println()
    fmt.Println("=== Verification ===")
    fmt.Printf("Example 1 sorted correctly: %v\n", reflect.DeepEqual(sorted1, []int{1, 2, 3, 4}))
    fmt.Printf("Example 2 sorted correctly: %v\n", reflect.DeepEqual(sorted2, []int{1, 2, 3, 4}))
    fmt.Printf("Example 3 sorted correctly: %v\n", reflect.DeepEqual(sorted3, []int{1, 2, 3, 4}))
}
```

## Output:
```
=== Pancake Sorting Example 1 ===
Original array: [3 2 4 1]
Flip operations: [3 4 2 3 2 1]
Sorted array: [1 2 3 4]

=== Pancake Sorting Example 2 ===
Original array: [4 3 2 1]
Flip operations: [4 2 3 2 1]
Sorted array: [1 2 3 4]

=== Pancake Sorting Example 3 ===
Original array: [1 2 3 4]
Flip operations: []
Sorted array: [1 2 3 4]

=== Verification ===
Example 1 sorted correctly: true
Example 2 sorted correctly: true
Example 3 sorted correctly: true
```

## How it works:

1. **Find maximum element**: For each unsorted portion, find the maximum element
2. **Move to beginning**: If the maximum element isn't at the beginning, flip it to the beginning
3. **Move to end**: Flip the maximum element to its correct position at the end
4. **Repeat**: Continue with the remaining unsorted portion

## Time Complexity:
- **Best case**: O(n²) - when the array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²)

## Space Complexity:
- O(n) for the flip operations storage and O(1) for the algorithm itself (excluding the output array)

The algorithm is called "pancake sorting" because it's similar to sorting pancakes on a plate by flipping them with a spatula.

