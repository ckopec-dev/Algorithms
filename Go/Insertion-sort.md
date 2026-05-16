# Insertion Sort in Go

Here's an implementation of the insertion sort algorithm in Go:

```go
package main

import "fmt"

// insertionSort sorts an array using the insertion sort algorithm
func insertionSort(arr []int) {
    // Iterate through the array starting from the second element
    for i := 1; i < len(arr); i++ {
        key := arr[i]        // Current element to be inserted
        j := i - 1           // Index of the last element in sorted portion
        
        // Move elements greater than key one position ahead
        for j >= 0 && arr[j] > key {
            arr[j+1] = arr[j]
            j--
        }
        
        // Insert the key at its correct position
        arr[j+1] = key
    }
}

// printArray prints the elements of an array
func printArray(arr []int) {
    for i := 0; i < len(arr); i++ {
        fmt.Print(arr[i], " ")
    }
    fmt.Println()
}

func main() {
    // Example usage
    arr := []int{64, 34, 25, 12, 22, 11, 90}
    
    fmt.Println("Original array:")
    printArray(arr)
    
    insertionSort(arr)
    
    fmt.Println("Sorted array:")
    printArray(arr)
}
```

## Output:
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## How it works:

1. **Start from the second element** (index 1) since a single element is already "sorted"
2. **Compare the current element** with elements in the sorted portion (to its left)
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until the entire array is sorted

## Time Complexity:
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²) - when array is reverse sorted

## Space Complexity:
- O(1) - sorts in place, only uses a constant amount of extra memory

