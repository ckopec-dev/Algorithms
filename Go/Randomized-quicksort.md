# Randomized Quicksort in Go

Here's an implementation of the Randomized Quicksort algorithm in Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// RandomizedQuicksort sorts the array using randomized quicksort
func RandomizedQuicksort(arr []int, low, high int) {
    if low < high {
        // Partition the array and get pivot index
        pivotIndex := RandomizedPartition(arr, low, high)
        
        // Recursively sort elements before and after partition
        RandomizedQuicksort(arr, low, pivotIndex-1)
        RandomizedQuicksort(arr, pivotIndex+1, high)
    }
}

// RandomizedPartition partitions the array using a random pivot
func RandomizedPartition(arr []int, low, high int) int {
    // Generate a random index between low and high
    randomIndex := low + rand.Intn(high-low+1)
    
    // Swap the random element with the last element
    arr[randomIndex], arr[high] = arr[high], arr[randomIndex]
    
    // Use the standard partition function
    return Partition(arr, low, high)
}

// Partition partitions the array around a pivot element
func Partition(arr []int, low, high int) int {
    // Choose the rightmost element as pivot
    pivot := arr[high]
    
    // Index of smaller element (indicates right position of pivot)
    i := low - 1
    
    for j := low; j < high; j++ {
        // If current element is smaller than or equal to pivot
        if arr[j] <= pivot {
            i++
            arr[i], arr[j] = arr[j], arr[i]
        }
    }
    
    // Place pivot in its correct position
    arr[i+1], arr[high] = arr[high], arr[i+1]
    return i + 1
}

// Helper function to print array
func PrintArray(arr []int) {
    fmt.Println(arr)
}

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())
    
    // Example array to sort
    arr := []int{10, 7, 8, 9, 1, 5, 3, 6, 2, 4}
    
    fmt.Println("Original array:")
    PrintArray(arr)
    
    // Sort the array
    RandomizedQuicksort(arr, 0, len(arr)-1)
    
    fmt.Println("Sorted array:")
    PrintArray(arr)
    
    // Test with another example
    fmt.Println("\n--- Another example ---")
    arr2 := []int{64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
    fmt.Println("Original array:")
    PrintArray(arr2)
    
    RandomizedQuicksort(arr2, 0, len(arr2)-1)
    
    fmt.Println("Sorted array:")
    PrintArray(arr2)
}
```

## How it works:

1. **RandomizedPartition**: Instead of always choosing the last element as pivot, it randomly selects an element and swaps it with the last element before partitioning.

2. **Partition**: Standard partitioning function that places the pivot element in its correct position and arranges smaller elements to the left and larger elements to the right.

3. **RandomizedQuicksort**: Recursively sorts the subarrays on both sides of the pivot.

## Key Features:

- **Randomization**: Uses `rand.Intn()` to select a random pivot, which helps avoid worst-case performance on sorted or nearly sorted arrays
- **In-place sorting**: Sorts the array without requiring additional memory proportional to input size
- **Time Complexity**: 
  - Average case: O(n log n)
  - Worst case: O(n²) (very unlikely with randomization)
  - Best case: O(n log n)

## Sample Output:
```
Original array:
[10 7 8 9 1 5 3 6 2 4]
Sorted array:
[1 2 3 4 5 6 7 8 9 10]

--- Another example ---
Original array:
[64 34 25 12 22 11 90 88 76 50 42]
Sorted array:
[11 12 22 25 34 42 50 64 76 88 90]
```

