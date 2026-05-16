# Selection Sort Algorithm in Go

Here's an implementation of the selection sort algorithm in Go:

```go
package main

import "fmt"

// selectionSort sorts an array using the selection sort algorithm
func selectionSort(arr []int) {
    n := len(arr)
    
    // Traverse through all array elements
    for i := 0; i < n-1; i++ {
        // Find the minimum element in the remaining unsorted array
        minIndex := i
        for j := i + 1; j < n; j++ {
            if arr[j] < arr[minIndex] {
                minIndex = j
            }
        }
        
        // Swap the found minimum element with the first element
        if minIndex != i {
            arr[i], arr[minIndex] = arr[minIndex], arr[i]
        }
    }
}

func main() {
    // Example usage
    arr := []int{64, 34, 25, 12, 22, 11, 90}
    
    fmt.Println("Original array:", arr)
    
    selectionSort(arr)
    
    fmt.Println("Sorted array:  ", arr)
}
```

## Output:
```
Original array: [64 34 25 12 22 11 90]
Sorted array:   [11 12 22 25 34 64 90]
```

## How it works:

1. **Find the minimum**: In each iteration, find the smallest element in the unsorted portion of the array
2. **Swap**: Place this minimum element at the beginning of the unsorted portion
3. **Move boundary**: Move the boundary between sorted and unsorted portions one position to the right
4. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in place, only uses a constant amount of extra memory

The selection sort algorithm is simple to understand and implement, making it a good educational example, though it's not efficient for large datasets due to its quadratic time complexity.

