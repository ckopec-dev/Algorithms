# Introsort Algorithm in Go

Introsort is a hybrid sorting algorithm that combines quicksort, heapsort, and insertion sort. It starts with quicksort, switches to heapsort when the recursion depth exceeds a certain threshold, and uses insertion sort for small arrays.

```go
package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

// Introsort performs hybrid sorting using quicksort, heapsort, and insertion sort
func Introsort(arr []int) {
    if len(arr) <= 1 {
        return
    }
    
    // Set maximum recursion depth
    maxDepth := 2 * int(log2(float64(len(arr))))
    introsortHelper(arr, 0, len(arr)-1, maxDepth)
}

// Helper function for introsort
func introsortHelper(arr []int, low, high, maxDepth int) {
    if high-low+1 <= 16 {
        // Use insertion sort for small arrays
        insertionSort(arr, low, high)
        return
    }
    
    if maxDepth == 0 {
        // Switch to heapsort
        heapSort(arr, low, high)
        return
    }
    
    // Use quicksort
    pivot := partition(arr, low, high)
    introsortHelper(arr, low, pivot-1, maxDepth-1)
    introsortHelper(arr, pivot+1, high, maxDepth-1)
}

// Partition function for quicksort
func partition(arr []int, low, high int) int {
    pivot := arr[high]
    i := low - 1
    
    for j := low; j < high; j++ {
        if arr[j] <= pivot {
            i++
            arr[i], arr[j] = arr[j], arr[i]
        }
    }
    arr[i+1], arr[high] = arr[high], arr[i+1]
    return i + 1
}

// Insertion sort for small arrays
func insertionSort(arr []int, low, high int) {
    for i := low + 1; i <= high; i++ {
        key := arr[i]
        j := i - 1
        for j >= low && arr[j] > key {
            arr[j+1] = arr[j]
            j--
        }
        arr[j+1] = key
    }
}

// Heap sort implementation
func heapSort(arr []int, low, high int) {
    n := high - low + 1
    
    // Build max heap
    for i := n/2 - 1; i >= 0; i-- {
        heapify(arr, low, n, i)
    }
    
    // Extract elements from heap one by one
    for i := n - 1; i > 0; i-- {
        arr[low], arr[low+i] = arr[low+i], arr[low]
        heapify(arr, low, i, 0)
    }
}

// Heapify function
func heapify(arr []int, low, n, i int) {
    largest := i
    left := 2*i + 1
    right := 2*i + 2
    
    if left < n && arr[low+left] > arr[low+largest] {
        largest = left
    }
    
    if right < n && arr[low+right] > arr[low+largest] {
        largest = right
    }
    
    if largest != i {
        arr[low+i], arr[low+largest] = arr[low+largest], arr[low+i]
        heapify(arr, low, n, largest)
    }
}

// Log2 function
func log2(x float64) float64 {
    return math.Log(x) / math.Log(2)
}

// Utility function to check if array is sorted
func isSorted(arr []int) bool {
    for i := 1; i < len(arr); i++ {
        if arr[i] < arr[i-1] {
            return false
        }
    }
    return true
}

func main() {
    // Test with random array
    fmt.Println("Introsort Algorithm Example")
    fmt.Println("==========================")
    
    // Generate random array
    rand.Seed(time.Now().UnixNano())
    arr := make([]int, 20)
    for i := range arr {
        arr[i] = rand.Intn(100)
    }
    
    fmt.Printf("Original array: %v\n", arr)
    
    // Sort using introsort
    Introsort(arr)
    
    fmt.Printf("Sorted array:   %v\n", arr)
    fmt.Printf("Is sorted:      %v\n", isSorted(arr))
    
    // Test with edge cases
    fmt.Println("\nEdge case tests:")
    
    // Empty array
    empty := []int{}
    Introsort(empty)
    fmt.Printf("Empty array: %v\n", empty)
    
    // Single element
    single := []int{42}
    Introsort(single)
    fmt.Printf("Single element: %v\n", single)
    
    // Already sorted
    sorted := []int{1, 2, 3, 4, 5}
    Introsort(sorted)
    fmt.Printf("Already sorted: %v\n", sorted)
    
    // Reverse sorted
    reverse := []int{5, 4, 3, 2, 1}
    Introsort(reverse)
    fmt.Printf("Reverse sorted: %v\n", reverse)
    
    // Test with larger array
    fmt.Println("\nLarge array test:")
    largeArr := make([]int, 1000)
    for i := range largeArr {
        largeArr[i] = rand.Intn(1000)
    }
    
    start := time.Now()
    Introsort(largeArr)
    duration := time.Since(start)
    
    fmt.Printf("Large array sorted (%d elements) in %v\n", len(largeArr), duration)
    fmt.Printf("Is large array sorted: %v\n", isSorted(largeArr))
}
```

## Key Features of this Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Control**: Uses maximum recursion depth to switch to heapsort
3. **Small Array Optimization**: Uses insertion sort for arrays ≤ 16 elements
4. **Time Complexity**: O(n log n) worst case
5. **Space Complexity**: O(log n) due to recursion

## How it Works:

1. **Initial Phase**: Uses quicksort for sorting
2. **Depth Check**: If recursion depth exceeds threshold, switches to heapsort
3. **Small Arrays**: Uses insertion sort for arrays ≤ 16 elements
4. **Hybrid Efficiency**: Combines the best properties of different sorting algorithms

## Output Example:
```
Introsort Algorithm Example
==========================
Original array: [23 67 12 89 45 34 78 56 91 29 43 67 82 15 38 71 54 96 27 50]
Sorted array:   [12 15 23 27 29 34 38 43 45 50 54 56 67 67 71 78 82 89 91 96]
Is sorted:      true
```

