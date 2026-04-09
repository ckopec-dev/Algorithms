# Bucket Sort Algorithm in Go

Here's an implementation of the Bucket Sort algorithm in Go:

```go
package main

import (
    "fmt"
    "sort"
)

// bucketSort sorts an array using the bucket sort algorithm
func bucketSort(arr []float64, bucketCount int) {
    if len(arr) <= 1 {
        return
    }
    
    // Find the minimum and maximum values
    min := arr[0]
    max := arr[0]
    
    for _, value := range arr {
        if value < min {
            min = value
        }
        if value > max {
            max = value
        }
    }
    
    // Create buckets
    bucketSize := (max - min) / float64(bucketCount)
    buckets := make([][]float64, bucketCount)
    
    // Distribute elements into buckets
    for _, value := range arr {
        if value == max {
            buckets[bucketCount-1] = append(buckets[bucketCount-1], value)
        } else {
            index := int((value - min) / bucketSize)
            buckets[index] = append(buckets[index], value)
        }
    }
    
    // Sort individual buckets and concatenate results
    index := 0
    for _, bucket := range buckets {
        if len(bucket) > 0 {
            sort.Float64s(bucket) // Sort each bucket
            for _, value := range bucket {
                arr[index] = value
                index++
            }
        }
    }
}

func main() {
    // Example usage
    arr := []float64{0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51}
    
    fmt.Println("Original array:")
    fmt.Println(arr)
    
    // Sort using bucket sort
    bucketSort(arr, 5)
    
    fmt.Println("Sorted array:")
    fmt.Println(arr)
    
    // Another example with different data
    arr2 := []float64{0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434}
    
    fmt.Println("\nOriginal array 2:")
    fmt.Println(arr2)
    
    bucketSort(arr2, 5)
    
    fmt.Println("Sorted array 2:")
    fmt.Println(arr2)
}
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Buckets**: Divide the range into equal-sized buckets
3. **Distribute Elements**: Place each element into the appropriate bucket
4. **Sort Buckets**: Sort each bucket individually (using built-in sort)
5. **Concatenate**: Combine all sorted buckets into the final result

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements are placed in one bucket

## Space Complexity:
- O(n + k) for the buckets and the input array

## Output:
```
Original array:
[0.42 0.32 0.33 0.52 0.37 0.47 0.51]
Sorted array:
[0.32 0.33 0.37 0.42 0.47 0.51 0.52]

Original array 2:
[0.897 0.565 0.656 0.1234 0.665 0.3434]
Sorted array 2:
[0.1234 0.3434 0.565 0.656 0.665 0.897]
```

This implementation assumes the input values are in the range [0, 1). For other ranges, you would need to normalize the values first.

