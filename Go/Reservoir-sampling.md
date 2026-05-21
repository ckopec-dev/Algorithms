# Reservoir Sampling Algorithm in Go

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size. Here's an implementation in Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// ReservoirSample samples k items from a stream of unknown size
func ReservoirSample(stream []int, k int) []int {
    if len(stream) <= k {
        return stream
    }
    
    // Initialize reservoir with first k elements
    reservoir := make([]int, k)
    for i := 0; i < k; i++ {
        reservoir[i] = stream[i]
    }
    
    // Process remaining elements
    for i := k; i < len(stream); i++ {
        // Generate random index between 0 and i (inclusive)
        j := rand.Intn(i + 1)
        
        // If j is less than k, replace element in reservoir
        if j < k {
            reservoir[j] = stream[i]
        }
    }
    
    return reservoir
}

// ReservoirSampleWithIndex returns both sampled values and their original indices
func ReservoirSampleWithIndex(stream []int, k int) ([]int, []int) {
    if len(stream) <= k {
        indices := make([]int, len(stream))
        for i := range stream {
            indices[i] = i
        }
        return stream, indices
    }
    
    reservoir := make([]int, k)
    indices := make([]int, k)
    
    // Initialize reservoir with first k elements
    for i := 0; i < k; i++ {
        reservoir[i] = stream[i]
        indices[i] = i
    }
    
    // Process remaining elements
    for i := k; i < len(stream); i++ {
        j := rand.Intn(i + 1)
        
        if j < k {
            reservoir[j] = stream[i]
            indices[j] = i
        }
    }
    
    return reservoir, indices
}

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())
    
    // Example 1: Sample 3 items from a stream of 10 items
    stream1 := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    k1 := 3
    
    fmt.Println("Example 1:")
    fmt.Printf("Stream: %v\n", stream1)
    fmt.Printf("Sample %d items: %v\n", k1, ReservoirSample(stream1, k1))
    
    // Example 2: Sample 4 items from a larger stream
    stream2 := []int{10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120}
    k2 := 4
    
    fmt.Println("\nExample 2:")
    fmt.Printf("Stream: %v\n", stream2)
    fmt.Printf("Sample %d items: %v\n", k2, ReservoirSample(stream2, k2))
    
    // Example 3: With indices
    fmt.Println("\nExample 3 (with indices):")
    values, indices := ReservoirSampleWithIndex(stream1, 3)
    fmt.Printf("Sampled values: %v\n", values)
    fmt.Printf("Original indices: %v\n", indices)
    
    // Demonstrate multiple runs to show randomness
    fmt.Println("\nMultiple runs (k=3 from stream of 10):")
    for i := 0; i < 5; i++ {
        result := ReservoirSample(stream1, 3)
        fmt.Printf("Run %d: %v\n", i+1, result)
    }
}
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements from the stream
2. **Processing**: For each subsequent element at position i (starting from k):
   - Generate a random number j between 0 and i (inclusive)
   - If j < k, replace reservoir[j] with the current element
3. **Result**: After processing all elements, the reservoir contains a random sample

## Key Properties:

- **Time Complexity**: O(n) where n is the size of the stream
- **Space Complexity**: O(k) for the reservoir
- **Probability**: Each element has equal probability (k/n) of being selected
- **Memory Efficient**: Works with streams of unknown size

## Output Example:
```
Example 1:
Stream: [1 2 3 4 5 6 7 8 9 10]
Sample 3 items: [2 7 10]

Example 2:
Stream: [10 20 30 40 50 60 70 80 90 100 110 120]
Sample 4 items: [30 80 110 120]

Multiple runs (k=3 from stream of 10):
Run 1: [1 5 9]
Run 2: [2 6 10]
Run 3: [3 7 8]
Run 4: [1 4 10]
Run 5: [2 5 7]
```

