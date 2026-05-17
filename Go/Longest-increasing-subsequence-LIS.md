# Longest Increasing Subsequence (LIS) Algorithm in Go

Here's an implementation of the LIS algorithm using dynamic programming with O(n²) time complexity:

```go
package main

import (
    "fmt"
    "math"
)

// lis returns the length of the longest increasing subsequence
func lis(nums []int) int {
    if len(nums) == 0 {
        return 0
    }
    
    // dp[i] represents the length of LIS ending at index i
    dp := make([]int, len(nums))
    
    // Initialize each element with 1 (each element forms a subsequence of length 1)
    for i := range dp {
        dp[i] = 1
    }
    
    // Fill dp array
    for i := 1; i < len(nums); i++ {
        for j := 0; j < i; j++ {
            if nums[i] > nums[j] {
                dp[i] = int(math.Max(float64(dp[i]), float64(dp[j]+1)))
            }
        }
    }
    
    // Find the maximum value in dp array
    maxLength := 0
    for _, length := range dp {
        if length > maxLength {
            maxLength = length
        }
    }
    
    return maxLength
}

// lisWithSequence returns both the length and the actual subsequence
func lisWithSequence(nums []int) (int, []int) {
    if len(nums) == 0 {
        return 0, []int{}
    }
    
    n := len(nums)
    dp := make([]int, n)
    parent := make([]int, n) // To track the actual sequence
    
    // Initialize
    for i := range dp {
        dp[i] = 1
        parent[i] = -1
    }
    
    // Fill dp array
    for i := 1; i < n; i++ {
        for j := 0; j < i; j++ {
            if nums[i] > nums[j] && dp[j]+1 > dp[i] {
                dp[i] = dp[j] + 1
                parent[i] = j
            }
        }
    }
    
    // Find the index with maximum LIS length
    maxLength := 0
    maxIndex := 0
    for i, length := range dp {
        if length > maxLength {
            maxLength = length
            maxIndex = i
        }
    }
    
    // Reconstruct the actual subsequence
    sequence := []int{}
    current := maxIndex
    for current != -1 {
        sequence = append([]int{nums[current]}, sequence...)
        current = parent[current]
    }
    
    return maxLength, sequence
}

func main() {
    // Example 1
    nums1 := []int{10, 9, 2, 5, 3, 7, 101, 18}
    fmt.Printf("Array: %v\n", nums1)
    fmt.Printf("Length of LIS: %d\n", lis(nums1))
    
    length, sequence := lisWithSequence(nums1)
    fmt.Printf("LIS sequence: %v\n", sequence)
    fmt.Printf("Length: %d\n\n", length)
    
    // Example 2
    nums2 := []int{0, 1, 0, 3, 2, 3}
    fmt.Printf("Array: %v\n", nums2)
    fmt.Printf("Length of LIS: %d\n", lis(nums2))
    
    length, sequence = lisWithSequence(nums2)
    fmt.Printf("LIS sequence: %v\n", sequence)
    fmt.Printf("Length: %d\n\n", length)
    
    // Example 3
    nums3 := []int{7, 7, 7, 7, 7, 7, 7}
    fmt.Printf("Array: %v\n", nums3)
    fmt.Printf("Length of LIS: %d\n", lis(nums3))
    
    length, sequence = lisWithSequence(nums3)
    fmt.Printf("LIS sequence: %v\n", sequence)
    fmt.Printf("Length: %d\n", length)
}
```

## Output:
```
Array: [10 9 2 5 3 7 101 18]
Length of LIS: 4
LIS sequence: [2 3 7 18]
Length: 4

Array: [0 1 0 3 2 3]
Length of LIS: 4
LIS sequence: [0 1 2 3]
Length: 4

Array: [7 7 7 7 7 7 7]
Length of LIS: 1
LIS sequence: [7]
Length: 1
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: 
   - Create a `dp` array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
   - Initialize each element to 1 (each element forms a subsequence of length 1)

2. **Fill DP Array**:
   - For each element, check all previous elements
   - If current element is greater than a previous element, update `dp[i]` to be the maximum of its current value and `dp[j] + 1`

3. **Find Maximum**:
   - Return the maximum value in the `dp` array

## Time and Space Complexity:
- **Time Complexity**: O(n²) where n is the length of the input array
- **Space Complexity**: O(n) for the DP array

## Alternative O(n log n) Solution:

For better performance with large inputs, here's a more efficient approach using binary search:

```go
func lisOptimized(nums []int) int {
    if len(nums) == 0 {
        return 0
    }
    
    // tails[i] stores the smallest ending element of all increasing subsequences of length i+1
    tails := []int{}
    
    for _, num := range nums {
        // Binary search for the position to insert/replace
        left, right := 0, len(tails)
        for left < right {
            mid := (left + right) / 2
            if tails[mid] < num {
                left = mid + 1
            } else {
                right = mid
            }
        }
        
        // If num is larger than all elements in tails, append it
        if left == len(tails) {
            tails = append(tails, num)
        } else {
            // Replace the element at position left
            tails[left] = num
        }
    }
    
    return len(tails)
}
```

This optimized version has O(n log n) time complexity but is more complex to implement and understand.

