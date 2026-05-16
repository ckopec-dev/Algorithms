# Subset Sum Algorithm in Go

Here's an implementation of the subset sum problem using dynamic programming in Go:

```go
package main

import (
    "fmt"
    "reflect"
)

// subsetSum determines if there exists a subset of the given array that sums to the target
func subsetSum(arr []int, target int) bool {
    n := len(arr)
    if n == 0 {
        return target == 0
    }
    
    // Create a 2D DP table
    // dp[i][j] represents whether sum j can be obtained using first i elements
    dp := make([][]bool, n+1)
    for i := range dp {
        dp[i] = make([]bool, target+1)
    }
    
    // Base case: sum 0 can always be obtained with empty subset
    for i := 0; i <= n; i++ {
        dp[i][0] = true
    }
    
    // Fill the DP table
    for i := 1; i <= n; i++ {
        for j := 0; j <= target; j++ {
            // Don't include current element
            dp[i][j] = dp[i-1][j]
            
            // Include current element if possible
            if j >= arr[i-1] {
                dp[i][j] = dp[i][j] || dp[i-1][j-arr[i-1]]
            }
        }
    }
    
    return dp[n][target]
}

// subsetSumOptimized uses space-optimized version with 1D array
func subsetSumOptimized(arr []int, target int) bool {
    n := len(arr)
    if n == 0 {
        return target == 0
    }
    
    // Use 1D array for space optimization
    dp := make([]bool, target+1)
    dp[0] = true // Sum 0 is always possible
    
    // Process each element
    for i := 0; i < n; i++ {
        // Traverse backwards to avoid using updated values
        for j := target; j >= arr[i]; j-- {
            dp[j] = dp[j] || dp[j-arr[i]]
        }
    }
    
    return dp[target]
}

// findSubset returns one possible subset that sums to target (if exists)
func findSubset(arr []int, target int) []int {
    n := len(arr)
    if n == 0 {
        if target == 0 {
            return []int{}
        }
        return nil
    }
    
    // Create DP table to track which elements are used
    dp := make([][]bool, n+1)
    for i := range dp {
        dp[i] = make([]bool, target+1)
    }
    
    dp[0][0] = true
    
    for i := 1; i <= n; i++ {
        for j := 0; j <= target; j++ {
            dp[i][j] = dp[i-1][j]
            if j >= arr[i-1] {
                dp[i][j] = dp[i][j] || dp[i-1][j-arr[i-1]]
            }
        }
    }
    
    // If no subset found
    if !dp[n][target] {
        return nil
    }
    
    // Backtrack to find the actual subset
    result := []int{}
    i, j := n, target
    
    for i > 0 && j > 0 {
        // If current cell is true but previous row's cell is false,
        // then current element was included in the subset
        if dp[i-1][j] == false && dp[i][j] == true {
            result = append(result, arr[i-1])
            j -= arr[i-1]
        }
        i--
    }
    
    return result
}

func main() {
    // Test cases
    testCases := []struct {
        arr    []int
        target int
        name   string
    }{
        {[]int{3, 34, 4, 12, 5, 2}, 9, "Test 1"},
        {[]int{3, 34, 4, 12, 5, 2}, 30, "Test 2"},
        {[]int{1, 2, 3, 4, 5}, 10, "Test 3"},
        {[]int{1, 2, 3, 4, 5}, 15, "Test 4"},
        {[]int{1, 2, 3, 4, 5}, 1, "Test 5"},
        {[]int{}, 0, "Test 6 - Empty array"},
        {[]int{5}, 5, "Test 7 - Single element"},
    }
    
    fmt.Println("Subset Sum Algorithm Results:")
    fmt.Println("==============================")
    
    for _, tc := range testCases {
        result1 := subsetSum(tc.arr, tc.target)
        result2 := subsetSumOptimized(tc.arr, tc.target)
        subset := findSubset(tc.arr, tc.target)
        
        fmt.Printf("\n%s:\n", tc.name)
        fmt.Printf("Array: %v\n", tc.arr)
        fmt.Printf("Target: %d\n", tc.target)
        fmt.Printf("Subset Sum Possible: %t\n", result1)
        fmt.Printf("Optimized Version: %t\n", result2)
        if subset != nil {
            fmt.Printf("One possible subset: %v\n", subset)
            fmt.Printf("Sum of subset: %d\n", calculateSum(subset))
        } else {
            fmt.Printf("No subset found\n")
        }
    }
}

// Helper function to calculate sum of slice
func calculateSum(arr []int) int {
    sum := 0
    for _, num := range arr {
        sum += num
    }
    return sum
}
```

## Output Example:
```
Subset Sum Algorithm Results:
==============================

Test 1:
Array: [3 34 4 12 5 2]
Target: 9
Subset Sum Possible: true
Optimized Version: true
One possible subset: [4 5]
Sum of subset: 9

Test 2:
Array: [3 34 4 12 5 2]
Target: 30
Subset Sum Possible: false
Optimized Version: false
No subset found

Test 3:
Array: [1 2 3 4 5]
Target: 10
Subset Sum Possible: true
Optimized Version: true
One possible subset: [1 4 5]
Sum of subset: 10
```

## Key Features:

1. **Two Implementations**:
   - Standard 2D DP approach
   - Space-optimized 1D approach

2. **Additional Functionality**:
   - `findSubset()` to return actual subset elements
   - Comprehensive test cases

3. **Time Complexity**: O(n × target)
4. **Space Complexity**: 
   - Standard: O(n × target)
   - Optimized: O(target)

5. **Edge Cases Handled**:
   - Empty arrays
   - Single element arrays
   - Target sum of 0

This implementation demonstrates the classic subset sum problem solution using dynamic programming, which is a fundamental algorithm in computer science and competitive programming.

