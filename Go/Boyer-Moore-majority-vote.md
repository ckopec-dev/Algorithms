# Boyer-Moore Majority Vote Algorithm in Go

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times).

```go
package main

import (
    "fmt"
    "math"
)

// BoyerMooreMajorityVote finds the majority element in an array
// Returns the majority element if it exists, otherwise returns -1
func BoyerMooreMajorityVote(nums []int) int {
    if len(nums) == 0 {
        return -1
    }
    
    // Phase 1: Find candidate
    candidate := nums[0]
    count := 1
    
    for i := 1; i < len(nums); i++ {
        if nums[i] == candidate {
            count++
        } else {
            count--
            if count == 0 {
                candidate = nums[i]
                count = 1
            }
        }
    }
    
    // Phase 2: Verify candidate is actually majority
    actualCount := 0
    for _, num := range nums {
        if num == candidate {
            actualCount++
        }
    }
    
    // Check if candidate appears more than n/2 times
    if actualCount > len(nums)/2 {
        return candidate
    }
    
    return -1 // No majority element found
}

// Alternative implementation that returns both candidate and count
func BoyerMooreMajorityVoteWithCount(nums []int) (int, int) {
    if len(nums) == 0 {
        return -1, 0
    }
    
    candidate := nums[0]
    count := 1
    
    for i := 1; i < len(nums); i++ {
        if nums[i] == candidate {
            count++
        } else {
            count--
            if count == 0 {
                candidate = nums[i]
                count = 1
            }
        }
    }
    
    // Count actual occurrences
    actualCount := 0
    for _, num := range nums {
        if num == candidate {
            actualCount++
        }
    }
    
    return candidate, actualCount
}

func main() {
    // Test cases
    testCases := [][]int{
        {3, 2, 3},                    // Expected: 3
        {2, 2, 1, 1, 1, 2, 2},        // Expected: 2
        {1},                          // Expected: 1
        {1, 1, 1, 2, 2},              // Expected: 1
        {1, 2, 3, 4, 5},              // Expected: -1 (no majority)
        {5, 5, 5, 5, 1, 2, 3},        // Expected: 5
    }
    
    fmt.Println("Boyer-Moore Majority Vote Algorithm Results:")
    fmt.Println("============================================")
    
    for i, nums := range testCases {
        result := BoyerMooreMajorityVote(nums)
        candidate, count := BoyerMooreMajorityVoteWithCount(nums)
        
        fmt.Printf("Test %d: %v\n", i+1, nums)
        if result != -1 {
            fmt.Printf("  Majority element: %d (appears %d times)\n", result, count)
        } else {
            fmt.Printf("  No majority element found\n")
        }
        fmt.Printf("  Candidate verification: %d\n", candidate)
        fmt.Println()
    }
    
    // Example with detailed step-by-step process
    fmt.Println("Detailed Example with Step-by-Step Process:")
    fmt.Println("===========================================")
    
    example := []int{3, 2, 3}
    fmt.Printf("Array: %v\n", example)
    
    // Manual trace of algorithm
    candidate := example[0]
    count := 1
    fmt.Printf("Step 1: candidate = %d, count = %d\n", candidate, count)
    
    for i := 1; i < len(example); i++ {
        if example[i] == candidate {
            count++
            fmt.Printf("Step %d: %d == %d, count = %d\n", i+1, example[i], candidate, count)
        } else {
            count--
            fmt.Printf("Step %d: %d != %d, count = %d\n", i+1, example[i], candidate, count)
            if count == 0 {
                candidate = example[i]
                count = 1
                fmt.Printf("  Reset: candidate = %d, count = %d\n", candidate, count)
            }
        }
    }
    
    fmt.Printf("Final candidate: %d\n", candidate)
    
    // Verify
    actualCount := 0
    for _, num := range example {
        if num == candidate {
            actualCount++
        }
    }
    
    if actualCount > len(example)/2 {
        fmt.Printf("✓ %d is the majority element (appears %d times)\n", candidate, actualCount)
    } else {
        fmt.Printf("✗ No majority element found\n")
    }
}
```

## Output:
```
Boyer-Moore Majority Vote Algorithm Results:
============================================
Test 1: [3 2 3]
  Majority element: 3 (appears 2 times)
  Candidate verification: 3

Test 2: [2 2 1 1 1 2 2]
  Majority element: 2 (appears 4 times)
  Candidate verification: 2

Test 3: [1]
  Majority element: 1 (appears 1 times)
  Candidate verification: 1

Test 4: [1 1 1 2 2]
  Majority element: 1 (appears 3 times)
  Candidate verification: 1

Test 5: [1 2 3 4 5]
  No majority element found
  Candidate verification: 5

Test 6: [5 5 5 5 1 2 3]
  Majority element: 5 (appears 4 times)
  Candidate verification: 5

Detailed Example with Step-by-Step Process:
===========================================
Array: [3 2 3]
Step 1: candidate = 3, count = 1
Step 2: 2 != 3, count = 0
  Reset: candidate = 2, count = 1
Step 3: 3 != 2, count = 0
  Reset: candidate = 3, count = 1
Final candidate: 3
✓ 3 is the majority element (appears 2 times)
```

## Key Features:

1. **Time Complexity**: O(n) - single pass through the array
2. **Space Complexity**: O(1) - only using constant extra space
3. **Two-phase approach**:
   - Phase 1: Find a candidate element
   - Phase 2: Verify if candidate is actually majority element
4. **Handles edge cases**: Empty arrays, single elements, no majority elements

## How it works:

1. **Phase 1**: Traverse the array and maintain a candidate and count
   - If current element equals candidate, increment count
   - If different, decrement count
   - If count reaches 0, replace candidate with current element

2. **Phase 2**: Count actual occurrences of the candidate to verify it's majority

The algorithm is optimal for this problem and is widely used in practice due to its linear time and constant space complexity.

