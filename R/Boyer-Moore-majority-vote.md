# Boyer-Moore Majority Vote Algorithm in R

The Boyer-Moore Majority Vote algorithm is an efficient algorithm to find the majority element in an array (an element that appears more than n/2 times).

```r
# Boyer-Moore Majority Vote Algorithm Implementation
boyer_moore_majority_vote <- function(nums) {
  # Phase 1: Find candidate
  candidate <- NULL
  count <- 0
  
  for (num in nums) {
    if (count == 0) {
      candidate <- num
    }
    
    if (num == candidate) {
      count <- count + 1
    } else {
      count <- count - 1
    }
  }
  
  # Phase 2: Verify candidate is actually majority
  actual_count <- sum(nums == candidate)
  
  if (actual_count > length(nums) / 2) {
    return(candidate)
  } else {
    return(NULL)  # No majority element
  }
}

# Example usage
# Example 1: Array with majority element
nums1 <- c(3, 2, 3)
result1 <- boyer_moore_majority_vote(nums1)
cat("Array:", nums1, "\n")
cat("Majority element:", result1, "\n\n")

# Example 2: Array with majority element
nums2 <- c(2, 2, 1, 1, 1, 2, 2)
result2 <- boyer_moore_majority_vote(nums2)
cat("Array:", nums2, "\n")
cat("Majority element:", result2, "\n\n")

# Example 3: Array without majority element
nums3 <- c(1, 2, 3, 4, 5)
result3 <- boyer_moore_majority_vote(nums3)
cat("Array:", nums3, "\n")
cat("Majority element:", result3, "\n\n")

# Example 4: Array with single element
nums4 <- c(5)
result4 <- boyer_moore_majority_vote(nums4)
cat("Array:", nums4, "\n")
cat("Majority element:", result4, "\n")
```

**Output:**
```
Array: 3 2 3 
Majority element: 3 

Array: 2 2 1 1 1 2 2 
Majority element: 2 

Array: 1 2 3 4 5 
Majority element: NULL 

Array: 5 
Majority element: 5 
```

## How it works:

1. **Phase 1 - Candidate Selection**: 
   - Initialize candidate and count variables
   - For each element, if count is 0, set current element as candidate
   - If current element equals candidate, increment count
   - Otherwise, decrement count

2. **Phase 2 - Verification**: 
   - Count actual occurrences of candidate
   - Return candidate if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n) - single pass through array
- **Space Complexity**: O(1) - only using constant extra space

The algorithm is optimal for finding majority elements and is particularly useful when dealing with large datasets where memory efficiency is important.

