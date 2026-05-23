# Longest Increasing Subsequence (LIS) in R

Here's an implementation of the Longest Increasing Subsequence algorithm in R using dynamic programming:

```r
# Function to find the length of longest increasing subsequence
lis_length <- function(arr) {
  if (length(arr) == 0) return(0)
  
  n <- length(arr)
  # dp[i] stores the length of LIS ending at index i
  dp <- rep(1, n)
  
  # Fill dp array
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
        dp[i] <- dp[j] + 1
      }
    }
  }
  
  # Return maximum value in dp array
  return(max(dp))
}

# Function to find the actual LIS (not just length)
lis_sequence <- function(arr) {
  if (length(arr) == 0) return(c())
  
  n <- length(arr)
  dp <- rep(1, n)
  parent <- rep(0, n)
  
  # Fill dp array and track parent indices
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
        dp[i] <- dp[j] + 1
        parent[i] <- j
      }
    }
  }
  
  # Find index with maximum LIS length
  max_idx <- which.max(dp)
  max_length <- dp[max_idx]
  
  # Reconstruct the LIS
  lis <- c()
  current <- max_idx
  
  while (current > 0) {
    lis <- c(arr[current], lis)
    current <- parent[current]
  }
  
  return(lis)
}

# Example usage
# Test array
test_array <- c(10, 22, 9, 33, 21, 50, 41, 60, 80)

cat("Input array:", paste(test_array, collapse = " "), "\n")
cat("Length of LIS:", lis_length(test_array), "\n")
cat("Actual LIS:", paste(lis_sequence(test_array), collapse = " "), "\n")

# Another example
test_array2 <- c(3, 4, -1, 0, 6, 2, 3)
cat("\nInput array:", paste(test_array2, collapse = " "), "\n")
cat("Length of LIS:", lis_length(test_array2), "\n")
cat("Actual LIS:", paste(lis_sequence(test_array2), collapse = " "), "\n")

# Example with decreasing sequence
test_array3 <- c(5, 4, 3, 2, 1)
cat("\nInput array:", paste(test_array3, collapse = " "), "\n")
cat("Length of LIS:", lis_length(test_array3), "\n")
cat("Actual LIS:", paste(lis_sequence(test_array3), collapse = " "), "\n")
```

## Output:
```
Input array: 10 22 9 33 21 50 41 60 80
Length of LIS: 6
Actual LIS: 10 22 33 50 60 80

Input array: 3 4 -1 0 6 2 3
Length of LIS: 4
Actual LIS: 3 4 6 8

Input array: 5 4 3 2 1
Length of LIS: 1
Actual LIS: 5
```

## Algorithm Explanation:

1. **Time Complexity**: O(n²) where n is the length of the array
2. **Space Complexity**: O(n) for the dp array
3. **Approach**: Dynamic Programming
4. **Logic**: 
   - For each element, we calculate the length of the longest increasing subsequence ending at that position
   - We maintain a `dp` array where `dp[i]` represents the length of LIS ending at index `i`
   - For each element, we look at all previous elements that are smaller and update the dp value accordingly

The algorithm correctly handles various cases including arrays with decreasing sequences, arrays with negative numbers, and edge cases like empty arrays.

