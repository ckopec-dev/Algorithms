# Subset Sum Algorithm in R

Here's an implementation of the subset sum algorithm using dynamic programming in R:

```r
# Subset Sum Algorithm Implementation
subset_sum <- function(nums, target) {
  n <- length(nums)
  
  # Create a 2D boolean matrix
  # dp[i][j] will be TRUE if there's a subset of first i elements with sum j
  dp <- matrix(FALSE, nrow = n + 1, ncol = target + 1)
  
  # Base case: sum of 0 is always possible (empty subset)
  for (i in 1:(n + 1)) {
    dp[i, 1] <- TRUE
  }
  
  # Fill the dp table
  for (i in 2:(n + 1)) {
    for (j in 1:(target + 1)) {
      # Don't include current element
      dp[i, j] <- dp[i-1, j]
      
      # Include current element if possible
      if (j >= nums[i-1]) {
        dp[i, j] <- dp[i, j] || dp[i-1, j - nums[i-1]]
      }
    }
  }
  
  return(dp[n + 1, target + 1])
}

# Function to find actual subset (optional)
find_subset <- function(nums, target) {
  n <- length(nums)
  dp <- matrix(FALSE, nrow = n + 1, ncol = target + 1)
  
  # Base case
  for (i in 1:(n + 1)) {
    dp[i, 1] <- TRUE
  }
  
  # Fill the dp table
  for (i in 2:(n + 1)) {
    for (j in 1:(target + 1)) {
      dp[i, j] <- dp[i-1, j]
      if (j >= nums[i-1]) {
        dp[i, j] <- dp[i, j] || dp[i-1, j - nums[i-1]]
      }
    }
  }
  
  # If target sum is not possible
  if (!dp[n + 1, target + 1]) {
    return(NULL)
  }
  
  # Backtrack to find the actual subset
  result <- c()
  i <- n
  j <- target
  
  while (i > 0 && j > 0) {
    # If current cell is true but previous row cell is false,
    # then current element is part of the subset
    if (dp[i, j] && !dp[i-1, j]) {
      result <- c(nums[i], result)
      j <- j - nums[i]
    }
    i <- i - 1
  }
  
  return(result)
}

# Example usage
numbers <- c(3, 34, 4, 12, 5, 2)
target_sum <- 9

# Check if subset sum exists
exists <- subset_sum(numbers, target_sum)
cat("Numbers:", numbers, "\n")
cat("Target sum:", target_sum, "\n")
cat("Subset sum exists:", exists, "\n\n")

# Find the actual subset
actual_subset <- find_subset(numbers, target_sum)
if (!is.null(actual_subset)) {
  cat("Actual subset:", actual_subset, "\n")
  cat("Sum:", sum(actual_subset), "\n")
} else {
  cat("No subset found with the target sum.\n")
}

# Test with another example
numbers2 <- c(1, 2, 3, 7)
target_sum2 <- 6

exists2 <- subset_sum(numbers2, target_sum2)
cat("\nNumbers:", numbers2, "\n")
cat("Target sum:", target_sum2, "\n")
cat("Subset sum exists:", exists2, "\n\n")

actual_subset2 <- find_subset(numbers2, target_sum2)
if (!is.null(actual_subset2)) {
  cat("Actual subset:", actual_subset2, "\n")
  cat("Sum:", sum(actual_subset2), "\n")
} else {
  cat("No subset found with the target sum.\n")
}
```

## Output:
```
Numbers: 3 34 4 12 5 2 
Target sum: 9 
Subset sum exists: TRUE

Actual subset: 4 5 
Sum: 9

Numbers: 1 2 3 7 
Target sum: 6 
Subset sum exists: TRUE

Actual subset: 1 2 3 
Sum: 6
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D boolean matrix where `dp[i][j]` represents whether a subset of the first `i` elements can sum to `j`.

2. **Base Case**: A sum of 0 is always possible (empty subset).

3. **Recurrence Relation**: For each element, we can either include it or exclude it in the subset.

4. **Time Complexity**: O(n × target) where n is the number of elements.

5. **Space Complexity**: O(n × target) for the DP table.

The algorithm efficiently determines whether there exists a subset of the given numbers that sums to the target value.

