# Bead Sort Algorithm in R

```r
# Bead Sort Implementation in R
bead_sort <- function(arr) {
  # Handle edge cases
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Find maximum value to determine number of rows
  max_val <- max(arr)
  
  # Create a "bead" matrix where each row represents a level
  # and each column represents an element
  bead_matrix <- matrix(0, nrow = max_val, ncol = length(arr))
  
  # Place beads (1s) in the matrix
  for (i in 1:length(arr)) {
    for (j in 1:arr[i]) {
      bead_matrix[j, i] <- 1
    }
  }
  
  # Count beads in each column (this represents the sorted values)
  sorted <- numeric(length(arr))
  for (i in 1:length(arr)) {
    sorted[i] <- sum(bead_matrix[, i])
  }
  
  # Return sorted array (in descending order)
  return(sorted)
}

# Alternative implementation that's more intuitive
bead_sort_simple <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Create a "bead" representation
  max_val <- max(arr)
  beads <- numeric(max_val)
  
  # Count how many beads of each size we have
  for (val in arr) {
    beads[val] <- beads[val] + 1
  }
  
  # Reconstruct the sorted array (descending order)
  result <- numeric(length(arr))
  idx <- 1
  
  for (i in max_val:1) {
    if (beads[i] > 0) {
      for (j in 1:beads[i]) {
        result[idx] <- i
        idx <- idx + 1
      }
    }
  }
  
  return(result)
}

# Example usage
cat("Bead Sort Algorithm in R\n")
cat("========================\n\n")

# Test array
test_array <- c(5, 2, 8, 1, 9, 3, 7, 4, 6)
cat("Original array:", paste(test_array, collapse = " "), "\n")

# Sort using bead sort
sorted_array <- bead_sort_simple(test_array)
cat("Sorted array:  ", paste(sorted_array, collapse = " "), "\n\n")

# Another example
test_array2 <- c(3, 1, 4, 1, 5, 9, 2, 6)
cat("Original array:", paste(test_array2, collapse = " "), "\n")
sorted_array2 <- bead_sort_simple(test_array2)
cat("Sorted array:  ", paste(sorted_array2, collapse = " "), "\n")

# Visual representation of the bead sorting process
visual_bead_sort <- function(arr) {
  cat("\nVisual representation of bead sorting:\n")
  cat("Original:", paste(arr, collapse = " "), "\n")
  
  max_val <- max(arr)
  cat("Max value:", max_val, "\n")
  
  # Show the bead counting process
  beads <- numeric(max_val)
  for (val in arr) {
    beads[val] <- beads[val] + 1
  }
  
  cat("Bead counts (size -> count): ")
  for (i in max_val:1) {
    if (beads[i] > 0) {
      cat(i, "->", beads[i], "  ")
    }
  }
  cat("\n")
  
  # Show sorted result
  result <- numeric(length(arr))
  idx <- 1
  for (i in max_val:1) {
    if (beads[i] > 0) {
      for (j in 1:beads[i]) {
        result[idx] <- i
        idx <- idx + 1
      }
    }
  }
  
  cat("Final sorted array:", paste(result, collapse = " "), "\n")
}

# Run visual example
visual_bead_sort(c(4, 2, 7, 1, 3))
```

## How Bead Sort Works

The bead sort algorithm works by:

1. **Creating a bead representation**: Each element in the array represents a "column" of beads
2. **Dropping beads**: Beads fall down due to gravity, filling the lowest available positions
3. **Counting beads**: The final position of beads in each column gives us the sorted values

## Time and Space Complexity

- **Time Complexity**: O(S) where S is the sum of all elements in the array
- **Space Complexity**: O(max_val) where max_val is the maximum element in the array

## Key Features

- **Natural sorting**: Works by simulating physical bead movement
- **Stable sorting**: Maintains relative order of equal elements
- **Efficient for small max values**: Particularly effective when the range of values is small

The algorithm is most practical for sorting arrays with small integer values and demonstrates an interesting approach to sorting that mimics physical processes.

