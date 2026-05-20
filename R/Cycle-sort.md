# Cycle Sort Algorithm in R

Cycle sort is an in-place, unstable sorting algorithm that is particularly efficient for sorting arrays where the elements are in the range [1, n]. It minimizes the number of writes to the array.

## R Implementation

```r
# Cycle Sort Algorithm in R
cycle_sort <- function(arr) {
  n <- length(arr)
  
  # Count the number of cycles
  cycles <- 0
  
  # Traverse the array to find cycles
  for (start in 1:(n - 1)) {
    # If element is already in correct position
    if (arr[start] == start) {
      next
    }
    
    # Find the correct position for the current element
    item <- arr[start]
    pos <- start
    
    # Find where to put the item
    for (i in (start + 1):n) {
      if (arr[i] < item) {
        pos <- pos + 1
      }
    }
    
    # If item is already in correct position
    if (pos == start) {
      next
    }
    
    # Put item at its correct position
    # If duplicate elements exist, put item at the next available position
    while (item == arr[pos]) {
      pos <- pos + 1
    }
    
    # Swap the item with its correct position
    temp <- arr[pos]
    arr[pos] <- item
    item <- temp
    
    # Continue the cycle
    while (pos != start) {
      pos <- start
      
      # Find correct position for item
      for (i in (start + 1):n) {
        if (arr[i] < item) {
          pos <- pos + 1
        }
      }
      
      # If item is already in correct position
      while (item == arr[pos]) {
        pos <- pos + 1
      }
      
      # Swap item with its correct position
      temp <- arr[pos]
      arr[pos] <- item
      item <- temp
    }
    
    cycles <- cycles + 1
  }
  
  return(arr)
}

# Example usage
# Test with an array
test_array <- c(4, 3, 2, 1)
cat("Original array:", paste(test_array, collapse = " "), "\n")

# Sort the array
sorted_array <- cycle_sort(test_array)
cat("Sorted array:", paste(sorted_array, collapse = " "), "\n")

# Another example
test_array2 <- c(3, 1, 5, 4, 2)
cat("\nOriginal array:", paste(test_array2, collapse = " "), "\n")

# Sort the array
sorted_array2 <- cycle_sort(test_array2)
cat("Sorted array:", paste(sorted_array2, collapse = " "), "\n")
```

## Output
```
Original array: 4 3 2 1
Sorted array: 1 2 3 4

Original array: 3 1 5 4 2
Sorted array: 1 2 3 4 5
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation
2. **Position Finding**: For each element, it determines where it should be placed
3. **Swapping**: Elements are swapped to their correct positions
4. **Cycle Completion**: The process continues until all elements are in their correct positions

## Key Characteristics:

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - in-place sorting
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **Use Case**: Most efficient when the array elements are in range [1, n]

The algorithm is particularly useful when minimizing the number of writes to memory is important, as it performs at most n-1 swaps in the worst case.

