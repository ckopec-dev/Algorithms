# Jump Search Algorithm in R

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing linear search in the identified block.

```r
# Jump Search Implementation in R
jump_search <- function(arr, target) {
  n <- length(arr)
  
  # Handle edge cases
  if (n == 0) return(-1)
  
  # Calculate optimal jump size (square root of array length)
  jump_size <- ceiling(sqrt(n))
  
  # Initialize pointers
  prev <- 0
  
  # Jump through the array
  while (arr[min(jump_size, n)] < target) {
    prev <- jump_size
    jump_size <- jump_size + ceiling(sqrt(n))
    
    # If we've gone beyond the array
    if (prev >= n) {
      return(-1)
    }
  }
  
  # Linear search in the identified block
  for (i in (prev + 1):min(jump_size, n)) {
    if (arr[i] == target) {
      return(i - 1)  # Return 0-based index
    }
  }
  
  return(-1)  # Target not found
}

# Example usage
# Create a sorted array
sorted_array <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)

# Test cases
cat("Sorted Array:", sorted_array, "\n\n")

# Search for existing elements
target1 <- 13
result1 <- jump_search(sorted_array, target1)
if (result1 != -1) {
  cat("Element", target1, "found at index:", result1, "\n")
} else {
  cat("Element", target1, "not found\n")
}

target2 <- 7
result2 <- jump_search(sorted_array, target2)
if (result2 != -1) {
  cat("Element", target2, "found at index:", result2, "\n")
} else {
  cat("Element", target2, "not found\n")
}

# Search for non-existing element
target3 <- 10
result3 <- jump_search(sorted_array, target3)
if (result3 != -1) {
  cat("Element", target3, "found at index:", result3, "\n")
} else {
  cat("Element", target3, "not found\n")
}

# Search for element at beginning
target4 <- 1
result4 <- jump_search(sorted_array, target4)
if (result4 != -1) {
  cat("Element", target4, "found at index:", result4, "\n")
} else {
  cat("Element", target4, "not found\n")
}
```

## Output:
```
Sorted Array: 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29

Element 13 found at index: 6
Element 7 found at index: 3
Element 10 not found
Element 1 found at index: 0
```

## How it works:

1. **Calculate jump size**: Uses square root of array length as optimal jump size
2. **Jump through array**: Move through the array in jumps of the calculated size
3. **Find block**: When we find an element greater than target, we know target is in the previous block
4. **Linear search**: Perform linear search within the identified block

## Time Complexity:
- **Best case**: O(1) - element found at first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)
- **Space complexity**: O(1)

## Advantages:
- More efficient than linear search for large sorted arrays
- Simpler than binary search implementation
- Works well when array size is large and random access is expensive

## Disadvantages:
- Requires sorted array
- Not as efficient as binary search for very large arrays
- Performance depends on jump size calculation

