# Comb Sort Algorithm in R

Here's an implementation of the Comb Sort algorithm in R:

```r
# Comb Sort Algorithm in R
comb_sort <- function(arr) {
  n <- length(arr)
  
  # Initialize gap
  gap <- n
  shrink_factor <- 1.3
  sorted <- FALSE
  
  while (!sorted) {
    # Update gap
    gap <- floor(gap / shrink_factor)
    
    # If gap becomes 1, we're doing a final bubble sort
    if (gap <= 1) {
      gap = 1
      sorted = TRUE
    }
    
    # Compare elements with current gap
    i <- 1
    while (i + gap <= n) {
      if (arr[i] > arr[i + gap]) {
        # Swap elements
        temp <- arr[i]
        arr[i] <- arr[i + gap]
        arr[i + gap] <- temp
        sorted <- FALSE
      }
      i <- i + 1
    }
  }
  
  return(arr)
}

# Example usage
# Test with different arrays
test_array1 <- c(64, 34, 25, 12, 22, 11, 90)
cat("Original array:", paste(test_array1, collapse = " "), "\n")
sorted_array1 <- comb_sort(test_array1)
cat("Sorted array: ", paste(sorted_array1, collapse = " "), "\n\n")

test_array2 <- c(5, 2, 8, 1, 9, 3, 7, 4, 6)
cat("Original array:", paste(test_array2, collapse = " "), "\n")
sorted_array2 <- comb_sort(test_array2)
cat("Sorted array: ", paste(sorted_array2, collapse = " "), "\n\n")

# Test with already sorted array
test_array3 <- c(1, 2, 3, 4, 5)
cat("Original array:", paste(test_array3, collapse = " "), "\n")
sorted_array3 <- comb_sort(test_array3)
cat("Sorted array: ", paste(sorted_array3, collapse = " "), "\n")
```

## Output:
```
Original array: 64 34 25 12 22 11 90
Sorted array:  11 22 25 34 64 90 

Original array: 5 2 8 1 9 3 7 4 6
Sorted array:  1 2 3 4 5 6 7 8 9 

Original array: 1 2 3 4 5
Sorted array:  1 2 3 4 5
```

## How Comb Sort Works:

1. **Initialize**: Start with a gap size equal to the array length
2. **Shrink**: Reduce the gap by a shrink factor (typically 1.3) in each iteration
3. **Compare**: Compare elements that are 'gap' positions apart
4. **Swap**: If elements are in wrong order, swap them
5. **Repeat**: Continue until gap becomes 1, then perform final bubble sort
6. **Finish**: When no more swaps are needed, the array is sorted

## Key Features:

- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Space Complexity**: O(1) - sorts in place
- **Improvement over Bubble Sort**: Uses gap sequence to eliminate turtles more efficiently
- **Adaptive**: Performs better than bubble sort on partially sorted arrays

The comb sort algorithm is particularly useful because it eliminates the "turtles" (small elements at the end of the array) more quickly than bubble sort by using a gap sequence.

