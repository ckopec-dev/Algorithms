# Counting Sort Algorithm in R

Here's an implementation of the counting sort algorithm in R:

```r
# Counting Sort Function
counting_sort <- function(arr) {
  # Handle edge cases
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Find the range of input values
  min_val <- min(arr)
  max_val <- max(arr)
  range_val <- max_val - min_val + 1
  
  # Create counting array
  count <- rep(0, range_val)
  
  # Count occurrences of each element
  for (i in arr) {
    count[i - min_val + 1] <- count[i - min_val + 1] + 1
  }
  
  # Reconstruct sorted array
  sorted_arr <- c()
  for (i in 1:length(count)) {
    if (count[i] > 0) {
      value <- i + min_val - 1
      sorted_arr <- c(sorted_arr, rep(value, count[i]))
    }
  }
  
  return(sorted_arr)
}

# Example usage
# Test with sample data
test_array <- c(4, 2, 2, 8, 3, 3, 1)
cat("Original array:", paste(test_array, collapse = " "), "\n")
sorted_array <- counting_sort(test_array)
cat("Sorted array:  ", paste(sorted_array, collapse = " "), "\n")

# Another example with different range
test_array2 <- c(10, 5, 8, 5, 10, 3, 7)
cat("\nOriginal array:", paste(test_array2, collapse = " "), "\n")
sorted_array2 <- counting_sort(test_array2)
cat("Sorted array:  ", paste(sorted_array2, collapse = " "), "\n")

# Example with negative numbers
test_array3 <- c(-2, 3, -1, 0, 5, -3, 2)
cat("\nOriginal array:", paste(test_array3, collapse = " "), "\n")
sorted_array3 <- counting_sort(test_array3)
cat("Sorted array:  ", paste(sorted_array3, collapse = " "), "\n")
```

**Output:**
```
Original array: 4 2 2 8 3 3 1
Sorted array:   1 2 2 3 3 4 8

Original array: 10 5 8 5 10 3 7
Sorted array:   3 5 5 7 8 10 10

Original array: -2 3 -1 0 5 -3 2
Sorted array:   -3 -2 -1 0 2 3 5
```

## How it works:

1. **Find range**: Determine the minimum and maximum values to establish the range
2. **Count occurrences**: Create a counting array to store frequency of each element
3. **Reconstruct**: Build the sorted array by iterating through the counting array

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the counting array

## Key Features:
- Handles negative numbers
- Stable sorting (maintains relative order of equal elements)
- Efficient for small range of integers
- Works well when k is not significantly larger than n

