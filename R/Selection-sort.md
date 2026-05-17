# Selection Sort Algorithm in R

Here's an implementation of the selection sort algorithm in R:

```r
# Selection Sort Function
selection_sort <- function(arr) {
  n <- length(arr)
  
  # Traverse through all array elements
  for (i in 1:(n - 1)) {
    # Find the minimum element in remaining unsorted array
    min_idx <- i
    
    for (j in (i + 1):n) {
      if (arr[j] < arr[min_idx]) {
        min_idx <- j
      }
    }
    
    # Swap the found minimum element with the first element
    if (min_idx != i) {
      temp <- arr[i]
      arr[i] <- arr[min_idx]
      arr[min_idx] <- temp
    }
  }
  
  return(arr)
}

# Example usage
# Create an example array
numbers <- c(64, 34, 25, 12, 22, 11, 90)

# Display original array
cat("Original array:", paste(numbers, collapse = " "), "\n")

# Sort the array using selection sort
sorted_numbers <- selection_sort(numbers)

# Display sorted array
cat("Sorted array: ", paste(sorted_numbers, collapse = " "), "\n")
```

## Output:
```
Original array: 64 34 25 12 22 11 90 
Sorted array:  11 12 22 25 34 64 90 
```

## How it works:

1. **Find minimum**: In each iteration, find the smallest element in the remaining unsorted portion
2. **Swap**: Place the minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in place

The algorithm works by repeatedly selecting the smallest element from the unsorted portion and placing it in its correct position, gradually building the sorted array from left to right.

