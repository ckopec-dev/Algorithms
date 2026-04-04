# Merge Sort Algorithm in R

Here's an implementation of the merge sort algorithm in R:

```r
# Merge Sort Implementation in R

merge_sort <- function(arr) {
  # Base case: if array has 1 or 0 elements, it's already sorted
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Split the array into two halves
  mid <- length(arr) %/% 2
  left <- arr[1:mid]
  right <- arr[(mid + 1):length(arr)]
  
  # Recursively sort both halves
  left_sorted <- merge_sort(left)
  right_sorted <- merge_sort(right)
  
  # Merge the sorted halves
  return(merge_arrays(left_sorted, right_sorted))
}

# Helper function to merge two sorted arrays
merge_arrays <- function(left, right) {
  result <- c()
  i <- j <- 1
  
  # Compare elements from both arrays and merge in sorted order
  while (i <= length(left) && j <= length(right)) {
    if (left[i] <= right[j]) {
      result <- c(result, left[i])
      i <- i + 1
    } else {
      result <- c(result, right[j])
      j <- j + 1
    }
  }
  
  # Add remaining elements from left array (if any)
  while (i <= length(left)) {
    result <- c(result, left[i])
    i <- i + 1
  }
  
  # Add remaining elements from right array (if any)
  while (j <= length(right)) {
    result <- c(result, right[j])
    j <- j + 1
  }
  
  return(result)
}

# Example usage
numbers <- c(64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42)
print("Original array:")
print(numbers)

sorted_numbers <- merge_sort(numbers)
print("Sorted array:")
print(sorted_numbers)
```

**Output:**
```
[1] "Original array:"
[1] 64 34 25 12 22 11 90 88 76 50 42
[1] "Sorted array:"
[1] 11 12 22 25 34 42 50 64 76 88 90
```

## How it works:

1. **Divide**: Split the array into two halves recursively until each subarray has 1 or 0 elements
2. **Conquer**: Sort the subarrays (base case handles single elements)
3. **Combine**: Merge the sorted subarrays back together in sorted order

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(n) - requires additional space for the temporary arrays during merging

The algorithm is stable (maintains relative order of equal elements) and works efficiently for large datasets.

