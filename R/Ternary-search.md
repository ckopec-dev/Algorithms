# Ternary Search Algorithm in R

Here's an implementation of the ternary search algorithm in R:

```r
# Ternary Search Function
ternary_search <- function(arr, target) {
  left <- 1
  right <- length(arr)
  
  while (left <= right) {
    # Calculate two mid points
    mid1 <- left + (right - left) %/% 3
    mid2 <- right - (right - left) %/% 3
    
    # If target is at either mid point
    if (arr[mid1] == target) {
      return(mid1)
    }
    if (arr[mid2] == target) {
      return(mid2)
    }
    
    # Determine which segment to search
    if (target < arr[mid1]) {
      # Target is in the left segment
      right <- mid1 - 1
    } else if (target > arr[mid2]) {
      # Target is in the right segment
      left <- mid2 + 1
    } else {
      # Target is in the middle segment
      left <- mid1 + 1
      right <- mid2 - 1
    }
  }
  
  # Target not found
  return(-1)
}

# Example usage
# Create a sorted array
sorted_array <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

# Test cases
cat("Array:", sorted_array, "\n\n")

# Search for existing elements
result1 <- ternary_search(sorted_array, 7)
cat("Searching for 7:", ifelse(result1 != -1, paste("Found at index", result1), "Not found"), "\n")

result2 <- ternary_search(sorted_array, 15)
cat("Searching for 15:", ifelse(result2 != -1, paste("Found at index", result2), "Not found"), "\n")

# Search for non-existing element
result3 <- ternary_search(sorted_array, 8)
cat("Searching for 8:", ifelse(result3 != -1, paste("Found at index", result3), "Not found"), "\n")

# Search for first element
result4 <- ternary_search(sorted_array, 1)
cat("Searching for 1:", ifelse(result4 != -1, paste("Found at index", result4), "Not found"), "\n")
```

## Output:
```
Array: 1 3 5 7 9 11 13 15 17 19

Searching for 7: Found at index 4
Searching for 15: Found at index 8
Searching for 8: Not found
Searching for 1: Found at index 1
```

## How it works:

1. **Divide**: Instead of dividing the array into two parts (like binary search), ternary search divides it into three parts
2. **Two mid points**: Calculate `mid1` and `mid2` to divide the array into three segments
3. **Compare**: Check if the target equals either of the mid points
4. **Narrow search**: Based on comparison, eliminate one-third of the array and continue searching in the remaining segments
5. **Repeat**: Continue until target is found or search space is exhausted

## Time Complexity:
- **Best case**: O(1)
- **Average case**: O(log₃ n)
- **Worst case**: O(log₃ n)

## Space Complexity:
- O(1) for iterative implementation
- O(log₃ n) for recursive implementation

The ternary search algorithm is particularly useful when the array is already sorted and you want to minimize the number of comparisons, though in practice binary search is often preferred due to simpler implementation and better cache performance.

