# Binary Search Algorithm in R

Here's an implementation of the binary search algorithm in R:

```r
# Binary Search Function
binary_search <- function(arr, target) {
  left <- 1
  right <- length(arr)
  
  while (left <= right) {
    mid <- left + (right - left) %/% 2
    
    if (arr[mid] == target) {
      return(mid)  # Return the index if found
    } else if (arr[mid] < target) {
      left <- mid + 1  # Search in the right half
    } else {
      right <- mid - 1  # Search in the left half
    }
  }
  
  return(-1)  # Return -1 if not found
}

# Example usage
# Create a sorted array
sorted_array <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

# Test cases
cat("Sorted array:", sorted_array, "\n\n")

# Search for existing element
result1 <- binary_search(sorted_array, 7)
cat("Searching for 7:", result1, "\n")  # Should return 4

# Search for non-existing element
result2 <- binary_search(sorted_array, 6)
cat("Searching for 6:", result2, "\n")  # Should return -1

# Search for first element
result3 <- binary_search(sorted_array, 1)
cat("Searching for 1:", result3, "\n")  # Should return 1

# Search for last element
result4 <- binary_search(sorted_array, 19)
cat("Searching for 19:", result4, "\n")  # Should return 10
```

## Output:
```
Sorted array: 1 3 5 7 9 11 13 15 17 19

Searching for 7: 4
Searching for 6: -1
Searching for 1: 1
Searching for 19: 10
```

## How it works:

1. **Initialize pointers**: Set `left` to 1 and `right` to the length of the array
2. **Loop while** `left <= right`
3. **Calculate middle**: `mid <- left + (right - left) %/% 2` (avoiding integer overflow)
4. **Compare**: 
   - If `arr[mid] == target`, return the index
   - If `arr[mid] < target`, search the right half
   - If `arr[mid] > target`, search the left half
5. **Return -1** if element is not found

## Time Complexity: O(log n)
## Space Complexity: O(1)

The algorithm requires the input array to be sorted for correct operation.

