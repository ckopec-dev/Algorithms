# Patience Sorting Algorithm in R

Here's an implementation of the patience sorting algorithm in R:

```r
# Patience Sorting Algorithm in R

patience_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Create piles (lists) to simulate the patience sorting process
  piles <- list()
  
  # Place each element in the appropriate pile
  for (element in arr) {
    placed <- FALSE
    
    # Try to place element in existing piles
    for (i in seq_along(piles)) {
      if (length(piles[[i]]) == 0 || piles[[i]][length(piles[[i]])] >= element) {
        piles[[i]] <- c(piles[[i]], element)
        placed <- TRUE
        break
      }
    }
    
    # If element couldn't be placed in any existing pile, create new pile
    if (!placed) {
      piles[[length(piles) + 1]] <- list(element)
    }
  }
  
  # Extract elements in sorted order by repeatedly taking the smallest
  # element from the tops of all piles
  result <- c()
  
  while (length(piles) > 0) {
    # Find the minimum element among all pile tops
    min_val <- Inf
    min_idx <- 0
    
    for (i in seq_along(piles)) {
      if (length(piles[[i]]) > 0 && piles[[i]][length(piles[[i]])] < min_val) {
        min_val <- piles[[i]][length(piles[[i]])]
        min_idx <- i
      }
    }
    
    # Add minimum element to result
    result <- c(result, min_val)
    
    # Remove the element from its pile
    piles[[min_idx]] <- piles[[min_idx]][-length(piles[[min_idx]])]
    
    # Remove empty piles
    piles <- piles[sapply(piles, length) > 0]
  }
  
  return(result)
}

# Example usage
# Test with different arrays
test_array1 <- c(4, 2, 6, 1, 3, 5)
cat("Original array:", paste(test_array1, collapse = " "), "\n")
sorted1 <- patience_sort(test_array1)
cat("Sorted array:  ", paste(sorted1, collapse = " "), "\n\n")

test_array2 <- c(9, 3, 1, 5, 8, 2, 7, 4, 6)
cat("Original array:", paste(test_array2, collapse = " "), "\n")
sorted2 <- patience_sort(test_array2)
cat("Sorted array:  ", paste(sorted2, collapse = " "), "\n\n")

test_array3 <- c(10, 5, 8, 3, 1, 9, 2, 7, 4, 6)
cat("Original array:", paste(test_array3, collapse = " "), "\n")
sorted3 <- patience_sort(test_array3)
cat("Sorted array:  ", paste(sorted3, collapse = " "), "\n")
```

## Output:
```
Original array: 4 2 6 1 3 5
Sorted array:   1 2 3 4 5 6

Original array: 9 3 1 5 8 2 7 4 6
Sorted array:   1 2 3 4 5 6 7 8 9

Original array: 10 5 8 3 1 9 2 7 4 6
Sorted array:   1 2 3 4 5 6 7 8 9 10
```

## How it works:

1. **Pile Creation**: Elements are placed in piles where each pile is maintained in decreasing order from top to bottom
2. **Placement Strategy**: Each new element is placed on the leftmost pile where the top element is greater than or equal to the new element
3. **Sorting**: Elements are extracted one by one from the tops of piles, always choosing the smallest among the top elements of all non-empty piles

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)  
- **Worst Case**: O(n log n)

## Space Complexity:
- O(n) for the piles storage

This implementation demonstrates the patience sorting algorithm's unique approach of simulating the card game "patience" (solitaire) to achieve sorting.

