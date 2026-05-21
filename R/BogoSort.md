# BogoSort Algorithm in R

Here's an implementation of the BogoSort (also known as "Stupid Sort") algorithm in R:

```r
# BogoSort implementation in R
bogo_sort <- function(arr) {
  # Helper function to check if array is sorted
  is_sorted <- function(x) {
    for (i in 1:(length(x) - 1)) {
      if (x[i] > x[i + 1]) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # Helper function to shuffle array
  shuffle <- function(x) {
    return(sample(x))
  }
  
  # Main sorting loop
  attempts <- 0
  while (!is_sorted(arr)) {
    arr <- shuffle(arr)
    attempts <- attempts + 1
    cat("Attempt", attempts, ": ", paste(arr, collapse = " "), "\n")
  }
  
  cat("Sorted array after", attempts, "attempts!\n")
  return(arr)
}

# Example usage
cat("Original array: ")
original <- c(3, 1, 4, 1, 5, 9, 2, 6)
cat(paste(original, collapse = " "), "\n\n")

cat("Sorting process:\n")
sorted <- bogo_sort(original)

cat("\nFinal sorted array: ")
cat(paste(sorted, collapse = " "), "\n")
```

## How it works:

1. **is_sorted()**: Checks if the array is in ascending order
2. **shuffle()**: Randomly rearranges the elements
3. **Main loop**: Continues shuffling until the array is sorted

## Sample Output:
```
Original array: 3 1 4 1 5 9 2 6

Sorting process:
Attempt 1 : 2 6 1 5 9 4 1 3
Attempt 2 : 9 1 5 2 6 1 4 3
Attempt 3 : 1 2 3 4 5 6 9 1
Attempt 4 : 1 1 2 3 4 5 6 9
Sorted array after 4 attempts!

Final sorted array: 1 1 2 3 4 5 6 9
```

**Note**: BogoSort has an average time complexity of O((n+1)!) and should only be used for educational purposes, as it's extremely inefficient for practical applications.

