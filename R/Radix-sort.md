# Radix Sort Algorithm in R

Here's an implementation of the Radix Sort algorithm in R:

```r
# Radix Sort Implementation in R
radix_sort <- function(arr) {
  # Handle edge cases
  if (length(arr) <= 1) return(arr)
  
  # Find maximum number to know number of digits
  max_num <- max(arr)
  
  # Do counting sort for every digit
  # exp is 10^i where i is current digit number
  exp <- 1
  while (max_num / exp > 0) {
    arr <- counting_sort_by_digit(arr, exp)
    exp <- exp * 10
  }
  
  return(arr)
}

# Counting sort for a specific digit
counting_sort_by_digit <- function(arr, exp) {
  n <- length(arr)
  output <- rep(0, n)
  count <- rep(0, 10)  # Count array for digits 0-9
  
  # Store count of occurrences of each digit
  for (i in 1:n) {
    digit <- (arr[i] %/% exp) %% 10
    count[digit + 1] <- count[digit + 1] + 1
  }
  
  # Change count[i] to actual position
  for (i in 2:10) {
    count[i] <- count[i] + count[i - 1]
  }
  
  # Build the output array
  for (i in n:1) {
    digit <- (arr[i] %/% exp) %% 10
    output[count[digit + 1]] <- arr[i]
    count[digit + 1] <- count[digit + 1] - 1
  }
  
  return(output)
}

# Example usage
# Test with sample data
numbers <- c(170, 45, 75, 90, 2, 802, 24, 66)
cat("Original array:", paste(numbers, collapse = " "), "\n")

sorted_numbers <- radix_sort(numbers)
cat("Sorted array:  ", paste(sorted_numbers, collapse = " "), "\n")

# Test with another example
numbers2 <- c(329, 457, 657, 839, 436, 720, 355)
cat("\nOriginal array:", paste(numbers2, collapse = " "), "\n")

sorted_numbers2 <- radix_sort(numbers2)
cat("Sorted array:  ", paste(sorted_numbers2, collapse = " "), "\n")
```

## Output:
```
Original array: 170 45 75 90 2 802 24 66
Sorted array:   2 24 45 66 75 90 170 802

Original array: 329 457 657 839 436 720 355
Sorted array:   329 355 436 457 657 720 839
```

## How it works:

1. **Find Maximum**: Determine the maximum number to know how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit (units place), sort the array using counting sort
3. **Repeat**: Move to the next significant digit (tens, hundreds, etc.) and repeat the process
4. **Counting Sort**: For each digit position, use counting sort to maintain stability

## Time Complexity:
- **Time**: O(d × n) where d is the number of digits and n is the number of elements
- **Space**: O(n + k) where k is the range of digits (0-9)

## Key Features:
- **Stable**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use comparison operations
- **Efficient**: Particularly good for sorting integers with limited range
- **In-place**: The counting sort helper function is not truly in-place but uses additional space proportional to input size

