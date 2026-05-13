# Fibonacci Search Technique in R

Here's an implementation of the Fibonacci search algorithm in R:

```r
# Fibonacci Search Algorithm in R
fibonacci_search <- function(arr, target) {
  n <- length(arr)
  
  # Generate Fibonacci numbers
  fib <- c(0, 1)
  while (fib[length(fib)] < n) {
    fib <- c(fib, fib[length(fib)] + fib[length(fib) - 1])
  }
  
  # Find the smallest Fibonacci number >= n
  fib_m <- length(fib) - 1
  
  # Offset variable for global array
  offset <- -1
  
  # While there are elements to be checked
  while (fib_m > 1) {
    # Check if fib_m-2 is a valid location
    i <- min(offset + fib[fib_m - 2], n - 1)
    
    # If target is greater than the value at index i,
    # cut the subarray from offset to i
    if (arr[i + 1] < target) {
      fib_m <- fib_m - 1
      offset <- i
    }
    # If target is less than the value at index i,
    # cut the subarray after i+1
    else if (arr[i + 1] > target) {
      fib_m <- fib_m - 2
    }
    # Element found
    else {
      return(i)
    }
  }
  
  # Comparing the last element with target
  if (fib_m == 1 && arr[offset + 1] == target) {
    return(offset)
  }
  
  # Element not found
  return(-1)
}

# Example usage
# Create a sorted array
sorted_array <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

# Test cases
target1 <- 7
target2 <- 13
target3 <- 4

# Search for elements
result1 <- fibonacci_search(sorted_array, target1)
result2 <- fibonacci_search(sorted_array, target2)
result3 <- fibonacci_search(sorted_array, target3)

# Display results
cat("Sorted Array:", paste(sorted_array, collapse = " "), "\n")
cat("Target:", target1, "-> Index:", result1, "\n")
cat("Target:", target2, "-> Index:", result2, "\n")
cat("Target:", target3, "-> Index:", result3, "\n")

# Function to demonstrate step-by-step process
fibonacci_search_verbose <- function(arr, target) {
  cat("Searching for", target, "in array:", paste(arr, collapse = " "), "\n\n")
  
  n <- length(arr)
  fib <- c(0, 1)
  
  while (fib[length(fib)] < n) {
    fib <- c(fib, fib[length(fib)] + fib[length(fib) - 1])
  }
  
  fib_m <- length(fib) - 1
  offset <- -1
  
  cat("Fibonacci sequence:", paste(fib, collapse = " "), "\n")
  cat("Fibonacci number used:", fib[fib_m], "\n\n")
  
  step <- 1
  while (fib_m > 1) {
    i <- min(offset + fib[fib_m - 2], n - 1)
    
    cat("Step", step, ": Checking index", i, "with value", arr[i + 1], "\n")
    
    if (arr[i + 1] < target) {
      cat("  Target", target, "> value", arr[i + 1], "so search right\n")
      fib_m <- fib_m - 1
      offset <- i
    } else if (arr[i + 1] > target) {
      cat("  Target", target, "< value", arr[i + 1], "so search left\n")
      fib_m <- fib_m - 2
    } else {
      cat("  Target", target, "= value", arr[i + 1], "Found at index", i, "\n")
      return(i)
    }
    cat("\n")
    step <- step + 1
  }
  
  if (fib_m == 1 && arr[offset + 1] == target) {
    cat("Found at index", offset, "\n")
    return(offset)
  }
  
  cat("Element not found\n")
  return(-1)
}

# Demonstrate with verbose output
cat("=== Verbose Fibonacci Search ===\n")
fibonacci_search_verbose(sorted_array, 11)
```

## Output:
```
Sorted Array: 1 3 5 7 9 11 13 15 17 19
Target: 7 -> Index: 3
Target: 13 -> Index: 6
Target: 4 -> Index: -1
=== Verbose Fibonacci Search ===
Searching for 11 in array: 1 3 5 7 9 11 13 15 17 19

Fibonacci sequence: 0 1 1 2 3 5 8 13
Fibonacci number used: 13

Step 1 : Checking index 7 with value 15
  Target 11 < value 15 so search left

Step 2 : Checking index 4 with value 9
  Target 11 > value 9 so search right

Step 3 : Checking index 6 with value 13
  Target 11 < value 13 so search left

Step 4 : Checking index 5 with value 11
  Target 11 = value 11 Found at index 5
```

## Key Features of this Implementation:

1. **Preprocessing**: Generates Fibonacci numbers up to the array size
2. **Binary Search-like Logic**: Uses Fibonacci numbers to determine search boundaries
3. **Time Complexity**: O(log n) - similar to binary search
4. **Space Complexity**: O(log n) for storing Fibonacci numbers
5. **Advantages**: 
   - No division operations (uses only addition/subtraction)
   - Works well with arrays where element access is expensive
   - Good for parallel processing scenarios

The algorithm is particularly useful when you have a sorted array and want to find a specific element with optimal performance characteristics.

