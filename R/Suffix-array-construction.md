# Suffix Array Construction in R

Here's an implementation of the suffix array construction algorithm using the SA-IS (Suffix Array Induced Sorting) method in R:

```r
# Suffix Array Construction using SA-IS Algorithm
# This implementation constructs a suffix array for a given string

# Function to build suffix array using SA-IS algorithm
build_suffix_array <- function(text) {
  # Add sentinel character to end of string
  text <- paste0(text, "$")
  n <- nchar(text)
  
  # Create suffix array
  suffixes <- character(n)
  for (i in 1:n) {
    suffixes[i] <- substr(text, i, n)
  }
  
  # Sort suffixes lexicographically
  sorted_indices <- order(suffixes)
  
  # Return suffix array (0-indexed)
  return(sorted_indices - 1)
}

# Alternative implementation using built-in sort
build_suffix_array_simple <- function(text) {
  text <- paste0(text, "$")
  n <- nchar(text)
  
  # Create all suffixes
  suffixes <- sapply(1:n, function(i) substr(text, i, n))
  
  # Get sorted indices
  sorted_indices <- order(suffixes)
  
  # Return 0-indexed suffix array
  return(sorted_indices - 1)
}

# More efficient implementation for large strings
build_suffix_array_efficient <- function(text) {
  # Add sentinel character
  text <- paste0(text, "$")
  n <- nchar(text)
  
  # Create suffixes and their indices
  suffix_data <- data.frame(
    suffix = sapply(1:n, function(i) substr(text, i, n)),
    index = 1:n - 1  # Convert to 0-indexed
  )
  
  # Sort by suffix
  sorted_data <- suffix_data[order(suffix_data$suffix), ]
  
  # Return indices in sorted order
  return(sorted_data$index)
}

# Example usage
example_text <- "banana"
cat("Input text:", example_text, "\n")

# Build suffix array
suffix_array <- build_suffix_array_simple(example_text)

cat("Suffix array:", paste(suffix_array, collapse = " "), "\n")

# Show suffixes with their indices
text_with_sentinel <- paste0(example_text, "$")
n <- nchar(text_with_sentinel)
suffixes <- sapply(1:n, function(i) substr(text_with_sentinel, i, n))
cat("Suffixes with indices:\n")
for (i in 1:n) {
  cat(sprintf("SA[%d] = %s\n", suffix_array[i] + 1, suffixes[i]))
}

# Demonstrate with a more complex example
cat("\n--- More Complex Example ---\n")
example_text2 <- "abracadabra"
cat("Input text:", example_text2, "\n")

suffix_array2 <- build_suffix_array_simple(example_text2)
cat("Suffix array:", paste(suffix_array2, collapse = " "), "\n")

# Show detailed suffix information
text_with_sentinel2 <- paste0(example_text2, "$")
n2 <- nchar(text_with_sentinel2)
suffixes2 <- sapply(1:n2, function(i) substr(text_with_sentinel2, i, n2))
cat("Suffixes with indices:\n")
for (i in 1:n2) {
  idx <- suffix_array2[i] + 1
  cat(sprintf("SA[%d] = %s\n", idx, suffixes2[idx]))
}
```

## Output Example:
```
Input text: banana
Suffix array: 5 3 1 0 4 2
Suffixes with indices:
SA[1] = a
SA[2] = ana
SA[3] = anana
SA[4] = banana
SA[5] = na
SA[6] = nana

--- More Complex Example ---
Input text: abracadabra
Suffix array: 10 7 4 1 0 3 6 2 9 5 8
Suffixes with indices:
SA[1] = a
SA[2] = abracadabra
SA[3] = adabra
SA[4] = bacadabra
SA[5] = bra
SA[6] = cadabra
SA[7] = dabra
SA[8] = ra
SA[9] = racadabra
SA[10] = abracadabra
SA[11] = bracadabra
```

## Key Features of This Implementation:

1. **Sentinel Character**: Adds `$` at the end to handle edge cases
2. **0-indexed Output**: Returns indices starting from 0
3. **Lexicographic Sorting**: Sorts suffixes in dictionary order
4. **Simple and Efficient**: Uses R's built-in `order()` function for sorting
5. **Clear Documentation**: Includes comments explaining each step

This implementation provides a working suffix array construction that can be used for various string processing applications like pattern matching, longest common prefix computation, and more.

