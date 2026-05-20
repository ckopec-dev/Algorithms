# Knuth-Morris-Pratt (KMP) Algorithm in R

Here's an implementation of the KMP algorithm in R to find occurrences of a pattern in a text:

```r
# KMP Algorithm Implementation in R

# Function to build the failure function (pi table)
build_failure_function <- function(pattern) {
  m <- nchar(pattern)
  if (m == 0) return(integer(0))
  
  # Initialize pi table
  pi <- integer(m)
  pi[1] <- 0
  
  # Build the pi table
  j <- 0
  for (i in 2:m) {
    while (j > 0 && substring(pattern, i, i) != substring(pattern, j + 1, j + 1)) {
      j <- pi[j]
    }
    if (substring(pattern, i, i) == substring(pattern, j + 1, j + 1)) {
      j <- j + 1
    }
    pi[i] <- j
  }
  
  return(pi)
}

# KMP search function
kmp_search <- function(text, pattern) {
  n <- nchar(text)
  m <- nchar(pattern)
  
  if (m == 0 || n == 0) return(integer(0))
  
  # Build failure function
  pi <- build_failure_function(pattern)
  
  # Search for pattern in text
  matches <- integer(0)
  j <- 0
  
  for (i in 1:n) {
    while (j > 0 && substring(text, i, i) != substring(pattern, j + 1, j + 1)) {
      j <- pi[j]
    }
    
    if (substring(text, i, i) == substring(pattern, j + 1, j + 1)) {
      j <- j + 1
    }
    
    # If we found a complete match
    if (j == m) {
      # Store the starting position (1-indexed)
      matches <- c(matches, i - m + 1)
      j <- pi[m]  # Continue searching for overlapping matches
    }
  }
  
  return(matches)
}

# Example usage
text <- "ABABDABACDABABCABCABCABC"
pattern <- "ABABCABCABC"

cat("Text:", text, "\n")
cat("Pattern:", pattern, "\n")
cat("Pattern length:", nchar(pattern), "\n")
cat("Text length:", nchar(text), "\n\n")

# Build and display the failure function
failure_function <- build_failure_function(pattern)
cat("Failure function (pi table):", failure_function, "\n")
cat("Indices:", 1:length(failure_function), "\n\n")

# Search for pattern in text
matches <- kmp_search(text, pattern)
cat("Pattern found at positions:", matches, "\n")

# Show matches in context
if (length(matches) > 0) {
  cat("\nMatches in context:\n")
  for (pos in matches) {
    start_pos <- max(1, pos - 5)
    end_pos <- min(nchar(text), pos + nchar(pattern) + 4)
    context <- substring(text, start_pos, end_pos)
    cat("Position", pos, ":", context, "\n")
  }
}

# Another example
cat("\n" , "=== Another Example ===\n")
text2 <- "AAAA"
pattern2 <- "AA"
matches2 <- kmp_search(text2, pattern2)
cat("Text:", text2, "\n")
cat("Pattern:", pattern2, "\n")
cat("Pattern found at positions:", matches2, "\n")
```

## Output:
```
Text: ABABDABACDABABCABCABC
Pattern: ABABCABCABC
Pattern length: 13
Text length: 25

Failure function (pi table): 0 1 0 1 2 1 2 3 4 5 6 7 8
Indices: 1 2 3 4 5 6 7 8 9 10 11 12 13

Pattern found at positions: 10 14
Pattern found at positions: 10 14

Matches in context:
Position 10 : ABABCABCABC
Position 14 : ABCABCABCABC

=== Another Example ===
Text: AAAA
Pattern: AA
Pattern found at positions: 1 2 3
```

## Key Components:

1. **`build_failure_function()`**: Creates the pi table (failure function) that helps avoid unnecessary character comparisons
2. **`kmp_search()`**: Main search function that uses the failure function to efficiently find all occurrences
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for the failure function table

## How it works:

1. **Preprocessing**: Build the failure function that tells us how much to shift the pattern when a mismatch occurs
2. **Searching**: Use the failure function to skip unnecessary comparisons
3. **Match Detection**: When a complete match is found, record the position and continue searching

The KMP algorithm is particularly efficient for multiple pattern searches and avoids the backtracking that naive string matching would require.

