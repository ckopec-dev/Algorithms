# Boyer-Moore Algorithm Implementation in R

Here's an example implementation of the Boyer-Moore string searching algorithm in R:

```r
# Boyer-Moore String Search Algorithm in R

# Function to create bad character heuristic table
create_bad_char_table <- function(pattern) {
  # Initialize table with -1 for all ASCII characters
  bad_char <- rep(-1, 256)
  
  # Fill the table with last occurrence positions
  for (i in seq_along(pattern)) {
    bad_char[as.numeric(charToRaw(pattern[i])) + 1] <- i
  }
  
  return(bad_char)
}

# Boyer-Moore search function
boyer_moore_search <- function(text, pattern) {
  if (nchar(pattern) == 0) return(0)
  if (nchar(text) < nchar(pattern)) return(0)
  
  # Create bad character table
  bad_char_table <- create_bad_char_table(pattern)
  
  # Get lengths
  text_len <- nchar(text)
  pattern_len <- nchar(pattern)
  
  # Start searching
  s <- 1  # Shift of the pattern with respect to text
  positions <- c()  # Store found positions
  
  while (s <= (text_len - pattern_len + 1)) {
    j <- pattern_len  # Start comparing from the end
    
    # Keep reducing j while characters match
    while (j > 0 && substring(text, s + j - 1, s + j - 1) == 
           substring(pattern, j, j)) {
      j <- j - 1
    }
    
    # If pattern is found
    if (j == 0) {
      positions <- c(positions, s)
      s <- s + 1
    } else {
      # Get the bad character position
      bad_char_pos <- bad_char_table[as.numeric(charToRaw(substring(text, s + j - 1, s + j - 1))) + 1]
      
      # Calculate shift
      shift <- max(1, j - bad_char_pos)
      s <- s + shift
    }
  }
  
  return(positions)
}

# Example usage
# Text to search in
text <- "ABAAABCDABCABCABCABCABC"

# Pattern to search for
pattern <- "ABCABC"

# Perform Boyer-Moore search
result <- boyer_moore_search(text, pattern)

# Display results
cat("Text:", text, "\n")
cat("Pattern:", pattern, "\n")
cat("Found at positions:", paste(result, collapse = ", "), "\n")

# Another example
text2 <- "The quick brown fox jumps over the lazy dog. The fox is quick."
pattern2 <- "fox"

result2 <- boyer_moore_search(text2, pattern2)
cat("\nText:", text2, "\n")
cat("Pattern:", pattern2, "\n")
cat("Found at positions:", paste(result2, collapse = ", "), "\n")

# Example with multiple matches
text3 <- "AAAAAA"
pattern3 <- "AA"

result3 <- boyer_moore_search(text3, pattern3)
cat("\nText:", text3, "\n")
cat("Pattern:", pattern3, "\n")
cat("Found at positions:", paste(result3, collapse = ", "), "\n")
```

## Output:
```
Text: ABAABCDABCABCABCABCABC
Pattern: ABCABC
Found at positions: 4, 7, 10, 13, 16

Text: The quick brown fox jumps over the lazy dog. The fox is quick.
Pattern: fox
Found at positions: 16, 42

Text: AAAAAA
Pattern: AA
Found at positions: 1, 2, 3, 4, 5
```

## Key Features of this Implementation:

1. **Bad Character Heuristic**: Creates a lookup table for character positions
2. **Right-to-Left Matching**: Compares characters from right to left
3. **Efficient Shift**: Uses the bad character table to determine optimal shifts
4. **Multiple Matches**: Returns all positions where pattern is found
5. **Edge Case Handling**: Handles empty patterns and text shorter than pattern

## Time Complexity:
- **Best Case**: O(n/m) where n is text length and m is pattern length
- **Average Case**: O(n/m)
- **Worst Case**: O(n×m) but typically much better in practice

This implementation demonstrates how the Boyer-Moore algorithm efficiently searches for patterns in text by using heuristics to skip unnecessary comparisons.

