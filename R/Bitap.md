# Bitap Algorithm Implementation in R

The Bitap algorithm (also known as the shift-or algorithm) is a string searching algorithm that uses bit operations to efficiently find occurrences of a pattern within a text.

```r
# Bitap Algorithm Implementation in R

# Function to implement Bitap algorithm
bitap_search <- function(text, pattern) {
  if (nchar(pattern) == 0) return(list(positions = integer(0), found = FALSE))
  
  # Create bitmasks for each character in the pattern
  # Using ASCII values for character mapping
  alphabet_size <- 256  # ASCII characters
  bitmasks <- rep(0, alphabet_size)
  
  # Set bitmasks for pattern characters
  for (i in 1:nchar(pattern)) {
    char <- substr(pattern, i, i)
    char_code <- utf8ToInt(char)
    bitmasks[char_code] <- bitwOr(bitmasks[char_code], bitwShiftL(1, nchar(pattern) - i))
  }
  
  # Initialize state
  state <- 0
  
  # Positions where pattern is found
  positions <- integer(0)
  
  # Search through the text
  for (i in 1:nchar(text)) {
    char <- substr(text, i, i)
    char_code <- utf8ToInt(char)
    
    # Update state using bit operations
    state <- bitwOr(bitwShiftR(state, 1), bitmasks[char_code])
    
    # Check if pattern is found (when the leftmost bit is set)
    if (bitwAnd(state, bitwShiftL(1, nchar(pattern) - 1)) != 0) {
      positions <- c(positions, i - nchar(pattern) + 1)
    }
  }
  
  return(list(positions = positions, found = length(positions) > 0))
}

# Alternative simpler implementation using base R
bitap_simple <- function(text, pattern) {
  if (nchar(pattern) == 0) return(list(positions = integer(0), found = FALSE))
  
  # Simple bit manipulation approach
  n <- nchar(pattern)
  m <- nchar(text)
  
  # Create a simple bitmask approach
  positions <- integer(0)
  
  # Check each possible position
  for (i in 1:(m - n + 1)) {
    substring_text <- substr(text, i, i + n - 1)
    if (substring_text == pattern) {
      positions <- c(positions, i)
    }
  }
  
  return(list(positions = positions, found = length(positions) > 0))
}

# Example usage
cat("=== Bitap Algorithm Examples ===\n\n")

# Example 1: Basic pattern matching
text1 <- "This is a sample text for pattern matching"
pattern1 <- "sample"
result1 <- bitap_search(text1, pattern1)
cat("Text:", text1, "\n")
cat("Pattern:", pattern1, "\n")
cat("Found:", result1$found, "\n")
cat("Positions:", paste(result1$positions, collapse = ", "), "\n\n")

# Example 2: Multiple occurrences
text2 <- "abababab"
pattern2 <- "abab"
result2 <- bitap_search(text2, pattern2)
cat("Text:", text2, "\n")
cat("Pattern:", pattern2, "\n")
cat("Found:", result2$found, "\n")
cat("Positions:", paste(result2$positions, collapse = ", "), "\n\n")

# Example 3: No match
text3 <- "hello world"
pattern3 <- "xyz"
result3 <- bitap_search(text3, pattern3)
cat("Text:", text3, "\n")
cat("Pattern:", pattern3, "\n")
cat("Found:", result3$found, "\n")
cat("Positions:", paste(result3$positions, collapse = ", "), "\n\n")

# Example 4: With special characters
text4 <- "The quick brown fox jumps over the lazy dog"
pattern4 <- "brown"
result4 <- bitap_search(text4, pattern4)
cat("Text:", text4, "\n")
cat("Pattern:", pattern4, "\n")
cat("Found:", result4$found, "\n")
cat("Positions:", paste(result4$positions, collapse = ", "), "\n\n")

# Performance comparison example
cat("=== Performance Comparison ===\n")
library(microbenchmark)

text_long <- paste(rep("abcdefghijklmnopqrstuvwxyz", 1000), collapse = "")
pattern_long <- "xyz"

# Simple approach
simple_result <- microbenchmark(
  simple = bitap_simple(text_long, pattern_long),
  times = 10
)

# Bitap approach
bitap_result <- microbenchmark(
  bitap = bitap_search(text_long, pattern_long),
  times = 10
)

cat("Simple approach (10 runs):\n")
print(simple_result)
cat("\nBitap approach (10 runs):\n")
print(bitap_result)
```

## Key Features of this Implementation:

1. **Bit Operations**: Uses bitwise OR and shift operations for efficient pattern matching
2. **Character Mapping**: Maps characters to bitmasks for fast lookup
3. **Multiple Matches**: Returns all positions where the pattern is found
4. **Edge Cases**: Handles empty patterns and no matches gracefully

## How Bitap Works:

1. **Preprocessing**: Create bitmasks for each character in the pattern
2. **State Tracking**: Maintain a state variable that represents the current match status
3. **Bit Shifting**: Shift the state right and update with character bitmasks
4. **Match Detection**: Check if the leftmost bit is set to indicate a complete match

## Time Complexity:
- **Preprocessing**: O(m) where m is the pattern length
- **Search**: O(n) where n is the text length
- **Space**: O(k) where k is the alphabet size

This implementation demonstrates the efficiency of bit-based string matching algorithms, particularly useful for large texts and multiple pattern searches.

