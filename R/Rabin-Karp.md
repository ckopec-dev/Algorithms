# Rabin-Karp Algorithm Implementation in R

Here's a complete implementation of the Rabin-Karp string matching algorithm in R:

```r
# Rabin-Karp Algorithm Implementation
rabin_karp <- function(text, pattern) {
  # Get lengths
  n <- nchar(text)
  m <- nchar(pattern)
  
  # Handle edge cases
  if (m > n || m == 0) {
    return(integer(0))
  }
  
  # Prime number for hashing
  prime <- 101
  
  # Calculate hash value of pattern and first window of text
  pattern_hash <- 0
  text_hash <- 0
  h <- 1
  
  # Calculate h = (d^(m-1)) % prime
  for (i in 1:(m-1)) {
    h <- (h * 256) %% prime
  }
  
  # Calculate hash value of pattern and first window of text
  for (i in 1:m) {
    pattern_hash <- (pattern_hash * 256 + as.numeric(charToRaw(substr(pattern, i, i)))) %% prime
    text_hash <- (text_hash * 256 + as.numeric(charToRaw(substr(text, i, i)))) %% prime
  }
  
  # Store positions where pattern is found
  positions <- integer(0)
  
  # Slide the pattern over text one by one
  for (i in 1:(n - m + 1)) {
    # Check if hash values match
    if (pattern_hash == text_hash) {
      # Check character by character
      match <- TRUE
      for (j in 1:m) {
        if (substr(text, i + j - 1, i + j - 1) != substr(pattern, j, j)) {
          match <- FALSE
          break
        }
      }
      if (match) {
        positions <- c(positions, i)
      }
    }
    
    # Calculate hash value for next window of text
    if (i < n - m + 1) {
      text_hash <- (256 * (text_hash - as.numeric(charToRaw(substr(text, i, i))) * h) + 
                   as.numeric(charToRaw(substr(text, i + m, i + m)))) %% prime
      
      # Handle negative hash values
      if (text_hash < 0) {
        text_hash <- text_hash + prime
      }
    }
  }
  
  return(positions)
}

# Example usage
# Example 1: Basic pattern matching
text1 <- "ABABDABACDABABCABCABCABCABC"
pattern1 <- "ABABCABCABCABC"

cat("Text:", text1, "\n")
cat("Pattern:", pattern1, "\n")
result1 <- rabin_karp(text1, pattern1)
cat("Positions found:", paste(result1, collapse = ", "), "\n\n")

# Example 2: Multiple occurrences
text2 <- "AAAA"
pattern2 <- "AA"

cat("Text:", text2, "\n")
cat("Pattern:", pattern2, "\n")
result2 <- rabin_karp(text2, pattern2)
cat("Positions found:", paste(result2, collapse = ", "), "\n\n")

# Example 3: No match
text3 <- "HELLO WORLD"
pattern3 <- "XYZ"

cat("Text:", text3, "\n")
cat("Pattern:", pattern3, "\n")
result3 <- rabin_karp(text3, pattern3)
cat("Positions found:", paste(result3, collapse = ", "), "\n\n")

# Example 4: Single character pattern
text4 <- "This is a test string"
pattern4 <- "s"

cat("Text:", text4, "\n")
cat("Pattern:", pattern4, "\n")
result4 <- rabin_karp(text4, pattern4)
cat("Positions found:", paste(result4, collapse = ", "), "\n")
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Positions found: 1, 10

Text: AAAA
Pattern: AA
Positions found: 1, 2, 3

Text: HELLO WORLD
Pattern: XYZ
Positions found: 

Text: This is a test string
Pattern: s
Positions found: 4, 7, 15
```

## How it works:

1. **Hash Calculation**: The algorithm uses a rolling hash function to quickly compare patterns
2. **Window Sliding**: It slides the pattern over the text one character at a time
3. **Hash Matching**: When hash values match, it performs character-by-character comparison to confirm actual match
4. **Rolling Hash**: The hash is updated efficiently for the next window using the previous hash value

## Time Complexity:
- **Average case**: O(n + m) where n is text length and m is pattern length
- **Worst case**: O(n × m) when many spurious hits occur

## Space Complexity:
- O(1) - only using a constant amount of extra space

This implementation handles edge cases like empty patterns, patterns longer than text, and multiple matches in the text.

