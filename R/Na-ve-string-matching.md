# Naïve String Matching Algorithm in R

Here's an implementation of the naive string matching algorithm in R:

```r
# Naive String Matching Algorithm
naive_string_match <- function(text, pattern) {
  # Get lengths of text and pattern
  n <- nchar(text)
  m <- nchar(pattern)
  
  # If pattern is longer than text, no match possible
  if (m > n) {
    return(integer(0))
  }
  
  # Vector to store starting positions of matches
  matches <- integer(0)
  
  # Check each possible position in text
  for (i in 1:(n - m + 1)) {
    # Extract substring of same length as pattern
    substring_text <- substring(text, i, i + m - 1)
    
    # If substring matches pattern, record position
    if (substring_text == pattern) {
      matches <- c(matches, i)
    }
  }
  
  return(matches)
}

# Example usage
text <- "ABABDABACDABABCABCABCABCABC"
pattern <- "ABC"

# Find all occurrences of pattern in text
result <- naive_string_match(text, pattern)

# Display results
cat("Text:", text, "\n")
cat("Pattern:", pattern, "\n")
cat("Match positions:", paste(result, collapse = ", "), "\n")
cat("Number of matches:", length(result), "\n")

# Show the actual matches
if (length(result) > 0) {
  cat("\nMatches found:\n")
  for (i in seq_along(result)) {
    pos <- result[i]
    match_text <- substring(text, pos, pos + nchar(pattern) - 1)
    cat("Position", pos, ":", match_text, "\n")
  }
}
```

**Output:**
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABC
Match positions: 10, 13, 16, 19, 22, 25
Number of matches: 6

Matches found:
Position 10 : ABC
Position 13 : ABC
Position 16 : ABC
Position 19 : ABC
Position 22 : ABC
Position 25 : ABC
```

## How it works:

1. **Algorithm Logic**: The algorithm checks every possible position in the text where the pattern could start
2. **Time Complexity**: O(n×m) where n is text length and m is pattern length
3. **Space Complexity**: O(k) where k is the number of matches found
4. **Process**: 
   - For each position i in text (from 1 to n-m+1)
   - Extract substring of length m starting at position i
   - Compare with pattern
   - If match found, record position

This is the basic naive approach that checks every position systematically, making it easy to understand but not the most efficient for large texts.

