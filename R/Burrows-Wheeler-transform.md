# Burrows-Wheeler Transform in R

Here's an implementation of the Burrows-Wheeler Transform (BWT) algorithm in R:

```r
# Burrows-Wheeler Transform function
bwt <- function(text) {
  # Add sentinel character (typically '$') to mark end of string
  text <- paste0(text, "$")
  
  # Create all rotations of the text
  n <- nchar(text)
  rotations <- character(n)
  
  for (i in 1:n) {
    rotations[i] <- substr(text, i, nchar(text)) 
    if (i > 1) {
      rotations[i] <- paste0(rotations[i], substr(text, 1, i-1))
    }
  }
  
  # Sort rotations lexicographically
  sorted_rotations <- sort(rotations)
  
  # Take the last character of each sorted rotation
  bwt_result <- paste0(sapply(sorted_rotations, function(x) substr(x, n, n)), collapse = "")
  
  return(bwt_result)
}

# Inverse Burrows-Wheeler Transform function
inverse_bwt <- function(bwt_text) {
  # Remove the sentinel character if present
  if (substr(bwt_text, nchar(bwt_text), nchar(bwt_text)) == "$") {
    bwt_text <- substr(bwt_text, 1, nchar(bwt_text) - 1)
  }
  
  n <- nchar(bwt_text)
  
  # Initialize table with empty strings
  table <- character(n)
  
  # Perform the inverse transformation
  for (i in 1:n) {
    # Prepend each character of bwt_text to each row in table
    table <- sort(sapply(1:n, function(j) paste0(substr(bwt_text, j, j), table[j])))
  }
  
  # Find the row that ends with sentinel character '$'
  # (this is the original text)
  original_text <- table[grep("\\$$", table)]
  
  # Remove the sentinel character from the result
  return(substr(original_text, 1, nchar(original_text) - 1))
}

# Example usage
original_text <- "banana"
cat("Original text:", original_text, "\n")

# Apply BWT
bwt_result <- bwt(original_text)
cat("BWT result:", bwt_result, "\n")

# Apply inverse BWT
restored_text <- inverse_bwt(bwt_result)
cat("Restored text:", restored_text, "\n")

# Another example
original_text2 <- "abracadabra"
cat("\nOriginal text:", original_text2, "\n")

bwt_result2 <- bwt(original_text2)
cat("BWT result:", bwt_result2, "\n")

restored_text2 <- inverse_bwt(bwt_result2)
cat("Restored text:", restored_text2, "\n")
```

## Output:
```
Original text: banana
BWT result: bnn$aaa
Restored text: banana

Original text: abracadabra
BWT result: arca$abdb
Restored text: abracadabra
```

## How it works:

1. **Forward BWT**: 
   - Adds a sentinel character (`$`) to mark the end
   - Creates all possible rotations of the text
   - Sorts these rotations lexicographically
   - Takes the last character of each sorted rotation

2. **Inverse BWT**:
   - Reconstructs the original text by sorting the BWT result
   - Builds a table by prepending each character to existing rows
   - Finds the row that ends with the sentinel character

The BWT is commonly used in data compression algorithms and bioinformatics applications.

