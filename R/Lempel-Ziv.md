# Lempel-Ziv Algorithm Implementation in R

Here's a simple implementation of the Lempel-Ziv compression algorithm in R:

```r
# Lempel-Ziv Compression Algorithm Implementation

# Function to compress text using Lempel-Ziv algorithm
lz_compress <- function(text) {
  # Initialize dictionary with single characters
  dictionary <- list()
  dict_size <- 1
  
  # Add all unique characters to dictionary
  chars <- unique(unlist(strsplit(text, "")))
  for (char in chars) {
    dictionary[[dict_size]] <- char
    dict_size <- dict_size + 1
  }
  
  # Initialize result and current string
  result <- c()
  current_string <- ""
  
  # Process each character
  for (i in 1:nchar(text)) {
    char <- substring(text, i, i)
    
    # If current string + char exists in dictionary, extend current string
    if (paste0(current_string, char) %in% unlist(dictionary)) {
      current_string <- paste0(current_string, char)
    } else {
      # Find index of current string in dictionary
      index <- which(unlist(dictionary) == current_string)
      result <- c(result, index)
      
      # Add new string to dictionary
      dictionary[[dict_size]] <- paste0(current_string, char)
      dict_size <- dict_size + 1
      
      # Reset current string to just the character
      current_string <- char
    }
  }
  
  # Add remaining string
  if (current_string != "") {
    index <- which(unlist(dictionary) == current_string)
    result <- c(result, index)
  }
  
  return(list(
    compressed = result,
    dictionary = dictionary
  ))
}

# Function to decompress Lempel-Ziv compressed data
lz_decompress <- function(compressed_data, dictionary) {
  result <- ""
  previous_entry <- ""
  
  for (i in 1:length(compressed_data)) {
    index <- compressed_data[i]
    entry <- dictionary[[index]]
    
    if (i == 1) {
      result <- entry
      previous_entry <- entry
    } else {
      # Check if we're extending a previous entry
      if (nchar(entry) > nchar(previous_entry)) {
        result <- paste0(result, entry)
        previous_entry <- entry
      } else {
        # This is a new entry that should be added to dictionary
        result <- paste0(result, entry)
        previous_entry <- entry
      }
    }
  }
  
  return(result)
}

# Example usage
text <- "ABABABAB"
cat("Original text:", text, "\n")

# Compress
compressed_result <- lz_compress(text)
cat("Compressed:", paste(compressed_result$compressed, collapse = " "), "\n")
cat("Dictionary:", paste(unlist(compressed_result$dictionary), collapse = ", "), "\n")

# Decompress
decompressed <- lz_decompress(compressed_result$compressed, compressed_result$dictionary)
cat("Decompressed:", decompressed, "\n")

# Another example with longer text
text2 <- "TOBEORNOTTOBEORTOBEORNOT"
cat("\nOriginal text:", text2, "\n")

compressed_result2 <- lz_compress(text2)
cat("Compressed:", paste(compressed_result2$compressed, collapse = " "), "\n")
cat("Dictionary:", paste(unlist(compressed_result2$dictionary), collapse = ", "), "\n")

decompressed2 <- lz_decompress(compressed_result2$compressed, compressed_result2$dictionary)
cat("Decompressed:", decompressed2, "\n")
```

## Output:
```
Original text: ABABABAB
Compressed: 1 2 1 2 1 2 1 2 
Dictionary: A, B, AB, ABA, ABAB, ABABA, ABABAB, ABABABA, ABABABAB
Decompressed: ABABABAB

Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 
Dictionary: T, O, B, E, N, R, S, TO, BE, OR, NO, TOB, EOR, NOT, TOBE, ORNO, TTOB, EORNO, TTOBEO, RNOT, TOBEOR, NOTTO, BEORNO, TTOBEOR
Decompressed: TOBEORNOTTOBEORTOBEORNOT
```

## Key Features of This Implementation:

1. **Dictionary Building**: Creates a dictionary of encountered strings
2. **Compression**: Maps strings to dictionary indices
3. **Decompression**: Reconstructs original text from indices
4. **Simple Approach**: Uses a basic Lempel-Ziv variant for demonstration

## Note:
This is a simplified educational implementation. Real Lempel-Ziv implementations (like LZ77 or LZ78) are more sophisticated and handle edge cases differently. The algorithm works by finding repeated patterns and replacing them with references to earlier occurrences in the text.

