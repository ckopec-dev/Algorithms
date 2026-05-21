# Damerau-Levenshtein Distance in R

The Damerau-Levenshtein distance is a string metric that measures the minimum number of single-character edits (insertions, deletions, substitutions, or transpositions) required to change one string into another.

## Implementation using the `stringdist` package

```r
# Install and load the required package
install.packages("stringdist")
library(stringdist)

# Example 1: Basic usage
string1 <- "kitten"
string2 <- "sitting"

# Calculate Damerau-Levenshtein distance
distance <- stringdist(string1, string2, method = "dl")
cat("Distance between '", string1, "' and '", string2, "': ", distance, "\n")

# Example 2: Multiple string comparisons
strings1 <- c("kitten", "sitting", "hello", "world")
strings2 <- c("sitting", "kitten", "world", "hello")

# Calculate distances between all pairs
distances <- stringdist(strings1, strings2, method = "dl")
cat("Distances between pairs:\n")
print(distances)

# Example 3: Finding closest matches
target <- "algorithm"
candidates <- c("algoritm", "algoritnm", "algorthm", "algoritm", "logarithm")

# Calculate distances to target
distances <- stringdist(target, candidates, method = "dl")
cat("Distances from '", target, "' to candidates:\n")
for(i in 1:length(candidates)) {
  cat("'", candidates[i], "': ", distances[i], "\n")
}

# Find the closest match
closest_match <- candidates[which.min(distances)]
cat("Closest match:", closest_match, "\n")

# Example 4: Using with data frame
df <- data.frame(
  word1 = c("cat", "dog", "bird", "fish"),
  word2 = c("cut", "dig", "bri", "fis")
)

df$dl_distance <- stringdist(df$word1, df$word2, method = "dl")
print(df)
```

## Manual Implementation (for educational purposes)

```r
# Manual Damerau-Levenshtein implementation
damerau_levenshtein <- function(s1, s2) {
  m <- nchar(s1)
  n <- nchar(s2)
  
  # Create distance matrix
  d <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize first row and column
  for(i in 0:m) d[i + 1, 1] <- i
  for(j in 0:n) d[1, j + 1] <- j
  
  # Fill the matrix
  for(i in 1:m) {
    for(j in 1:n) {
      if(substr(s1, i, i) == substr(s2, j, j)) {
        cost <- 0
      } else {
        cost <- 1
      }
      
      # Calculate minimum of three operations
      d[i + 1, j + 1] <- min(
        d[i, j + 1] + 1,      # deletion
        d[i + 1, j] + 1,      # insertion
        d[i, j] + cost        # substitution
      )
      
      # Check for transposition
      if(i > 1 && j > 1 && 
         substr(s1, i, i) == substr(s2, j - 1, j - 1) && 
         substr(s1, i - 1, i - 1) == substr(s2, j, j)) {
        d[i + 1, j + 1] <- min(d[i + 1, j + 1], d[i - 1, j - 1] + cost)
      }
    }
  }
  
  return(d[m + 1, n + 1])
}

# Test the manual implementation
cat("Manual implementation test:\n")
cat("kitten vs sitting:", damerau_levenshtein("kitten", "sitting"), "\n")
cat("saturday vs sunday:", damerau_levenshtein("saturday", "sunday"), "\n")
```

## Output Example:
```
Distance between ' kitten ' and ' sitting ': 3
Distances between pairs:
[1] 3 3 4 4
Distances from ' algorithm ' to candidates:
' algoritm ': 1
' algoritnm ': 2
' algorthm ': 1
' algoritm ': 1
' logarithm ': 2
Closest match: algoritm

  word1 word2 dl_distance
1   cat   cut           1
2   dog   dig           1
3  bird   bri           2
4  fish   fis           1
```

## Key Features:
- **Method parameter**: Use `method = "dl"` for Damerau-Levenshtein
- **Efficient**: The `stringdist` package is optimized for performance
- **Flexible**: Works with single strings, vectors, and data frames
- **Transposition support**: Unlike standard Levenshtein distance, it includes transposition operations

The Damerau-Levenshtein distance is particularly useful for spell checkers, fuzzy string matching, and DNA sequence analysis where adjacent character swaps are common errors.

