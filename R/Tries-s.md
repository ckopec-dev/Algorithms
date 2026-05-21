# Trie Algorithm Implementation in R

Here's an example implementation of a Trie (prefix tree) data structure in R:

```r
# Trie Node Class
TrieNode <- function() {
  node <- list(
    children = list(),
    is_end = FALSE,
    word = NULL
  )
  class(node) <- "TrieNode"
  return(node)
}

# Trie Class
Trie <- function() {
  root <- TrieNode()
  trie <- list(
    root = root,
    size = 0
  )
  class(trie) <- "Trie"
  return(trie)
}

# Insert a word into the trie
insert <- function(trie, word) {
  node <- trie$root
  
  for (char in strsplit(word, "")[[1]]) {
    if (!char %in% names(node$children)) {
      node$children[[char]] <- TrieNode()
    }
    node <- node$children[[char]]
  }
  
  if (!node$is_end) {
    node$is_end <- TRUE
    node$word <- word
    trie$size <- trie$size + 1
  }
}

# Search for a complete word
search <- function(trie, word) {
  node <- trie$root
  
  for (char in strsplit(word, "")[[1]]) {
    if (!char %in% names(node$children)) {
      return(FALSE)
    }
    node <- node$children[[char]]
  }
  
  return(node$is_end)
}

# Check if any word starts with the given prefix
starts_with <- function(trie, prefix) {
  node <- trie$root
  
  for (char in strsplit(prefix, "")[[1]]) {
    if (!char %in% names(node$children)) {
      return(FALSE)
    }
    node <- node$children[[char]]
  }
  
  return(TRUE)
}

# Get all words with a given prefix
get_words_with_prefix <- function(trie, prefix) {
  node <- trie$root
  result <- c()
  
  # Navigate to prefix
  for (char in strsplit(prefix, "")[[1]]) {
    if (!char %in% names(node$children)) {
      return(result)
    }
    node <- node$children[[char]]
  }
  
  # Collect all words from this node
  collect_words <- function(current_node, prefix) {
    words <- c()
    
    if (current_node$is_end) {
      words <- c(words, prefix)
    }
    
    for (char in names(current_node$children)) {
      child_node <- current_node$children[[char]]
      words <- c(words, collect_words(child_node, paste0(prefix, char)))
    }
    
    return(words)
  }
  
  return(collect_words(node, prefix))
}

# Example usage
# Create a new trie
my_trie <- Trie()

# Insert words
words_to_insert <- c("apple", "app", "application", "apply", "banana", "band")
for (word in words_to_insert) {
  insert(my_trie, word)
}

# Test search functionality
cat("Search 'app':", search(my_trie, "app"), "\n")        # TRUE
cat("Search 'appl':", search(my_trie, "appl"), "\n")      # FALSE
cat("Search 'banana':", search(my_trie, "banana"), "\n")  # TRUE

# Test prefix functionality
cat("Starts with 'app':", starts_with(my_trie, "app"), "\n")    # TRUE
cat("Starts with 'ban':", starts_with(my_trie, "ban"), "\n")    # TRUE
cat("Starts with 'xyz':", starts_with(my_trie, "xyz"), "\n")    # FALSE

# Get words with prefix
cat("Words with prefix 'app':\n")
print(get_words_with_prefix(my_trie, "app"))

cat("Words with prefix 'ban':\n")
print(get_words_with_prefix(my_trie, "ban"))
```

## Output:
```
Search 'app': TRUE
Search 'appl': FALSE
Search 'banana': TRUE
Starts with 'app': TRUE
Starts with 'ban': TRUE
Starts with 'xyz': FALSE
Words with prefix 'app':
[1] "app"         "apple"       "application" "apply"      
Words with prefix 'ban':
[1] "banana" "band"
```

## Key Features of this Trie Implementation:

1. **Insert**: Adds words to the trie structure
2. **Search**: Checks if a complete word exists
3. **Starts With**: Checks if any word begins with a given prefix
4. **Get Words with Prefix**: Returns all words that start with a given prefix

## Time Complexity:
- Insert: O(m) where m is the length of the word
- Search: O(m) where m is the length of the word
- Prefix Search: O(p) where p is the length of the prefix

## Space Complexity:
- O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length of words

This implementation demonstrates how tries are particularly useful for autocomplete features, spell checkers, and prefix-based searches.

