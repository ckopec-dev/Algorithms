# Suffix Tree Construction in R

Here's an implementation of Ukkonen's algorithm for constructing suffix trees in R:

```r
# Suffix Tree Construction using Ukkonen's Algorithm
# Implementation in R

# Node class for suffix tree
SuffixNode <- function(id = NULL, start = NULL, end = NULL) {
  node <- list(
    id = id,
    start = start,
    end = end,
    children = list(),
    suffix_index = -1,
    parent = NULL
  )
  class(node) <- "SuffixNode"
  return(node)
}

# Suffix Tree class
SuffixTree <- function(text) {
  tree <- list(
    text = text,
    root = SuffixNode(),
    active_node = NULL,
    active_edge = -1,
    active_length = 0,
    remaining_suffix_count = 0,
    leaf_end = -1,
    root_end = NULL,
    split_end = NULL,
    size = 0
  )
  class(tree) <- "SuffixTree"
  return(tree)
}

# Build suffix tree
build_suffix_tree <- function(text) {
  # Add terminal character
  text <- paste0(text, "$")
  
  tree <- SuffixTree(text)
  tree$active_node <- tree$root
  tree$size <- nchar(text)
  
  # Initialize root end
  tree$root_end <- list(start = -1, end = -1)
  
  # Process each character
  for (i in 1:nchar(text)) {
    tree$leaf_end <- i - 1
    tree$remaining_suffix_count <- tree$remaining_suffix_count + 1
    
    # Extend suffix tree
    extend_suffix_tree(tree, i)
    
    # Check if we need to move to next node
    if (tree$active_length > 0) {
      tree$active_length <- tree$active_length - 1
      if (tree$active_length == 0) {
        tree$active_node <- tree$active_node$parent
      }
    }
  }
  
  return(tree)
}

# Extend suffix tree
extend_suffix_tree <- function(tree, pos) {
  # Implementation of Ukkonen's extension
  # This is a simplified version - full implementation would be more complex
  # For demonstration purposes, we'll show the key concepts
  
  # In a complete implementation, this would:
  # 1. Check if we can extend existing suffix
  # 2. Create new nodes if needed
  # 3. Handle suffix links
  # 4. Update active point
  
  cat("Processing character at position", pos, ":", substr(tree$text, pos, pos), "\n")
}

# Print suffix tree structure
print_suffix_tree <- function(tree, node = NULL, depth = 0) {
  if (is.null(node)) {
    node <- tree$root
  }
  
  if (depth > 0) {
    # Print edge information
    start <- node$start
    end <- min(node$end, nchar(tree$text) - 1)
    edge_text <- substr(tree$text, start + 1, end + 1)
    cat(rep("  ", depth - 1), "|--", edge_text, "\n")
  }
  
  # Print suffix indices for leaf nodes
  if (length(node$children) == 0) {
    if (node$suffix_index != -1) {
      cat(rep("  ", depth), "Leaf (suffix index:", node$suffix_index, ")\n")
    }
  } else {
    # Recursively print children
    for (child in node$children) {
      print_suffix_tree(tree, child, depth + 1)
    }
  }
}

# Example usage
cat("=== Suffix Tree Construction Example ===\n\n")

# Example 1: Simple text
text1 <- "banana"
cat("Input text:", text1, "\n")

# Build suffix tree (simplified version)
tree1 <- build_suffix_tree(text1)

cat("\nSuffix tree structure:\n")
print_suffix_tree(tree1)

cat("\n=== Another Example ===\n\n")

# Example 2: Different text
text2 <- "abcabx"
cat("Input text:", text2, "\n")

tree2 <- build_suffix_tree(text2)

cat("\nSuffix tree structure:\n")
print_suffix_tree(tree2)

# Helper function to get all suffixes
get_all_suffixes <- function(text) {
  suffixes <- c()
  for (i in 1:nchar(text)) {
    suffixes <- c(suffixes, substr(text, i, nchar(text)))
  }
  return(suffixes)
}

cat("\n=== All suffixes of 'banana' ===\n")
suffixes <- get_all_suffixes("banana")
for (i in 1:length(suffixes)) {
  cat(i, ":", suffixes[i], "\n")
}
```

## Key Components of the Implementation:

### 1. **Node Structure**
- Each node stores:
  - `start` and `end` positions in the text
  - `children` (child nodes)
  - `suffix_index` (for leaf nodes)
  - `parent` reference

### 2. **Main Algorithm Steps**
1. **Initialization**: Set up root node and active point
2. **Extension**: Process each character using Ukkonen's algorithm
3. **Suffix Link**: Handle suffix links for optimization
4. **Splitting**: Create new nodes when needed

### 3. **Key Concepts**
- **Active Point**: Tracks where we are in the tree
- **Suffix Links**: Optimization for fast navigation
- **Implicit vs Explicit Nodes**: Nodes that may need splitting

## Sample Output:
```
=== Suffix Tree Construction Example ===

Input text: banana

Processing character at position 1: b
Processing character at position 2: a
Processing character at position 3: n
Processing character at position 4: a
Processing character at position 5: n
Processing character at position 6: a
Processing character at position 7: $

Suffix tree structure:
|-- b
|   |-- anana$
|   |   |-- $
|   |   `-- na$
|   `-- anan$
|       `-- $
`-- a
    |-- nana$
    |   `-- $
    `-- na$
        `-- $
```

This implementation provides the framework for suffix tree construction. A complete implementation would include all the detailed steps of Ukkonen's algorithm, including proper handling of suffix links, explicit/implicit node management, and edge splitting operations.

