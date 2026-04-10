# Treap Implementation in R

A treap is a data structure that combines the properties of a binary search tree and a heap. Each node has both a key (for BST property) and a priority (for heap property).

```r
# Treap implementation in R

# Node class for treap
TreapNode <- function(key, priority = NULL) {
  list(
    key = key,
    priority = ifelse(is.null(priority), runif(1, 0, 1), priority),
    left = NULL,
    right = NULL,
    parent = NULL
  )
}

# Treap class
Treap <- function() {
  list(
    root = NULL,
    size = 0
  )
}

# Right rotation
rotate_right <- function(node) {
  left_child <- node$left
  node$left = left_child$right
  if (!is.null(node$left)) {
    node$left$parent = node
  }
  left_child$right = node
  left_child$parent = node$parent
  node$parent = left_child
  left_child
}

# Left rotation
rotate_left <- function(node) {
  right_child <- node$right
  node$right = right_child$left
  if (!is.null(node$right)) {
    node$right$parent = node
  }
  right_child$left = node
  right_child$parent = node$parent
  node$parent = right_child
  right_child
}

# Insert a key into the treap
treap_insert <- function(treap, key) {
  # Create new node
  new_node <- TreapNode(key)
  
  # If tree is empty
  if (is.null(treap$root)) {
    treap$root <- new_node
    treap$size <- treap$size + 1
    return(treap)
  }
  
  # Find insertion point
  current <- treap$root
  parent <- NULL
  
  while (!is.null(current)) {
    parent <- current
    if (key < current$key) {
      current <- current$left
    } else if (key > current$key) {
      current <- current$right
    } else {
      # Key already exists
      return(treap)
    }
  }
  
  # Insert new node
  new_node$parent <- parent
  if (key < parent$key) {
    parent$left <- new_node
  } else {
    parent$right <- new_node
  }
  
  treap$size <- treap$size + 1
  
  # Restore heap property
  current <- new_node
  while (!is.null(current$parent) && 
         current$priority > current$parent$priority) {
    
    if (current == current$parent$left) {
      current <- rotate_right(current$parent)
    } else {
      current <- rotate_left(current$parent)
    }
  }
  
  # Update root if needed
  if (is.null(current$parent)) {
    treap$root <- current
  }
  
  treap
}

# Search for a key in the treap
treap_search <- function(treap, key) {
  current <- treap$root
  while (!is.null(current)) {
    if (key < current$key) {
      current <- current$left
    } else if (key > current$key) {
      current <- current$right
    } else {
      return(current)
    }
  }
  NULL
}

# Delete a key from the treap
treap_delete <- function(treap, key) {
  node <- treap_search(treap, key)
  if (is.null(node)) {
    return(treap)
  }
  
  # Find the node to delete and handle cases
  while (!is.null(node$left) || !is.null(node$right)) {
    # Rotate to make node a leaf
    if (is.null(node$left) || 
        (!is.null(node$right) && node$right$priority > node$left$priority)) {
      node <- rotate_left(node)
    } else {
      node <- rotate_right(node)
    }
  }
  
  # Remove leaf node
  parent <- node$parent
  if (!is.null(parent)) {
    if (node == parent$left) {
      parent$left <- NULL
    } else {
      parent$right <- NULL
    }
  } else {
    treap$root <- NULL
  }
  
  treap$size <- treap$size - 1
  treap
}

# Print treap in-order traversal
treap_inorder <- function(treap, node = NULL) {
  if (is.null(node)) {
    node <- treap$root
  }
  
  if (is.null(node)) {
    return(character(0))
  }
  
  result <- c()
  result <- c(result, treap_inorder(treap, node$left))
  result <- c(result, paste0(node$key, "(", node$priority, ")"))
  result <- c(result, treap_inorder(treap, node$right))
  
  result
}

# Example usage
cat("=== Treap Example ===\n")

# Create a new treap
my_treap <- Treap()

# Insert some keys
keys <- c(10, 5, 15, 3, 7, 12, 18, 1, 6, 8)
cat("Inserting keys:", paste(keys, collapse = " "), "\n")

for (key in keys) {
  my_treap <- treap_insert(my_treap, key)
}

# Display the treap
cat("In-order traversal:", paste(treap_inorder(my_treap), collapse = " "), "\n")
cat("Tree size:", my_treap$size, "\n\n")

# Search for a key
search_key <- 7
found <- treap_search(my_treap, search_key)
if (!is.null(found)) {
  cat("Found key", search_key, "with priority", found$priority, "\n")
} else {
  cat("Key", search_key, "not found\n")
}

# Delete a key
cat("\nDeleting key 7...\n")
my_treap <- treap_delete(my_treap, 7)
cat("In-order traversal after deletion:", paste(treap_inorder(my_treap), collapse = " "), "\n")
cat("Tree size:", my_treap$size, "\n")
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, left/right children, and parent pointer
2. **Insertion**: Maintains BST property and heap property through rotations
3. **Search**: Standard BST search with O(log n) average complexity
4. **Deletion**: Uses rotations to maintain heap property while removing nodes
5. **Rotations**: Left and right rotations to restore heap property

## Time Complexity:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case  
- **Delete**: O(log n) average, O(n) worst case

## Space Complexity:
- O(n) where n is the number of nodes

The treap maintains both the BST property (in-order traversal gives sorted sequence) and the heap property (priority of each node is higher than its children), making it an efficient data structure for dynamic sets.

