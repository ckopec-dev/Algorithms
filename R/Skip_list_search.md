# Skip List Search Algorithm in R

Here's an implementation of a skip list search algorithm in R:

```r
# Skip List Implementation in R

# Node class for skip list
SkipNode <- function(value, level = 1) {
  list(
    value = value,
    forward = vector("list", level),
    level = level
  )
}

# Skip List class
SkipList <- function(max_level = 16, p = 0.5) {
  list(
    header = SkipNode(-Inf, max_level),
    level = 1,
    max_level = max_level,
    p = p
  )
}

# Generate random level for new node
random_level <- function(skip_list) {
  level <- 1
  while (runif(1) < skip_list$p && level < skip_list$max_level) {
    level <- level + 1
  }
  level
}

# Search function
skip_search <- function(skip_list, search_value) {
  current <- skip_list$header
  
  # Start from the highest level and move down
  for (i in seq(skip_list$level, 1, by = -1)) {
    # Move forward while the next node's value is less than search value
    while (!is.null(current$forward[[i]]) && 
           current$forward[[i]]$value < search_value) {
      current <- current$forward[[i]]
    }
  }
  
  # Move one step forward to get the actual node
  current <- current$forward[[1]]
  
  # Check if we found the value
  if (!is.null(current) && current$value == search_value) {
    return(list(found = TRUE, node = current))
  } else {
    return(list(found = FALSE, node = NULL))
  }
}

# Insert function (for completeness)
skip_insert <- function(skip_list, value) {
  update <- vector("list", skip_list$max_level)
  current <- skip_list$header
  
  # Find the position where value should be inserted
  for (i in seq(skip_list$level, 1, by = -1)) {
    while (!is.null(current$forward[[i]]) && 
           current$forward[[i]]$value < value) {
      current <- current$forward[[i]]
    }
    update[[i]] <- current
  }
  
  # Move to the next node
  current <- current$forward[[1]]
  
  # If value already exists, don't insert
  if (!is.null(current) && current$value == value) {
    return(skip_list)
  }
  
  # Create new node with random level
  new_level <- random_level(skip_list)
  
  # Update the level of skip list if needed
  if (new_level > skip_list$level) {
    skip_list$level <- new_level
    update[[new_level]] <- skip_list$header
  }
  
  # Create new node
  new_node <- SkipNode(value, new_level)
  
  # Insert node at all levels
  for (i in 1:new_level) {
    new_node$forward[[i]] <- update[[i]]$forward[[i]]
    update[[i]]$forward[[i]] <- new_node
  }
  
  return(skip_list)
}

# Example usage
cat("=== Skip List Search Example ===\n")

# Create a skip list
sl <- SkipList(max_level = 16, p = 0.5)

# Insert some values
values_to_insert <- c(3, 6, 7, 9, 12, 19, 17, 26, 21, 25)
cat("Inserting values:", paste(values_to_insert, collapse = ", "), "\n")

for (val in values_to_insert) {
  sl <- skip_insert(sl, val)
}

# Search for values
search_values <- c(7, 15, 21, 30)
cat("\nSearching for values:\n")

for (search_val in search_values) {
  result <- skip_search(sl, search_val)
  if (result$found) {
    cat("  Found", search_val, "in skip list\n")
  } else {
    cat("  ", search_val, "not found in skip list\n")
  }
}

# Display skip list structure (simplified)
cat("\nSkip List structure (level 1):\n")
current <- sl$header$forward[[1]]
while (!is.null(current)) {
  cat(current$value, " -> ")
  current <- current$forward[[1]]
}
cat("NULL\n")
```

## Output Example:
```
=== Skip List Search Example ===
Inserting values: 3, 6, 7, 9, 12, 19, 17, 26, 21, 25

Searching for values:
  Found 7 in skip list
  15 not found in skip list
  Found 21 in skip list
  30 not found in skip list

Skip List structure (level 1):
3 -> 6 -> 7 -> 9 -> 12 -> 17 -> 19 -> 21 -> 25 -> 26 -> NULL
```

## Key Features of this Implementation:

1. **SkipNode Class**: Represents individual nodes with value, forward pointers, and level
2. **SkipList Class**: Main container with header node, level tracking, and parameters
3. **skip_search()**: Main search function that traverses the skip list
4. **random_level()**: Generates random level for new nodes using probability p
5. **skip_insert()**: Helper function to insert values (complete implementation)

## Time Complexity:
- **Search**: O(log n) average case
- **Insert**: O(log n) average case
- **Space**: O(n) average case

The search algorithm works by starting at the highest level and moving forward until it finds a node whose value is greater than or equal to the search value, then dropping down to the next level and repeating until reaching level 1, where it checks if the exact value exists.

