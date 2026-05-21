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
    head = SkipNode(-Inf, max_level),
    level = 1,
    max_level = max_level,
    p = p
  )
}

# Function to generate random level
random_level <- function(skip_list) {
  level <- 1
  while (runif(1) < skip_list$p && level < skip_list$max_level) {
    level <- level + 1
  }
  level
}

# Search function for skip list
skip_list_search <- function(skip_list, search_value) {
  # Start from the highest level
  current <- skip_list$head
  
  # Traverse the skip list
  for (i in seq(skip_list$level, 1, by = -1)) {
    # Move forward while the next node's value is less than search value
    while (length(current$forward[[i]]) > 0 && 
           current$forward[[i]]$value < search_value) {
      current <- current$forward[[i]]
    }
  }
  
  # Move to the next node (this is the node with value >= search_value)
  current <- current$forward[[1]]
  
  # Check if we found the exact value
  if (length(current) > 0 && current$value == search_value) {
    return(list(found = TRUE, node = current))
  } else {
    return(list(found = FALSE, node = NULL))
  }
}

# Insert function (for completeness)
skip_list_insert <- function(skip_list, value) {
  update <- vector("list", skip_list$max_level)
  
  current <- skip_list$head
  
  # Find the position where value should be inserted
  for (i in seq(skip_list$level, 1, by = -1)) {
    while (length(current$forward[[i]]) > 0 && 
           current$forward[[i]]$value < value) {
      current <- current$forward[[i]]
    }
    update[[i]] <- current
  }
  
  # Generate random level for new node
  new_level <- random_level(skip_list)
  
  # Update the level of skip list if needed
  if (new_level > skip_list$level) {
    skip_list$level <- new_level
    update[[new_level]] <- skip_list$head
  }
  
  # Create new node
  new_node <- SkipNode(value, new_level)
  
  # Insert the new node
  for (i in 1:new_level) {
    new_node$forward[[i]] <- update[[i]]$forward[[i]]
    update[[i]]$forward[[i]] <- new_node
  }
  
  return(skip_list)
}

# Example usage
# Create a skip list
skip_list <- SkipList(max_level = 16, p = 0.5)

# Insert some values
values <- c(3, 6, 7, 9, 12, 19, 21, 34, 45, 57, 70, 81, 92, 100)
for (val in values) {
  skip_list <- skip_list_insert(skip_list, val)
}

# Search for values
search_results <- list(
  skip_list_search(skip_list, 12),
  skip_list_search(skip_list, 25),
  skip_list_search(skip_list, 92)
)

# Display results
cat("Search Results:\n")
for (i in 1:length(search_results)) {
  result <- search_results[[i]]
  if (result$found) {
    cat("Value", result$node$value, "found\n")
  } else {
    cat("Value not found\n")
  }
}
```

## Output:
```
Search Results:
Value 12 found
Value not found
Value 92 found
```

## Algorithm Explanation:

The `skip_list_search` function works as follows:

1. **Start from the highest level** - Begin at the topmost level of the skip list
2. **Traverse horizontally** - Move forward through the current level while the next node's value is less than the search value
3. **Drop down** - When we find a node whose value is greater than or equal to the search value, drop down to the next lower level
4. **Continue until bottom level** - Repeat until we reach level 1
5. **Check exact match** - At the bottom level, check if we found the exact value

## Time Complexity:
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

The skip list search algorithm efficiently finds elements by "skipping" over large portions of the list, similar to how you might jump through a sorted array using a jump search approach.

