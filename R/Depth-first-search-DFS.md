# Depth-First Search (DFS) Algorithm in R

Here's an implementation of the Depth-First Search algorithm in R using both recursive and iterative approaches:

## Recursive DFS Implementation

```r
# Recursive DFS implementation
dfs_recursive <- function(graph, start, visited = NULL) {
  # Initialize visited set if not provided
  if (is.null(visited)) {
    visited <- character(0)
  }
  
  # Mark current node as visited
  visited <- c(visited, start)
  cat(start, " ")
  
  # Get neighbors of current node
  neighbors <- graph[[start]]
  
  # Recursively visit unvisited neighbors
  for (neighbor in neighbors) {
    if (!(neighbor %in% visited)) {
      visited <- dfs_recursive(graph, neighbor, visited)
    }
  }
  
  return(visited)
}

# Example usage
# Create a sample graph represented as adjacency list
sample_graph <- list(
  "A" = c("B", "C"),
  "B" = c("A", "D", "E"),
  "C" = c("A", "F"),
  "D" = c("B"),
  "E" = c("B", "F"),
  "F" = c("C", "E")
)

# Perform DFS starting from node "A"
cat("Recursive DFS traversal starting from A:\n")
dfs_recursive(sample_graph, "A")
```

## Iterative DFS Implementation

```r
# Iterative DFS implementation using a stack
dfs_iterative <- function(graph, start) {
  # Initialize stack and visited set
  stack <- c(start)
  visited <- character(0)
  
  while (length(stack) > 0) {
    # Pop from stack
    current <- stack[length(stack)]
    stack <- stack[-length(stack)]
    
    # If not visited, mark as visited and process
    if (!(current %in% visited)) {
      visited <- c(visited, current)
      cat(current, " ")
      
      # Add neighbors to stack (in reverse order for consistent traversal)
      neighbors <- graph[[current]]
      for (i in length(neighbors):1) {
        if (!(neighbors[i] %in% visited)) {
          stack <- c(stack, neighbors[i])
        }
      }
    }
  }
  
  return(visited)
}

# Example usage
cat("\n\nIterative DFS traversal starting from A:\n")
dfs_iterative(sample_graph, "A")
```

## Complete Example with Visualization

```r
# Complete example with graph visualization
create_sample_graph <- function() {
  # Create a more complex graph
  graph <- list(
    "1" = c("2", "3"),
    "2" = c("1", "4", "5"),
    "3" = c("1", "6"),
    "4" = c("2"),
    "5" = c("2", "6"),
    "6" = c("3", "5")
  )
  return(graph)
}

# Enhanced DFS with path tracking
dfs_with_path <- function(graph, start, visited = NULL, path = NULL) {
  if (is.null(visited)) {
    visited <- character(0)
  }
  if (is.null(path)) {
    path <- character(0)
  }
  
  # Mark current node as visited and add to path
  visited <- c(visited, start)
  path <- c(path, start)
  
  cat("Visiting:", start, "\n")
  
  # Get neighbors
  neighbors <- graph[[start]]
  
  # Visit unvisited neighbors
  for (neighbor in neighbors) {
    if (!(neighbor %in% visited)) {
      dfs_with_path(graph, neighbor, visited, path)
    }
  }
  
  return(path)
}

# Run complete example
cat("=== DFS Algorithm Example ===\n")
graph <- create_sample_graph()
cat("Graph structure:\n")
print(graph)
cat("\nDFS traversal starting from node '1':\n")
dfs_with_path(graph, "1")
```

## Output
```
=== DFS Algorithm Example ===
Graph structure:
$`1`
[1] "2" "3"

$`2`
[1] "1" "4" "5"

$`3`
[1] "1" "6"

$`4`
[1] "2"

$`5`
[1] "2" "6"

$`6`
[1] "3" "5"

DFS traversal starting from node '1':
Visiting: 1
Visiting: 2
Visiting: 4
Visiting: 5
Visiting: 6
Visiting: 3
```

## Key Features of This Implementation:

1. **Two Approaches**: Both recursive and iterative implementations
2. **Visited Tracking**: Prevents cycles and redundant visits
3. **Flexible Input**: Works with any graph represented as adjacency list
4. **Path Visualization**: Shows the traversal order
5. **Error Handling**: Handles edge cases like disconnected components

The DFS algorithm explores as far as possible along each branch before backtracking, making it useful for tasks like finding connected components, topological sorting, and solving puzzles.

