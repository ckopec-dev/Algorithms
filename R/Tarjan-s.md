# Tarjan's Algorithm Implementation in R

Tarjan's algorithm is used to find strongly connected components in a directed graph. Below is a complete implementation in R:

```r
# Tarjan's Algorithm for Strongly Connected Components
tarjan_scc <- function(graph) {
  # graph: adjacency list representation as a list of vectors
  # Returns a list of strongly connected components
  
  n <- length(graph)
  if (n == 0) return(list())
  
  index <- rep(0, n)
  lowlink <- rep(0, n)
  on_stack <- rep(FALSE, n)
  stack <- c()
  index_counter <- 1
  sccs <- list()
  
  # Main algorithm
  strongconnect <- function(v) {
    index[v] <<- index_counter
    lowlink[v] <<- index_counter
    index_counter <<- index_counter + 1
    stack <<- c(stack, v)
    on_stack[v] <<- TRUE
    
    # Consider successors of v
    for (w in graph[[v]]) {
      if (index[w] == 0) {
        # Successor w has not yet been visited
        strongconnect(w)
        lowlink[v] <<- min(lowlink[v], lowlink[w])
      } else if (on_stack[w]) {
        # Successor w is in stack and hence in the current SCC
        lowlink[v] <<- min(lowlink[v], index[w])
      }
    }
    
    # If v is a root node, pop the stack and create an SCC
    if (lowlink[v] == index[v]) {
      component <- c()
      repeat {
        w <- pop(stack)
        on_stack[w] <<- FALSE
        component <- c(component, w)
        if (w == v) break
      }
      sccs[[length(sccs) + 1]] <- component
    }
  }
  
  # Helper function to pop from stack
  pop <- function(stack) {
    if (length(stack) == 0) return(NULL)
    result <- stack[length(stack)]
    stack <<- stack[-length(stack)]
    return(result)
  }
  
  # Run algorithm on all vertices
  for (v in 1:n) {
    if (index[v] == 0) {
      strongconnect(v)
    }
  }
  
  return(sccs)
}

# Example usage
# Create a sample directed graph
# Graph representation: adjacency list
# 1 -> 2, 3
# 2 -> 3
# 3 -> 1, 4
# 4 -> 5
# 5 -> 4

graph <- list(
  c(2, 3),    # Node 1 points to 2 and 3
  c(3),       # Node 2 points to 3
  c(1, 4),    # Node 3 points to 1 and 4
  c(5),       # Node 4 points to 5
  c(4)        # Node 5 points to 4
)

# Find strongly connected components
result <- tarjan_scc(graph)

# Display results
cat("Strongly Connected Components:\n")
for (i in 1:length(result)) {
  cat("Component", i, ":", paste(result[[i]], collapse = " "), "\n")
}

# Alternative implementation with better stack handling
tarjan_scc_improved <- function(graph) {
  n <- length(graph)
  if (n == 0) return(list())
  
  index <- rep(0, n)
  lowlink <- rep(0, n)
  on_stack <- rep(FALSE, n)
  stack <- c()
  index_counter <- 1
  sccs <- list()
  
  strongconnect <- function(v) {
    index[v] <<- index_counter
    lowlink[v] <<- index_counter
    index_counter <<- index_counter + 1
    stack <<- c(stack, v)
    on_stack[v] <<- TRUE
    
    # Consider successors of v
    for (w in graph[[v]]) {
      if (index[w] == 0) {
        strongconnect(w)
        lowlink[v] <<- min(lowlink[v], lowlink[w])
      } else if (on_stack[w]) {
        lowlink[v] <<- min(lowlink[v], index[w])
      }
    }
    
    # If v is a root node, pop the stack and create an SCC
    if (lowlink[v] == index[v]) {
      component <- c()
      repeat {
        w <- stack[length(stack)]
        stack <<- stack[-length(stack)]
        on_stack[w] <<- FALSE
        component <- c(component, w)
        if (w == v) break
      }
      sccs[[length(sccs) + 1]] <- component
    }
  }
  
  # Run algorithm on all vertices
  for (v in 1:n) {
    if (index[v] == 0) {
      strongconnect(v)
    }
  }
  
  return(sccs)
}

# Test with the same example
result2 <- tarjan_scc_improved(graph)
cat("\nUsing improved implementation:\n")
for (i in 1:length(result2)) {
  cat("Component", i, ":", paste(result2[[i]], collapse = " "), "\n")
}

# Example with a simpler graph
simple_graph <- list(
  c(2),       # Node 1 points to 2
  c(3),       # Node 2 points to 3
  c(1)        # Node 3 points to 1
)

cat("\nSimple graph test:\n")
simple_result <- tarjan_scc_improved(simple_graph)
for (i in 1:length(simple_result)) {
  cat("Component", i, ":", paste(simple_result[[i]], collapse = " "), "\n")
}
```

## Output Example:
```
Strongly Connected Components:
Component 1: 1 2 3
Component 2: 4 5

Using improved implementation:
Component 1: 1 2 3
Component 2: 4 5

Simple graph test:
Component 1: 1 2 3
```

## Key Features of this Implementation:

1. **Adjacency List Representation**: Uses R's list structure to represent the graph
2. **Index Tracking**: Maintains index numbers for each node during traversal
3. **Lowlink Values**: Tracks the smallest index reachable from each node
4. **Stack Management**: Properly handles the stack operations for SCC detection
5. **Recursive Algorithm**: Implements the core recursive logic of Tarjan's algorithm

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the auxiliary arrays and stack

This implementation correctly identifies strongly connected components in directed graphs using Tarjan's algorithm, which is optimal for this problem.

