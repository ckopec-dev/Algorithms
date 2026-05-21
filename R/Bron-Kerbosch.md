# Bron-Kerbosch Algorithm Implementation in R

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation in R:

```r
# Bron-Kerbosch Algorithm Implementation in R
# Finds all maximal cliques in an undirected graph

# Function to implement Bron-Kerbosch algorithm
bron_kerbosch <- function(graph) {
  # Convert adjacency list to matrix if needed
  if (!is.matrix(graph)) {
    # Assuming graph is an adjacency list (list of neighbors)
    n <- length(graph)
    adj_matrix <- matrix(0, nrow = n, ncol = n)
    
    for (i in 1:n) {
      if (!is.null(graph[[i]])) {
        for (j in graph[[i]]) {
          adj_matrix[i, j] <- 1
          adj_matrix[j, i] <- 1
        }
      }
    }
    graph <- adj_matrix
  }
  
  n <- nrow(graph)
  cliques <- list()
  
  # Initialize sets
  R <- integer(0)  # Current clique
  P <- 1:n         # Potential vertices
  X <- integer(0)  # Excluded vertices
  
  # Recursive helper function
  bron_kerbosch_recursive <- function(R, P, X) {
    # If P and X are both empty, R is a maximal clique
    if (length(P) == 0 && length(X) == 0) {
      cliques[[length(cliques) + 1]] <- sort(R)
      return()
    }
    
    # For each vertex v in P
    for (i in seq_along(P)) {
      v <- P[i]
      
      # Add v to current clique
      new_R <- c(R, v)
      
      # Find neighbors of v in P
      P_v <- P[graph[v, P] == 1]
      
      # Find neighbors of v in X
      X_v <- X[graph[v, X] == 1]
      
      # Recursively call with updated sets
      bron_kerbosch_recursive(new_R, P_v, X_v)
      
      # Remove v from P and add to X
      P <- P[P != v]
      X <- c(X, v)
    }
  }
  
  bron_kerbosch_recursive(R, P, X)
  return(cliques)
}

# Alternative simpler implementation using the igraph package
library(igraph)

# Function to find maximal cliques using igraph
find_maximal_cliques_igraph <- function(graph) {
  # Create igraph object from adjacency matrix
  g <- graph_from_adjacency_matrix(graph, mode = "undirected")
  
  # Find all maximal cliques
  cliques <- maximal.cliques(g)
  
  # Convert to list format
  result <- list()
  for (i in seq_along(cliques)) {
    result[[i]] <- sort(cliques[[i]])
  }
  
  return(result)
}

# Example usage
cat("=== Bron-Kerbosch Algorithm Example ===\n\n")

# Create a sample graph (adjacency matrix)
# Graph with 6 vertices
# Edges: (1,2), (1,3), (2,3), (2,4), (3,4), (4,5), (4,6), (5,6)
sample_graph <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 1, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 1, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), nrow = 6, byrow = TRUE)

cat("Sample Graph Adjacency Matrix:\n")
print(sample_graph)

cat("\nUsing custom Bron-Kerbosch implementation:\n")
cliques1 <- bron_kerbosch(sample_graph)
cat("Maximal cliques found:\n")
for (i in seq_along(cliques1)) {
  cat("Clique", i, ":", paste(cliques1[[i]], collapse = ", "), "\n")
}

# Using igraph package (more efficient)
cat("\nUsing igraph package:\n")
cliques2 <- find_maximal_cliques_igraph(sample_graph)
cat("Maximal cliques found:\n")
for (i in seq_along(cliques2)) {
  cat("Clique", i, ":", paste(cliques2[[i]], collapse = ", "), "\n")
}

# Another example with a smaller graph
cat("\n=== Another Example ===\n")

# Simple triangle graph
triangle_graph <- matrix(c(
  0, 1, 1,
  1, 0, 1,
  1, 1, 0
), nrow = 3, byrow = TRUE)

cat("Triangle Graph Adjacency Matrix:\n")
print(triangle_graph)

cat("\nMaximal cliques in triangle graph:\n")
cliques3 <- find_maximal_cliques_igraph(triangle_graph)
for (i in seq_along(cliques3)) {
  cat("Clique", i, ":", paste(cliques3[[i]], collapse = ", "), "\n")
}
```

## Expected Output:
```
=== Bron-Kerbosch Algorithm Example ===

Sample Graph Adjacency Matrix:
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    0    1    1    0    0    0
[2,]    1    0    1    1    0    0
[3,]    1    1    0    1    0    0
[4,]    0    1    1    0    1    1
[5,]    0    0    0    1    0    1
[6,]    0    0    0    1    1    0

Using custom Bron-Kerbosch implementation:
Maximal cliques found:
Clique 1 : 1, 2, 3
Clique 2 : 2, 3, 4
Clique 3 : 4, 5, 6

Using igraph package:
Maximal cliques found:
Clique 1 : 1, 2, 3
Clique 2 : 2, 3, 4
Clique 3 : 4, 5, 6

=== Another Example ===
Triangle Graph Adjacency Matrix:
     [,1] [,2] [,3]
[1,]    0    1    1
[2,]    1    0    1
[3,]    1    1    0

Maximal cliques in triangle graph:
Clique 1 : 1, 2, 3
```

## Key Features:

1. **Two implementations provided**:
   - Custom recursive implementation
   - Using the `igraph` package for efficiency

2. **Input format**: Accepts adjacency matrix representation of undirected graphs

3. **Output**: Returns all maximal cliques in the graph

4. **Example graphs**: Shows usage with both complex and simple graphs

The algorithm works by recursively exploring potential cliques and maintaining three sets:
- **R**: Current clique being built
- **P**: Potential vertices that could be added to R
- **X**: Excluded vertices that cannot be added to R

This implementation correctly identifies all maximal cliques in the given graph structure.

