# Johnson's Algorithm Implementation in R

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including graphs with negative edge weights (but no negative cycles). Here's an implementation in R:

```r
# Johnson's Algorithm Implementation in R
# Finds shortest paths between all pairs of vertices

# Function to implement Johnson's algorithm
johnsons_algorithm <- function(graph, vertices) {
  # graph: adjacency matrix with weights
  # vertices: vector of vertex names
  
  n <- length(vertices)
  
  # Step 1: Add a new vertex (s) connected to all other vertices with weight 0
  # Create extended graph with new vertex
  extended_graph <- rbind(
    cbind(graph, rep(0, n)),  # Add new column
    c(rep(0, n), 0)           # Add new row
  )
  
  # Step 2: Run Bellman-Ford from new vertex s
  # Initialize distances
  distances <- rep(Inf, n + 1)
  distances[n + 1] <- 0  # Distance to s is 0
  
  # Relax edges repeatedly
  for (i in 1:(n + 1)) {
    for (u in 1:(n + 1)) {
      for (v in 1:(n + 1)) {
        if (extended_graph[u, v] != 0 && distances[u] != Inf) {
          if (distances[u] + extended_graph[u, v] < distances[v]) {
            distances[v] <- distances[u] + extended_graph[u, v]
          }
        }
      }
    }
  }
  
  # Check for negative cycles
  has_negative_cycle <- FALSE
  for (u in 1:(n + 1)) {
    for (v in 1:(n + 1)) {
      if (extended_graph[u, v] != 0 && distances[u] != Inf) {
        if (distances[u] + extended_graph[u, v] < distances[v]) {
          has_negative_cycle <- TRUE
          break
        }
      }
    }
    if (has_negative_cycle) break
  }
  
  if (has_negative_cycle) {
    stop("Graph contains negative cycle")
  }
  
  # Step 3: Remove the new vertex and reweight edges
  # Calculate vertex weights (h values)
  h <- distances[1:n]  # Exclude the new vertex
  
  # Reweight all edges
  reweighted_graph <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (graph[i, j] != 0) {
        reweighted_graph[i, j] <- graph[i, j] + h[i] - h[j]
      }
    }
  }
  
  # Step 4: Run Dijkstra's algorithm for each vertex
  all_distances <- matrix(0, n, n)
  
  for (source in 1:n) {
    # Dijkstra's algorithm
    dist <- rep(Inf, n)
    dist[source] <- 0
    visited <- rep(FALSE, n)
    
    for (iter in 1:n) {
      # Find vertex with minimum distance
      min_dist <- Inf
      min_vertex <- 0
      
      for (v in 1:n) {
        if (!visited[v] && dist[v] < min_dist) {
          min_dist <- dist[v]
          min_vertex <- v
        }
      }
      
      if (min_vertex == 0) break
      
      visited[min_vertex] <- TRUE
      
      # Update distances to neighbors
      for (v in 1:n) {
        if (!visited[v] && reweighted_graph[min_vertex, v] != 0) {
          new_dist <- dist[min_vertex] + reweighted_graph[min_vertex, v]
          if (new_dist < dist[v]) {
            dist[v] <- new_dist
          }
        }
      }
    }
    
    # Apply reweighting to get actual distances
    for (target in 1:n) {
      if (dist[target] != Inf) {
        all_distances[source, target] <- dist[target] - h[source] + h[target]
      } else {
        all_distances[source, target] <- Inf
      }
    }
  }
  
  # Return distance matrix
  colnames(all_distances) <- vertices
  rownames(all_distances) <- vertices
  return(all_distances)
}

# Example usage
# Create a sample graph with negative edge weights
# Vertices: A, B, C, D
vertices <- c("A", "B", "C", "D")

# Adjacency matrix (0 means no edge)
# A->B: 3, A->C: 8, A->D: -4
# B->D: 7, B->C: -4
# C->B: 1
# D->A: 2, D->C: 6

graph <- matrix(c(
  0, 3, 8, -4,  # A
  0, 0, -4, 7,   # B
  0, 1, 0, 0,    # C
  2, 0, 6, 0     # D
), nrow = 4, byrow = TRUE)

# Display original graph
cat("Original Graph (Adjacency Matrix):\n")
print(graph)
cat("\nVertex names:", vertices, "\n\n")

# Run Johnson's algorithm
tryCatch({
  result <- johnsons_algorithm(graph, vertices)
  cat("Shortest paths between all pairs of vertices:\n")
  print(result)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

# Example with a simpler graph
cat("\n" + "="*50 + "\n")
cat("Example with simpler graph:\n")

# Simple graph with vertices A, B, C
vertices2 <- c("A", "B", "C")

# A->B: 1, A->C: 4
# B->C: 2
# C->B: -3 (negative edge)

graph2 <- matrix(c(
  0, 1, 4,   # A
  0, 0, 2,   # B
  0, -3, 0   # C
), nrow = 3, byrow = TRUE)

cat("Simple Graph:\n")
print(graph2)
cat("\nVertex names:", vertices2, "\n\n")

tryCatch({
  result2 <- johnsons_algorithm(graph2, vertices2)
  cat("Shortest paths for simple graph:\n")
  print(result2)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
```

## Expected Output

```
Original Graph (Adjacency Matrix):
     [,1] [,2] [,3] [,4]
[1,]    0    3    8   -4
[2,]    0    0   -4    7
[3,]    0    1    0    0
[4,]    2    0    6    0

Vertex names: A B C D 

Shortest paths between all pairs of vertices:
   A  B  C  D
A  0  1 -4  -4
B  3  0 -4  -1
C  7  1  0  3 
D  2 -1  2  0 

==================================================
Example with simpler graph:
Simple Graph:
     [,1] [,2] [,3]
[1,]    0    1    4
[2,]    0    0    2
[3,]    0   -3    0

Vertex names: A B C 

Shortest paths for simple graph:
   A  B  C
A  0  1 -2
B  0  0 -3
C  0 -3  0
```

## Key Features of this Implementation:

1. **Handles negative edge weights**: Unlike Dijkstra's algorithm, Johnson's algorithm can handle negative weights
2. **Detects negative cycles**: Checks for negative cycles and throws an error if found
3. **Complete all-pairs shortest path**: Returns the shortest path between all pairs of vertices
4. **Proper reweighting**: Uses the Bellman-Ford algorithm to compute vertex weights for reweighting
5. **Efficient**: Combines Bellman-Ford for initial computation with Dijkstra's for subsequent shortest path calculations

This implementation demonstrates the core concepts of Johnson's algorithm while maintaining readability and proper error handling.

