# Floyd-Warshall Algorithm in R

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. Here's an implementation in R:

```r
# Floyd-Warshall Algorithm Implementation in R

floyd_warshall <- function(graph) {
  # Get the number of vertices
  n <- nrow(graph)
  
  # Create a copy of the graph for distance matrix
  dist <- graph
  
  # Initialize distance matrix with infinity for missing edges
  dist[dist == 0 & !is.na(dist)] <- Inf
  
  # Set diagonal to 0 (distance from vertex to itself)
  diag(dist) <- 0
  
  # Floyd-Warshall algorithm
  for (k in 1:n) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (dist[i, k] + dist[k, j] < dist[i, j]) {
          dist[i, j] <- dist[i, k] + dist[k, j]
        }
      }
    }
  }
  
  return(dist)
}

# Example usage
# Create a sample adjacency matrix (weighted graph)
# 0 means no direct edge, positive numbers are edge weights
# Note: We'll use a symmetric matrix for an undirected graph

# Example graph with 4 vertices
adjacency_matrix <- matrix(c(
  0, 3, Inf, 7,
  3, 0, 2, Inf,
  Inf, 2, 0, 1,
  7, Inf, 1, 0
), nrow = 4, byrow = TRUE)

# Print original adjacency matrix
cat("Original adjacency matrix:\n")
print(adjacency_matrix)

# Apply Floyd-Warshall algorithm
shortest_distances <- floyd_warshall(adjacency_matrix)

# Print result
cat("\nShortest distances between all pairs of vertices:\n")
print(shortest_distances)

# Function to print results in a more readable format
print_results <- function(dist_matrix) {
  n <- nrow(dist_matrix)
  cat("\nDistance matrix (row = source, column = destination):\n")
  for (i in 1:n) {
    for (j in 1:n) {
      if (dist_matrix[i, j] == Inf) {
        cat("Inf\t")
      } else {
        cat(sprintf("%3.0f\t", dist_matrix[i, j]))
      }
    }
    cat("\n")
  }
}

# Display results in a cleaner format
print_results(shortest_distances)
```

## Expected Output:
```
Original adjacency matrix:
     [,1] [,2] [,3] [,4]
[1,]    0    3  Inf    7
[2,]    3    0    2  Inf
[3,]  Inf    2    0    1
[4,]    7  Inf    1    0

Shortest distances between all pairs of vertices:
     [,1] [,2] [,3] [,4]
[1,]    0    3    5    6
[2,]    3    0    2    3
[3,]    5    2    0    1
[4,]    6    3    1    0

Distance matrix (row = source, column = destination):
  0   3   5   6 
  3   0   2   3 
  5   2   0   1 
  6   3   1   0 
```

## Key Features of this Implementation:

1. **Input**: Takes an adjacency matrix where `0` represents no direct edge
2. **Output**: Returns a matrix of shortest distances between all pairs of vertices
3. **Handles**: Negative weights (though not demonstrated here)
4. **Time Complexity**: O(n³) where n is the number of vertices
5. **Space Complexity**: O(n²) for the distance matrix

## How it works:
1. Initialize distance matrix with the input graph
2. For each intermediate vertex `k`, check if going through `k` provides a shorter path
3. Update the distance matrix accordingly
4. After all iterations, the matrix contains shortest paths between all pairs

This algorithm is particularly useful for finding transitive relationships in graphs and is widely used in network analysis and routing problems.

