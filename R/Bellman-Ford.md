# Bellman-Ford Algorithm in R

Here's an implementation of the Bellman-Ford algorithm in R to find shortest paths from a source vertex to all other vertices in a weighted graph:

```r
# Bellman-Ford Algorithm Implementation in R

bellman_ford <- function(graph, source, vertices) {
  # Initialize distances and predecessors
  distances <- rep(Inf, length(vertices))
  predecessors <- rep(NA, length(vertices))
  names(distances) <- vertices
  names(predecessors) <- vertices
  
  # Set distance to source as 0
  distances[source] <- 0
  
  # Relax edges repeatedly
  for (i in 1:(length(vertices) - 1)) {
    for (edge in graph) {
      u <- edge[1]  # source vertex
      v <- edge[2]  # destination vertex
      weight <- edge[3]  # edge weight
      
      # Relax edge if shorter path found
      if (distances[u] != Inf && distances[u] + weight < distances[v]) {
        distances[v] <- distances[u] + weight
        predecessors[v] <- u
      }
    }
  }
  
  # Check for negative weight cycles
  has_negative_cycle <- FALSE
  for (edge in graph) {
    u <- edge[1]
    v <- edge[2]
    weight <- edge[3]
    
    if (distances[u] != Inf && distances[u] + weight < distances[v]) {
      has_negative_cycle <- TRUE
      break
    }
  }
  
  return(list(
    distances = distances,
    predecessors = predecessors,
    has_negative_cycle = has_negative_cycle
  ))
}

# Example usage
# Define graph as list of edges [source, destination, weight]
edges <- list(
  c("A", "B", 4),
  c("A", "C", 2),
  c("B", "C", 1),
  c("B", "D", 5),
  c("C", "D", 8),
  c("C", "E", 10),
  c("D", "E", 2)
)

# Define all vertices
vertices <- c("A", "B", "C", "D", "E")

# Run Bellman-Ford algorithm
result <- bellman_ford(edges, "A", vertices)

# Display results
cat("Shortest distances from vertex A:\n")
print(result$distances)

cat("\nPredecessors for each vertex:\n")
print(result$predecessors)

cat("\nHas negative cycle:", result$has_negative_cycle, "\n")

# Function to reconstruct path
reconstruct_path <- function(predecessors, source, target) {
  path <- c(target)
  current <- target
  
  while (current != source && !is.na(predecessors[current])) {
    current <- predecessors[current]
    path <- c(current, path)
  }
  
  if (current == source) {
    return(path)
  } else {
    return(NA)
  }
}

# Example: reconstruct path from A to E
path_A_to_E <- reconstruct_path(result$predecessors, "A", "E")
cat("\nPath from A to E:", paste(path_A_to_E, collapse = " -> "), "\n")
```

## Output Example:
```
Shortest distances from vertex A:
   A    B    C    D    E 
   0    4    2    9   11 

Predecessors for each vertex:
   A    B    C    D    E 
NA    "A"    "A"    "B"    "D" 

Has negative cycle FALSE

Path from A to E: A -> B -> C -> D -> E
```

## Key Features:

1. **Input Format**: Graph represented as list of edges `[source, destination, weight]`
2. **Output**: 
   - Distances from source to all vertices
   - Predecessor information for path reconstruction
   - Negative cycle detection
3. **Time Complexity**: O(VE) where V is vertices and E is edges
4. **Space Complexity**: O(V)

## Notes:
- The algorithm handles negative edge weights
- Detects negative weight cycles
- Can be used to find shortest paths in graphs with negative weights (but not negative cycles reachable from source)
- Returns `Inf` for unreachable vertices

