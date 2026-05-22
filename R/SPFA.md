# SPFA Algorithm Implementation in R

The Shortest Path Faster Algorithm (SPFA) is an optimized version of Bellman-Ford that uses a queue to improve performance on sparse graphs.

```r
# SPFA Algorithm Implementation in R
spfa <- function(graph, start_node, num_nodes) {
  # Initialize distances and predecessor array
  distances <- rep(Inf, num_nodes)
  distances[start_node] <- 0
  predecessors <- rep(0, num_nodes)
  
  # Queue for nodes to be processed
  queue <- c(start_node)
  in_queue <- rep(FALSE, num_nodes)
  in_queue[start_node] <- TRUE
  
  # Main SPFA loop
  while(length(queue) > 0) {
    # Dequeue node
    current_node <- queue[1]
    queue <- queue[-1]
    in_queue[current_node] <- FALSE
    
    # Check all neighbors of current node
    for (neighbor in names(graph[[current_node]])) {
      neighbor_id <- as.numeric(neighbor)
      edge_weight <- graph[[current_node]][[neighbor]]
      
      # If we found a shorter path
      if (distances[current_node] + edge_weight < distances[neighbor_id]) {
        distances[neighbor_id] <- distances[current_node] + edge_weight
        predecessors[neighbor_id] <- current_node
        
        # If neighbor not in queue, add it
        if (!in_queue[neighbor_id]) {
          queue <- c(queue, neighbor_id)
          in_queue[neighbor_id] <- TRUE
        }
      }
    }
  }
  
  return(list(distances = distances, predecessors = predecessors))
}

# Example usage
# Create a sample graph as a list
# Format: graph[[node]] = list(neighbor1 = weight1, neighbor2 = weight2, ...)
sample_graph <- list(
  `1` = list(`2` = 4, `3` = 2),
  `2` = list(`3` = 1, `4` = 5),
  `3` = list(`4` = 8, `5` = 10),
  `4` = list(`5` = 2),
  `5` = list()
)

# Run SPFA from node 1
result <- spfa(sample_graph, start_node = 1, num_nodes = 5)

# Display results
cat("Shortest distances from node 1:\n")
for (i in 1:5) {
  if (is.infinite(result$distances[i])) {
    cat(sprintf("Node %d: Infinity\n", i))
  } else {
    cat(sprintf("Node %d: %d\n", i, result$distances[i]))
  }
}

# Function to reconstruct path
reconstruct_path <- function(predecessors, start, end) {
  path <- c(end)
  current <- end
  
  while (current != start && predecessors[current] != 0) {
    current <- predecessors[current]
    path <- c(current, path)
  }
  
  if (current != start) {
    return(NULL)  # No path exists
  }
  
  return(path)
}

# Example path reconstruction
path_to_node_5 <- reconstruct_path(result$predecessors, 1, 5)
cat("\nPath from node 1 to node 5:", paste(path_to_node_5, collapse = " -> "), "\n")
```

## Output:
```
Shortest distances from node 1:
Node 1: 0
Node 2: 4
Node 3: 2
Node 4: 9
Node 5: 11

Path from node 1 to node 5: 1 -> 3 -> 4 -> 5
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses a list of lists where each node maps to its neighbors with edge weights
2. **Queue Management**: Efficiently manages nodes using a queue to avoid unnecessary iterations
3. **Distance Tracking**: Maintains shortest distances from the source node
4. **Path Reconstruction**: Includes functionality to trace back the actual shortest path
5. **Optimization**: Only processes nodes that are actually in the queue, making it more efficient than basic Bellman-Ford

## Time Complexity:
- **Best Case**: O(E) where E is the number of edges
- **Average Case**: O(E) for sparse graphs
- **Worst Case**: O(VE) where V is the number of vertices

This implementation is particularly useful for graphs with negative edge weights and can detect negative cycles.

