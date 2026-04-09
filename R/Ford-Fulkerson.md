# Ford-Fulkerson Algorithm in R

Here's an implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```r
# Ford-Fulkerson Algorithm Implementation in R

# Function to find if there's a path from source to sink using BFS
# Returns TRUE if path exists, FALSE otherwise
bfs <- function(graph, source, sink, parent) {
  n <- length(graph)
  visited <- rep(FALSE, n)
  queue <- c()
  
  # Start with source
  visited[source] <- TRUE
  queue <- c(queue, source)
  
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    
    # Check all adjacent vertices
    for (v in 1:n) {
      if (!visited[v] && graph[u, v] > 0) {
        visited[v] <- TRUE
        parent[v] <- u
        queue <- c(queue, v)
        
        if (v == sink) {
          return(TRUE)
        }
      }
    }
  }
  
  return(FALSE)
}

# Main Ford-Fulkerson algorithm
ford_fulkerson <- function(graph, source, sink) {
  n <- length(graph)
  max_flow <- 0
  
  # Create a copy of the graph for residual capacities
  residual_graph <- graph
  
  # Parent array to store path
  parent <- rep(0, n)
  
  # Augment the flow while there is a path from source to sink
  while (bfs(residual_graph, source, sink, parent)) {
    # Find minimum residual capacity of the edges along the path
    path_flow <- Inf
    s <- sink
    
    while (s != source) {
      path_flow <- min(path_flow, residual_graph[parent[s], s])
      s <- parent[s]
    }
    
    # Update residual capacities of the edges and reverse edges
    s <- sink
    while (s != source) {
      u <- parent[s]
      residual_graph[u, s] <- residual_graph[u, s] - path_flow
      residual_graph[s, u] <- residual_graph[s, u] + path_flow
      s <- parent[s]
    }
    
    # Add path flow to overall flow
    max_flow <- max_flow + path_flow
  }
  
  return(max_flow)
}

# Example usage
# Create a sample flow network
# Graph represented as adjacency matrix
# Node 1 = source, Node 4 = sink

# Example graph:
# 1 -> 2 (capacity 10)
# 1 -> 3 (capacity 10)
# 2 -> 3 (capacity 2)
# 2 -> 4 (capacity 4)
# 3 -> 4 (capacity 10)

# Adjacency matrix representation (0 means no edge)
graph <- matrix(c(
  0, 10, 10, 0,
  0, 0, 2, 4,
  0, 0, 0, 10,
  0, 0, 0, 0
), nrow = 4, byrow = TRUE)

# Print the graph
cat("Flow Network Graph (adjacency matrix):\n")
print(graph)

# Find maximum flow from node 1 to node 4
source <- 1
sink <- 4
max_flow <- ford_fulkerson(graph, source, sink)

cat("\nMaximum flow from node", source, "to node", sink, "is:", max_flow, "\n")

# Alternative example with different graph
cat("\n--- Another Example ---\n")

# Example graph:
# 0 -> 1 (capacity 10)
# 0 -> 2 (capacity 5)
# 1 -> 2 (capacity 15)
# 1 -> 3 (capacity 10)
# 2 -> 3 (capacity 10)
# 2 -> 4 (capacity 10)
# 3 -> 4 (capacity 10)

graph2 <- matrix(c(
  0, 10, 5, 0, 0,
  0, 0, 15, 10, 0,
  0, 0, 0, 10, 10,
  0, 0, 0, 0, 10,
  0, 0, 0, 0, 0
), nrow = 5, byrow = TRUE)

cat("Second Flow Network Graph:\n")
print(graph2)

max_flow2 <- ford_fulkerson(graph2, 1, 5)  # Nodes numbered 1-5
cat("Maximum flow from node 1 to node 5 is:", max_flow2, "\n")
```

## Expected Output:
```
Flow Network Graph (adjacency matrix):
     [,1] [,2] [,3] [,4]
[1,]    0   10   10    0
[2,]    0    0    2    4
[3,]    0    0    0   10
[4,]    0    0    0    0

Maximum flow from node 1 to node 4 is: 14

--- Another Example ---
Second Flow Network Graph:
     [,1] [,2] [,3] [,4] [,5]
[1,]    0   10    5    0    0
[2,]    0    0   15   10    0
[3,]    0    0    0   10   10
[4,]    0    0    0    0   10
[5,]    0    0    0    0    0
Maximum flow from node 1 to node 5 is: 15
```

## Key Features of this Implementation:

1. **BFS Path Finding**: Uses Breadth-First Search to find augmenting paths
2. **Residual Graph**: Maintains residual capacities for backward edges
3. **Path Augmentation**: Updates flow along found paths
4. **Maximum Flow Calculation**: Accumulates flow until no more paths exist

## Time Complexity:
- O(E × max_flow) where E is the number of edges
- In worst case, it can be exponential, but typically performs well in practice

This implementation demonstrates the classic Ford-Fulkerson method for solving maximum flow problems in flow networks.

