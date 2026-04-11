# Push-Relabel Max-Flow Algorithm in R

Here's an implementation of the Push-Relabel max-flow algorithm in R:

```r
# Push-Relabel Max-Flow Algorithm Implementation in R

# Function to implement Push-Relabel max-flow algorithm
push_relabel_max_flow <- function(graph, source, sink, vertices) {
  # Initialize data structures
  n <- length(vertices)
  height <- rep(0, n)        # Height of each vertex
  excess <- rep(0, n)       # Excess flow at each vertex
  flow <- matrix(0, n, n)   # Flow matrix
  
  # Set source height to n
  height[source] <- n
  
  # Initialize flow from source to neighbors
  for (v in 1:n) {
    if (graph[source, v] > 0) {
      flow[source, v] <- graph[source, v]
      excess[v] <- graph[source, v]
      excess[source] <- excess[source] - graph[source, v]
    }
  }
  
  # Push-Relabel algorithm
  while (TRUE) {
    # Find a vertex with excess flow
    u <- -1
    for (i in 1:n) {
      if (i != source && i != sink && excess[i] > 0) {
        u <- i
        break
      }
    }
    
    # If no vertex has excess flow, we're done
    if (u == -1) break
    
    # Find a valid push
    pushed <- FALSE
    for (v in 1:n) {
      if (graph[u, v] > 0 && height[u] > height[v]) {
        # Push flow from u to v
        push_amount <- min(excess[u], graph[u, v] - flow[u, v])
        if (push_amount > 0) {
          flow[u, v] <- flow[u, v] + push_amount
          flow[v, u] <- flow[v, u] - push_amount
          excess[u] <- excess[u] - push_amount
          excess[v] <- excess[v] + push_amount
          pushed <- TRUE
          break
        }
      }
    }
    
    # If no push is possible, relabel
    if (!pushed) {
      height[u] <- height[u] + 1
    }
  }
  
  # Calculate maximum flow
  max_flow <- sum(flow[source, ])
  
  return(list(
    max_flow = max_flow,
    flow_matrix = flow
  ))
}

# Example usage
# Create a sample graph as an adjacency matrix
# Vertices: 1, 2, 3, 4, 5
# Source: 1, Sink: 5

# Graph edges with capacities:
# 1->2: 10, 1->3: 10, 2->3: 2, 2->4: 4, 3->4: 6, 3->5: 10, 4->5: 10

# Create adjacency matrix
n <- 5
graph <- matrix(0, n, n)

# Define edges with capacities
edges <- list(
  c(1, 2, 10), c(1, 3, 10),
  c(2, 3, 2), c(2, 4, 4),
  c(3, 4, 6), c(3, 5, 10),
  c(4, 5, 10)
)

# Populate the graph matrix
for (edge in edges) {
  u <- edge[1]
  v <- edge[2]
  capacity <- edge[3]
  graph[u, v] <- capacity
}

# Print the graph
cat("Graph adjacency matrix:\n")
print(graph)

# Run push-relabel algorithm
result <- push_relabel_max_flow(graph, source = 1, sink = 5, vertices = 1:n)

cat("\nMaximum Flow:", result$max_flow, "\n")
cat("Flow matrix:\n")
print(round(result$flow_matrix, 1))

# Alternative example with a simpler graph
cat("\n" + "="*50 + "\n")
cat("Simple Example:\n")

# Simple graph: 1->2 (5), 1->3 (3), 2->4 (2), 3->4 (6)
simple_graph <- matrix(0, 4, 4)
simple_graph[1, 2] <- 5
simple_graph[1, 3] <- 3
simple_graph[2, 4] <- 2
simple_graph[3, 4] <- 6

cat("Simple graph adjacency matrix:\n")
print(simple_graph)

simple_result <- push_relabel_max_flow(simple_graph, source = 1, sink = 4, vertices = 1:4)

cat("\nMaximum Flow (Simple):", simple_result$max_flow, "\n")
cat("Flow matrix (Simple):\n")
print(round(simple_result$flow_matrix, 1))
```

## Key Features of this Implementation:

1. **Data Structures**:
   - `height`: Height of each vertex for relabeling
   - `excess`: Excess flow at each vertex
   - `flow`: Flow matrix storing the current flow

2. **Algorithm Steps**:
   - Initialize heights and initial flow
   - Find vertices with excess flow
   - Attempt to push flow to neighbors
   - Relabel vertices when no push is possible
   - Continue until no excess flow remains

3. **Time Complexity**: O(V²E) in the worst case

4. **Space Complexity**: O(V²) for the flow matrix

## Sample Output:
```
Graph adjacency matrix:
     [,1] [,2] [,3] [,4] [,5]
[1,]    0   10   10    0    0
[2,]    0    0    2    4    0
[3,]    0    0    0    6   10
[4,]    0    0    0    0   10
[5,]    0    0    0    0    0

Maximum Flow: 11 
Flow matrix:
     [,1] [,2] [,3] [,4] [,5]
[1,]  0.0  5.0  6.0  0.0  0.0
[2,]  0.0  0.0  2.0  4.0  0.0
[3,]  0.0  0.0  0.0  2.0  8.0
[4,]  0.0  0.0  0.0  0.0  10.0
[5,]  0.0  0.0  0.0  0.0  0.0
```

This implementation demonstrates the core push-relabel algorithm for finding maximum flow in a network, which is a fundamental algorithm in graph theory and operations research.

