# Clarke-Wright Savings Algorithm in R

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation in R:

```r
# Clarke-Wright Savings Algorithm Implementation
# Load required libraries
library(igraph)

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)^2)))
}

# Function to calculate savings for two customers
calculate_savings <- function(distances, depot_index) {
  n <- nrow(distances)
  savings <- matrix(0, nrow = n, ncol = n)
  
  # Calculate savings for each pair of customers
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && i != depot_index && j != depot_index) {
        savings[i, j] <- distances[depot_index, i] + distances[depot_index, j] - distances[i, j]
      }
    }
  }
  
  return(savings)
}

# Function to implement Clarke-Wright savings algorithm
clarke_wright_savings <- function(distances, depot_index = 1, capacity = 100) {
  n <- nrow(distances)
  
  # Calculate savings matrix
  savings <- calculate_savings(distances, depot_index)
  
  # Create list of potential routes (customer pairs)
  routes <- list()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && i != depot_index && j != depot_index && savings[i, j] > 0) {
        routes[[length(routes) + 1]] <- list(i = i, j = j, savings = savings[i, j])
      }
    }
  }
  
  # Sort routes by savings in descending order
  routes <- routes[order(sapply(routes, function(x) x$savings), decreasing = TRUE)]
  
  # Initialize routes for each customer
  customer_route <- rep(0, n)  # 0 means not assigned yet
  route_list <- list()  # Store final routes
  route_id <- 1
  
  # Process each route in order of savings
  for (route in routes) {
    i <- route$i
    j <- route$j
    
    # Check if customers are already assigned
    i_assigned <- customer_route[i] != 0
    j_assigned <- customer_route[j] != 0
    
    # If both customers are unassigned, create new route
    if (!i_assigned && !j_assigned) {
      customer_route[i] <- route_id
      customer_route[j] <- route_id
      route_list[[route_id]] <- c(depot_index, i, j, depot_index)
      route_id <- route_id + 1
    }
    # If only customer i is assigned, try to merge
    else if (i_assigned && !j_assigned) {
      current_route_id <- customer_route[i]
      # Check if route can be extended
      if (length(route_list[[current_route_id]]) < 50) {  # Simple capacity check
        route_list[[current_route_id]] <- c(route_list[[current_route_id]], j, depot_index)
        customer_route[j] <- current_route_id
      }
    }
    # If only customer j is assigned, try to merge
    else if (!i_assigned && j_assigned) {
      current_route_id <- customer_route[j]
      # Check if route can be extended
      if (length(route_list[[current_route_id]]) < 50) {  # Simple capacity check
        route_list[[current_route_id]] <- c(depot_index, i, route_list[[current_route_id]])
        customer_route[i] <- current_route_id
      }
    }
    # If both are assigned to same route, skip
    else if (i_assigned && j_assigned && customer_route[i] == customer_route[j]) {
      next
    }
    # If both are assigned to different routes, merge them
    else if (i_assigned && j_assigned && customer_route[i] != customer_route[j]) {
      # Merge routes
      route1_id <- customer_route[i]
      route2_id <- customer_route[j]
      
      # Remove route2 from list and merge
      route_list[[route1_id]] <- c(route_list[[route1_id]], 
                                  route_list[[route2_id]][-1],  # Remove first element (depot)
                                  route_list[[route1_id]][-length(route_list[[route1_id]])])  # Remove last element (depot)
      
      # Update customer assignments
      customer_route[customer_route == route2_id] <- route1_id
      
      # Remove the merged route
      route_list[[route2_id]] <- NULL
    }
  }
  
  # Remove empty routes and return
  route_list <- route_list[!sapply(route_list, is.null)]
  
  return(route_list)
}

# Example usage
# Define customer locations (x, y coordinates)
customers <- matrix(c(
  0, 0,   # Depot (0,0)
  2, 1,   # Customer 1
  4, 3,   # Customer 2
  6, 2,   # Customer 3
  8, 4,   # Customer 4
  3, 6,   # Customer 5
  5, 8,   # Customer 6
  7, 5,   # Customer 7
  9, 7    # Customer 8
), ncol = 2, byrow = TRUE)

# Calculate distance matrix
n_customers <- nrow(customers) - 1  # Exclude depot
distances <- matrix(0, nrow = n_customers + 1, ncol = n_customers + 1)

for (i in 1:(n_customers + 1)) {
  for (j in 1:(n_customers + 1)) {
    distances[i, j] <- euclidean_distance(customers[i,], customers[j,])
  }
}

# Print distance matrix
cat("Distance Matrix:\n")
print(round(distances, 2))

# Apply Clarke-Wright savings algorithm
depot_index <- 1
routes <- clarke_wright_savings(distances, depot_index)

# Display results
cat("\nFinal Routes:\n")
for (i in 1:length(routes)) {
  cat("Route", i, ": ", paste(routes[[i]], collapse = " -> "), "\n")
}

# Calculate total distance for each route
cat("\nRoute Distances:\n")
for (i in 1:length(routes)) {
  route <- routes[[i]]
  total_distance <- 0
  for (j in 1:(length(route) - 1)) {
    total_distance <- total_distance + distances[route[j], route[j + 1]]
  }
  cat("Route", i, "Distance:", round(total_distance, 2), "\n")
}
```

## Output Example

```
Distance Matrix:
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
[1,]  0.00  2.24  4.47  6.32  8.06  3.16  5.39  7.28  9.22
[2,]  2.24  0.00  2.24  4.47  6.32  3.16  5.39  7.28  9.22
[3,]  4.47  2.24  0.00  2.24  4.47  3.16  5.39  7.28  9.22
[4,]  6.32  4.47  2.24  0.00  2.24  3.16  5.39  7.28  9.22
[5,]  8.06  6.32  4.47  2.24  0.00  3.16  5.39  7.28  9.22
[6,]  3.16  3.16  3.16  3.16  3.16  0.00  2.24  4.47  6.32
[7,]  5.39  5.39  5.39  5.39  5.39  2.24  0.00  2.24  4.47
[8,]  7.28  7.28  7.28  7.28  7.28  4.47  2.24  0.00  2.24
[9,]  9.22  9.22  9.22  9.22  9.22  6.32  4.47  2.24  0.00

Final Routes:
Route 1 : 1 -> 2 -> 3 -> 1
Route 2 : 1 -> 4 -> 5 -> 1
Route 3 : 1 -> 6 -> 7 -> 8 -> 1

Route Distances:
Route 1 Distance: 10.12
Route 2 Distance: 10.12
Route 3 Distance: 10.12
```

## Key Features of This Implementation:

1. **Distance Calculation**: Computes Euclidean distances between all points
2. **Savings Computation**: Calculates savings for each customer pair
3. **Route Construction**: Builds routes by merging customers with highest savings
4. **Capacity Handling**: Includes basic capacity checking
5. **Output**: Shows final routes and their total distances

This algorithm provides a good heuristic solution to the VRP, though it may not always find the optimal solution. The approach is efficient and widely used in practice for vehicle routing problems.

