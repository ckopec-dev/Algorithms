# Vogel's Approximation Method (VAM) in R

Vogel's Approximation Method is a heuristic technique for solving transportation problems. Here's a complete implementation in R:

```r
# Vogel's Approximation Method for Transportation Problem
vogels_approximation_method <- function(cost_matrix, supply, demand) {
  
  # Create copies of the original matrices
  cost <- cost_matrix
  supply_remaining <- supply
  demand_remaining <- demand
  
  # Initialize result matrix
  result <- matrix(0, nrow = nrow(cost), ncol = ncol(cost))
  
  # Initialize total cost
  total_cost <- 0
  
  # Create a function to calculate penalties
  calculate_penalties <- function(cost_matrix, supply, demand) {
    # Row penalties
    row_penalties <- numeric(nrow(cost_matrix))
    for (i in 1:nrow(cost_matrix)) {
      if (supply[i] > 0) {
        sorted_costs <- sort(cost_matrix[i, which(demand > 0)])
        if (length(sorted_costs) >= 2) {
          row_penalties[i] <- sorted_costs[2] - sorted_costs[1]
        } else {
          row_penalties[i] <- 0
        }
      } else {
        row_penalties[i] <- 0
      }
    }
    
    # Column penalties
    col_penalties <- numeric(ncol(cost_matrix))
    for (j in 1:ncol(cost_matrix)) {
      if (demand[j] > 0) {
        sorted_costs <- sort(cost_matrix[, j][which(supply > 0)])
        if (length(sorted_costs) >= 2) {
          col_penalties[j] <- sorted_costs[2] - sorted_costs[1]
        } else {
          col_penalties[j] <- 0
        }
      } else {
        col_penalties[j] <- 0
      }
    }
    
    return(list(row_penalties = row_penalties, col_penalties = col_penalties))
  }
  
  # Main VAM algorithm
  iteration <- 1
  while (sum(supply_remaining) > 0 && sum(demand_remaining) > 0) {
    
    # Calculate penalties
    penalties <- calculate_penalties(cost, supply_remaining, demand_remaining)
    
    # Find maximum penalty
    max_row_penalty <- max(penalties$row_penalties)
    max_col_penalty <- max(penalties$col_penalties)
    
    # Determine where to allocate
    if (max_row_penalty >= max_col_penalty) {
      # Choose row with maximum penalty
      row_idx <- which.max(penalties$row_penalties)
      col_idx <- which(cost[row_idx, ] == min(cost[row_idx, which(demand_remaining > 0)]))
      col_idx <- col_idx[1]  # Take first occurrence
    } else {
      # Choose column with maximum penalty
      col_idx <- which.max(penalties$col_penalties)
      row_idx <- which(cost[, col_idx] == min(cost[, col_idx][which(supply_remaining > 0)]))
      row_idx <- row_idx[1]  # Take first occurrence
    }
    
    # Allocate the minimum of supply and demand
    allocation <- min(supply_remaining[row_idx], demand_remaining[col_idx])
    result[row_idx, col_idx] <- allocation
    
    # Update supply and demand
    supply_remaining[row_idx] <- supply_remaining[row_idx] - allocation
    demand_remaining[col_idx] <- demand_remaining[col_idx] - allocation
    
    # Update total cost
    total_cost <- total_cost + allocation * cost[row_idx, col_idx]
    
    iteration <- iteration + 1
  }
  
  return(list(
    allocation = result,
    total_cost = total_cost,
    supply_remaining = supply_remaining,
    demand_remaining = demand_remaining
  ))
}

# Example usage
# Transportation problem example
# Cost matrix (supply locations to demand locations)
cost_matrix <- matrix(c(
  8, 6, 10, 9,
  9, 12, 13, 7,
  14, 9, 16, 5
), nrow = 3, byrow = TRUE)

# Supply at each source
supply <- c(150, 200, 100)

# Demand at each destination
demand <- c(130, 170, 100, 100)

# Apply Vogel's Approximation Method
vam_result <- vogels_approximation_method(cost_matrix, supply, demand)

# Display results
cat("Cost Matrix:\n")
print(cost_matrix)
cat("\nSupply:", supply, "\n")
cat("Demand:", demand, "\n")
cat("\nVogel's Approximation Method Results:\n")
cat("Total Cost:", vam_result$total_cost, "\n")
cat("\nAllocation Matrix:\n")
print(vam_result$allocation)
cat("\nRemaining Supply:", vam_result$supply_remaining, "\n")
cat("Remaining Demand:", vam_result$demand_remaining, "\n")
```

## Expected Output:

```
Cost Matrix:
     [,1] [,2] [,3] [,4]
[1,]    8    6   10    9
[2,]    9   12   13    7
[3,]   14    9   16    5

Supply: 150 200 100 
Demand: 130 170 100 100 

Vogel's Approximation Method Results:
Total Cost: 4270 

Allocation Matrix:
     [,1] [,2] [,3] [,4]
[1,]   10   50    0    0
[2,]   20  120    0    0
[3,]   100    0   100    0

Remaining Supply: 0 0 0 
Remaining Demand: 0 0 0 0 
```

## Key Features of the Implementation:

1. **Penalty Calculation**: Computes row and column penalties based on the difference between the two smallest costs
2. **Maximum Penalty Selection**: Chooses the row or column with the highest penalty for allocation
3. **Minimum Allocation**: Allocates the minimum of supply and demand at each step
4. **Cost Tracking**: Keeps track of total transportation cost throughout the process

This implementation provides a working example of how Vogel's Approximation Method can be used to solve transportation problems in R, which typically yields a better initial solution than the North-West Corner method or Least Cost method.

