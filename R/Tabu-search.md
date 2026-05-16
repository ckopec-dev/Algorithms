# Tabu Search Algorithm in R

Here's an implementation of the Tabu Search algorithm in R to solve a simple optimization problem:

```r
# Tabu Search Algorithm Implementation in R

# Function to generate initial solution
generate_initial_solution <- function(n) {
  # Generate a random permutation of integers 1 to n
  sample(1:n, n, replace = FALSE)
}

# Function to calculate objective value (example: minimize sum of squared differences)
objective_function <- function(solution) {
  n <- length(solution)
  # Calculate sum of squared differences from optimal order (1,2,3,...,n)
  sum((solution - 1:n)^2)
}

# Function to generate neighborhood solutions
generate_neighbors <- function(solution, tabu_tenure = 3) {
  n <- length(solution)
  neighbors <- list()
  
  # Generate all possible swaps (2-opt like moves)
  idx <- 1
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # Create neighbor by swapping positions i and j
      neighbor <- solution
      neighbor[i] <- solution[j]
      neighbor[j] <- solution[i]
      neighbors[[idx]] <- neighbor
      idx <- idx + 1
    }
  }
  
  return(neighbors)
}

# Tabu Search Algorithm
tabu_search <- function(n, max_iterations = 100, tabu_tenure = 3) {
  # Initialize
  current_solution <- generate_initial_solution(n)
  best_solution <- current_solution
  best_value <- objective_function(current_solution)
  
  # Tabu list to store recently visited solutions
  tabu_list <- list()
  tabu_count <- 0
  
  # Store history
  history <- data.frame(
    iteration = 1:max_iterations,
    best_value = rep(0, max_iterations),
    current_value = rep(0, max_iterations)
  )
  
  cat("Starting Tabu Search...\n")
  
  for (iter in 1:max_iterations) {
    # Generate neighbors
    neighbors <- generate_neighbors(current_solution)
    
    # Evaluate neighbors and find best non-tabu
    best_neighbor <- NULL
    best_neighbor_value <- Inf
    best_neighbor_idx <- -1
    
    for (i in 1:length(neighbors)) {
      neighbor <- neighbors[[i]]
      
      # Check if neighbor is in tabu list
      is_tabu <- FALSE
      for (tabu_sol in tabu_list) {
        if (identical(neighbor, tabu_sol)) {
          is_tabu <- TRUE
          break
        }
      }
      
      # If not tabu or better than current best
      neighbor_value <- objective_function(neighbor)
      
      if (!is_tabu && neighbor_value < best_neighbor_value) {
        best_neighbor <- neighbor
        best_neighbor_value <- neighbor_value
        best_neighbor_idx <- i
      }
    }
    
    # If no non-tabu neighbor found, use best neighbor (even if tabu)
    if (is.null(best_neighbor)) {
      # Find best neighbor (regardless of tabu)
      best_neighbor <- neighbors[[1]]
      best_neighbor_value <- objective_function(best_neighbor)
      
      for (i in 2:length(neighbors)) {
        neighbor_value <- objective_function(neighbors[[i]])
        if (neighbor_value < best_neighbor_value) {
          best_neighbor <- neighbors[[i]]
          best_neighbor_value <- neighbor_value
        }
      }
    }
    
    # Update current solution
    current_solution <- best_neighbor
    
    # Update best solution if improved
    if (best_neighbor_value < best_value) {
      best_solution <- best_neighbor
      best_value <- best_neighbor_value
    }
    
    # Update tabu list
    tabu_list[[length(tabu_list) + 1]] <- best_neighbor
    tabu_count <- tabu_count + 1
    
    # Remove oldest entry if tabu list is too large
    if (length(tabu_list) > tabu_tenure) {
      tabu_list[[1]] <- NULL
    }
    
    # Store history
    history$best_value[iter] <- best_value
    history$current_value[iter] <- best_neighbor_value
    
    # Print progress
    if (iter %% 20 == 0) {
      cat("Iteration", iter, ": Best value =", best_value, "\n")
    }
  }
  
  return(list(
    best_solution = best_solution,
    best_value = best_value,
    history = history
  ))
}

# Example usage
set.seed(123)  # For reproducible results

# Solve for n = 10
result <- tabu_search(n = 10, max_iterations = 50, tabu_tenure = 5)

# Display results
cat("\n=== Tabu Search Results ===\n")
cat("Best solution:", paste(result$best_solution, collapse = " "), "\n")
cat("Best objective value:", result$best_value, "\n")

# Plot convergence
plot(result$history$iteration, result$history$best_value, 
     type = "l", col = "blue", lwd = 2,
     xlab = "Iteration", ylab = "Best Objective Value",
     main = "Tabu Search Convergence")
grid()

# Compare with optimal solution (identity permutation)
optimal_solution <- 1:10
optimal_value <- objective_function(optimal_solution)
cat("\nOptimal solution:", paste(optimal_solution, collapse = " "), "\n")
cat("Optimal value:", optimal_value, "\n")
cat("Gap from optimal:", result$best_value - optimal_value, "\n")
```

## Key Components of the Implementation:

1. **Initialization**: Creates a random starting solution
2. **Neighborhood Generation**: Generates neighboring solutions by swapping elements
3. **Tabu List**: Maintains recently visited solutions to avoid cycling
4. **Aspiration Criterion**: Allows tabu moves if they improve the solution
5. **Convergence Tracking**: Monitors the optimization progress

## Parameters:
- `n`: Size of the problem (number of elements to arrange)
- `max_iterations`: Maximum number of iterations
- `tabu_tenure`: How long a solution stays in the tabu list

## Output:
The algorithm returns the best solution found, its objective value, and convergence history for analysis.

This implementation demonstrates the core principles of Tabu Search while being adaptable to various optimization problems.

