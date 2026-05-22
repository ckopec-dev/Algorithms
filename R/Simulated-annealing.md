# Simulated Annealing Algorithm in R

Here's a complete example of implementing the Simulated Annealing algorithm in R to solve the Traveling Salesman Problem (TSP):

```r
# Simulated Annealing for Traveling Salesman Problem
# Install required packages if needed
# install.packages(c("ggplot2", "reshape2"))

library(ggplot2)
library(reshape2)

# Generate random cities
set.seed(123)
n_cities <- 20
cities <- data.frame(
  x = runif(n_cities, 0, 100),
  y = runif(n_cities, 0, 100),
  id = 1:n_cities
)

# Calculate distance between two cities
distance <- function(city1, city2) {
  sqrt((city1$x - city2$x)^2 + (city1$y - city2$y)^2)
}

# Calculate total tour distance
tour_distance <- function(tour, cities) {
  total_dist <- 0
  n <- length(tour)
  
  for (i in 1:(n-1)) {
    total_dist <- total_dist + distance(cities[tour[i],], cities[tour[i+1],])
  }
  
  # Return to starting city
  total_dist <- total_dist + distance(cities[tour[n],], cities[tour[1],])
  return(total_dist)
}

# Generate initial random tour
generate_initial_tour <- function(n_cities) {
  return(sample(1:n_cities, n_cities, replace = FALSE))
}

# Generate neighbor tour by swapping two random cities
generate_neighbor <- function(tour) {
  new_tour <- tour
  i <- sample(1:length(tour), 2, replace = FALSE)
  # Swap two random cities
  temp <- new_tour[i[1]]
  new_tour[i[1]] <- new_tour[i[2]]
  new_tour[i[2]] <- temp
  return(new_tour)
}

# Simulated Annealing algorithm
simulated_annealing <- function(cities, max_iter = 10000, initial_temp = 1000, 
                               cooling_rate = 0.995, min_temp = 1e-8) {
  
  # Initialize
  current_tour <- generate_initial_tour(nrow(cities))
  current_distance <- tour_distance(current_tour, cities)
  
  best_tour <- current_tour
  best_distance <- current_distance
  
  # Store history for plotting
  history <- data.frame(iteration = 1, distance = current_distance)
  
  temp <- initial_temp
  
  for (iter in 1:max_iter) {
    # Generate neighbor solution
    neighbor_tour <- generate_neighbor(current_tour)
    neighbor_distance <- tour_distance(neighbor_tour, cities)
    
    # Calculate energy difference
    delta <- neighbor_distance - current_distance
    
    # Accept or reject the neighbor
    if (delta < 0 || runif(1) < exp(-delta / temp)) {
      current_tour <- neighbor_tour
      current_distance <- neighbor_distance
      
      # Update best solution if improved
      if (current_distance < best_distance) {
        best_tour <- current_tour
        best_distance <- current_distance
      }
    }
    
    # Cool down the temperature
    temp <- temp * cooling_rate
    
    # Store history
    if (iter %% 100 == 0) {
      history <- rbind(history, data.frame(iteration = iter, distance = current_distance))
    }
    
    # Stop if temperature is too low
    if (temp < min_temp) break
  }
  
  return(list(
    best_tour = best_tour,
    best_distance = best_distance,
    history = history
  ))
}

# Run simulated annealing
result <- simulated_annealing(cities, max_iter = 5000)

# Display results
cat("Best tour distance:", round(result$best_distance, 2), "\n")
cat("Number of cities:", n_cities, "\n")

# Plot the cities and best tour
plot_tour <- function(cities, tour) {
  # Create data frame for plotting
  tour_df <- cities[tour, ]
  tour_df <- rbind(tour_df, tour_df[1, ])  # Close the loop
  
  # Plot cities and tour
  p <- ggplot() +
    geom_point(data = cities, aes(x = x, y = y), size = 3, color = "blue") +
    geom_line(data = tour_df, aes(x = x, y = y), color = "red", size = 1) +
    geom_text(data = cities, aes(x = x, y = y, label = id), vjust = -0.5, size = 3) +
    labs(title = paste("Simulated Annealing TSP Solution (Distance:", 
                       round(result$best_distance, 2), ")"),
         x = "X Coordinate", y = "Y Coordinate") +
    theme_minimal()
  
  return(p)
}

# Plot the result
plot_tour(cities, result$best_tour)

# Plot convergence history
plot_convergence <- function(history) {
  p <- ggplot(history, aes(x = iteration, y = distance)) +
    geom_line(color = "blue") +
    labs(title = "Simulated Annealing Convergence",
         x = "Iteration", y = "Tour Distance") +
    theme_minimal()
  
  return(p)
}

# Plot convergence
plot_convergence(result$history)

# Compare with random solution
random_tour <- generate_initial_tour(n_cities)
random_distance <- tour_distance(random_tour, cities)

cat("\nComparison:\n")
cat("Random solution distance:", round(random_distance, 2), "\n")
cat("SA solution distance:", round(result$best_distance, 2), "\n")
cat("Improvement:", round((random_distance - result$best_distance) / random_distance * 100, 2), "%\n")
```

## Key Components of the Implementation:

### 1. **Problem Setup**
- Randomly generated cities in 2D space
- Distance calculation between cities
- Tour distance calculation (including return to start)

### 2. **Algorithm Components**
- **Initial solution**: Random tour through all cities
- **Neighbor generation**: Swap two random cities in the tour
- **Acceptance criterion**: Metropolis criterion based on temperature
- **Cooling schedule**: Exponential cooling with rate 0.995

### 3. **Parameters**
- `max_iter`: Maximum number of iterations
- `initial_temp`: Starting temperature
- `cooling_rate`: Temperature reduction factor
- `min_temp`: Minimum temperature threshold

### 4. **Output**
- Best tour found
- Distance of best tour
- Convergence plot
- Comparison with random solution

This implementation demonstrates how Simulated Annealing can effectively find near-optimal solutions to the TSP by gradually reducing randomness (temperature) while accepting worse solutions with decreasing probability, allowing the algorithm to escape local optima and converge toward better solutions.

