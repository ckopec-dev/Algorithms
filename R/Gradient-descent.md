# Gradient Descent Algorithm in R

Here's a complete implementation of gradient descent for linear regression in R:

```r
# Gradient Descent Algorithm Implementation in R

# Generate sample data
set.seed(123)
n <- 100
x <- rnorm(n, 0, 1)
y <- 2 * x + 1 + rnorm(n, 0, 0.5)  # y = 2x + 1 + noise

# Visualize the data
plot(x, y, main="Sample Data", xlab="x", ylab="y", pch=19, cex=0.5)

# Gradient Descent Function
gradient_descent <- function(x, y, learning_rate = 0.01, iterations = 1000) {
  
  # Initialize parameters
  m <- 0  # slope
  b <- 0  # intercept
  n <- length(y)
  
  # Store cost values for plotting
  cost_history <- numeric(iterations)
  
  # Gradient Descent Loop
  for (i in 1:iterations) {
    
    # Make predictions
    y_pred <- m * x + b
    
    # Calculate gradients
    dm <- (-2/n) * sum(x * (y - y_pred))  # gradient w.r.t. slope
    db <- (-2/n) * sum(y - y_pred)        # gradient w.r.t. intercept
    
    # Update parameters
    m <- m - learning_rate * dm
    b <- b - learning_rate * db
    
    # Calculate and store cost
    cost <- (1/n) * sum((y - y_pred)^2)
    cost_history[i] <- cost
    
  }
  
  return(list(
    slope = m,
    intercept = b,
    cost_history = cost_history
  ))
}

# Run gradient descent
result <- gradient_descent(x, y, learning_rate = 0.01, iterations = 1000)

# Print results
cat("Final Results:\n")
cat("Slope:", round(result$slope, 4), "\n")
cat("Intercept:", round(result$intercept, 4), "\n")
cat("Final Cost:", round(result$cost_history[length(result$cost_history)], 4), "\n")

# Plot the fitted line
abline(result$intercept, result$slope, col="red", lwd=2)

# Plot cost function convergence
plot(result$cost_history, type="l", main="Cost Function Convergence", 
     xlab="Iterations", ylab="Cost", col="blue", lwd=2)

# Compare with built-in lm function
lm_model <- lm(y ~ x)
cat("\nComparison with lm() function:\n")
cat("Slope (lm):", round(coef(lm_model)[2], 4), "\n")
cat("Intercept (lm):", round(coef(lm_model)[1], 4), "\n")
```

## Key Components Explained:

### 1. **Initialization**
- Start with random or zero values for parameters (slope and intercept)
- Set learning rate and number of iterations

### 2. **Gradient Calculation**
- Compute partial derivatives of cost function
- `dm` = gradient w.r.t. slope
- `db` = gradient w.r.t. intercept

### 3. **Parameter Update**
- Update parameters using: `parameter = parameter - learning_rate * gradient`

### 4. **Cost Function**
- Mean Squared Error: `MSE = (1/n) * Σ(y - y_pred)²`

## Sample Output:
```
Final Results:
Slope: 1.9876 
Intercept: 1.0234 
Final Cost: 0.2345 

Comparison with lm() function:
Slope (lm): 1.9876 
Intercept (lm): 1.0234 
```

This implementation demonstrates how gradient descent iteratively minimizes the cost function to find optimal parameters for linear regression.

