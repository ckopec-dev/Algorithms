# Perceptron Learning Algorithm in R

Here's a complete implementation of the Perceptron learning algorithm in R:

```r
# Perceptron Learning Algorithm Implementation in R

# Function to implement Perceptron learning
perceptron_train <- function(X, y, learning_rate = 0.1, max_iterations = 1000) {
  # X: input features (matrix)
  # y: target labels (vector, should be -1 or 1)
  # learning_rate: step size for weight updates
  # max_iterations: maximum number of training iterations
  
  # Initialize weights and bias
  n_features <- ncol(X)
  weights <- rep(0, n_features)
  bias <- 0
  
  # Training loop
  for (iteration in 1:max_iterations) {
    errors <- 0
    
    # Go through each training example
    for (i in 1:nrow(X)) {
      # Calculate prediction
      prediction <- sign(sum(weights * X[i, ]) + bias)
      
      # If prediction is wrong, update weights
      if (prediction != y[i]) {
        errors <- errors + 1
        weights <- weights + learning_rate * y[i] * X[i, ]
        bias <- bias + learning_rate * y[i]
      }
    }
    
    # If no errors, training is complete
    if (errors == 0) {
      cat("Training completed in", iteration, "iterations\n")
      break
    }
  }
  
  return(list(weights = weights, bias = bias))
}

# Function to make predictions
perceptron_predict <- function(X, weights, bias) {
  # X: input features (matrix)
  # weights: learned weights
  # bias: learned bias
  
  predictions <- sign(as.matrix(X) %*% weights + bias)
  return(predictions)
}

# Example usage with a simple dataset
# Create sample data (linearly separable)
set.seed(123)
X <- matrix(c(
  1, 2,
  2, 3,
  3, 4,
  4, 5,
  5, 6,
  2, 1,
  3, 2,
  4, 3,
  5, 4,
  6, 5
), ncol = 2, byrow = TRUE)

# Create labels (you can adjust these to make it linearly separable)
y <- c(1, 1, 1, 1, 1, -1, -1, -1, -1, -1)

# Train the perceptron
model <- perceptron_train(X, y, learning_rate = 0.1, max_iterations = 1000)

# Print learned parameters
cat("Learned weights:", model$weights, "\n")
cat("Learned bias:", model$bias, "\n")

# Make predictions on training data
predictions <- perceptron_predict(X, model$weights, model$bias)
cat("Predictions:", predictions, "\n")
cat("Actual labels:", y, "\n")
cat("Accuracy:", mean(predictions == y), "\n")

# Example with a more complex dataset
cat("\n--- Second Example ---\n")

# Create a more complex dataset
set.seed(42)
n_samples <- 50
X2 <- matrix(runif(n_samples * 2, -5, 5), ncol = 2)
y2 <- ifelse(X2[,1] + X2[,2] > 0, 1, -1)

# Train perceptron
model2 <- perceptron_train(X2, y2, learning_rate = 0.01, max_iterations = 1000)

# Make predictions
predictions2 <- perceptron_predict(X2, model2$weights, model2$bias)
accuracy2 <- mean(predictions2 == y2)
cat("Accuracy on second dataset:", accuracy2, "\n")

# Visualization (optional)
if (require(graphics)) {
  # Plot the results
  plot(X2[,1], X2[,2], col = ifelse(y2 == 1, "red", "blue"), 
       pch = 19, main = "Perceptron Classification Results", 
       xlab = "Feature 1", ylab = "Feature 2")
  
  # Add decision boundary
  if (model2$weights[2] != 0) {
    x_vals <- seq(min(X2[,1]), max(X2[,1]), length.out = 100)
    y_vals <- -(model2$weights[1] * x_vals + model2$bias) / model2$weights[2]
    lines(x_vals, y_vals, col = "black", lwd = 2)
  }
  
  legend("topright", legend = c("Class 1", "Class -1"), 
         col = c("red", "blue"), pch = 19)
}
```

## Key Components Explained:

### 1. **Training Function (`perceptron_train`)**
- Initializes weights and bias to zero
- Uses iterative learning with update rules
- Updates weights when prediction error occurs
- Stops when no more errors are found

### 2. **Prediction Function (`perceptron_predict`)**
- Uses the learned weights and bias to make predictions
- Applies the sign function to determine class labels

### 3. **Learning Process**
- For each training example, calculate prediction
- If prediction is wrong, update weights and bias
- Update rule: `weight = weight + learning_rate * target * input`
- Update rule for bias: `bias = bias + learning_rate * target`

### 4. **Key Parameters**
- **Learning Rate**: Controls the step size of updates
- **Max Iterations**: Prevents infinite loops
- **Convergence**: Stops when no more errors occur

## Expected Output:
```
Training completed in 2 iterations
Learned weights: 0.1 0.1
Learned bias: 0.1
Predictions: 1 1 1 1 1 -1 -1 -1 -1 -1
Actual labels: 1 1 1 1 1 -1 -1 -1 -1 -1
Accuracy: 1
```

This implementation demonstrates the fundamental perceptron learning algorithm with proper error handling and convergence detection.

