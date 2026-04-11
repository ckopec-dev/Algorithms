# Backpropagation Algorithm in R

Here's a complete example of implementing a backpropagation neural network in R:

```r
# Load required libraries
library(neuralnet)

# Create sample data
set.seed(123)
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
# Create target variable (logical relationship)
y <- as.numeric(x1 + x2 > 0)

# Combine into data frame
data <- data.frame(x1 = x1, x2 = x2, y = y)

# Split into training and testing sets
train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Method 1: Using neuralnet package
# Create neural network with 2 inputs, 3 hidden neurons, 1 output
nn <- neuralnet(y ~ x1 + x2, 
                data = train_data,
                hidden = c(3, 2),  # 3 neurons in first hidden layer, 2 in second
                threshold = 0.01,
                stepmax = 1e6,
                linear.output = FALSE,
                threshold = 0.01)

# Plot the network
plot(nn)

# Make predictions on test data
predictions <- predict(nn, test_data[, c("x1", "x2")])

# Convert to binary predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(binary_predictions == test_data$y)
cat("Accuracy:", round(accuracy, 3), "\n")

# Method 2: Manual implementation of backpropagation
# Create a simple neural network class
NeuralNetwork <- function(input_size, hidden_size, output_size, learning_rate = 0.1) {
  # Initialize weights with small random values
  W1 <- matrix(rnorm(input_size * hidden_size, 0, 0.1), 
               nrow = input_size, ncol = hidden_size)
  b1 <- matrix(rnorm(hidden_size, 0, 0.1), nrow = 1, ncol = hidden_size)
  W2 <- matrix(rnorm(hidden_size * output_size, 0, 0.1), 
               nrow = hidden_size, ncol = output_size)
  b2 <- matrix(rnorm(output_size, 0, 0.1), nrow = 1, ncol = output_size)
  
  list(W1 = W1, b1 = b1, W2 = W2, b2 = b2, learning_rate = learning_rate)
}

# Sigmoid activation function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Derivative of sigmoid
sigmoid_derivative <- function(x) {
  x * (1 - x)
}

# Forward propagation
forward_prop <- function(nn, X) {
  z1 <- X %*% nn$W1 + nn$b1
  a1 <- sigmoid(z1)
  z2 <- a1 %*% nn$W2 + nn$b2
  a2 <- sigmoid(z2)
  list(z1 = z1, a1 = a1, z2 = z2, a2 = a2)
}

# Backward propagation
backward_prop <- function(nn, X, y, forward_result) {
  m <- nrow(X)
  
  # Calculate gradients
  dz2 <- forward_result$a2 - y
  dW2 <- (1/m) * t(forward_result$a1) %*% dz2
  db2 <- (1/m) * colSums(dz2)
  
  dz1 <- (dz2 %*% t(nn$W2)) * sigmoid_derivative(forward_result$a1)
  dW1 <- (1/m) * t(X) %*% dz1
  db1 <- (1/m) * colSums(dz1)
  
  # Update weights
  nn$W2 <- nn$W2 - nn$learning_rate * dW2
  nn$b2 <- nn$b2 - nn$learning_rate * db2
  nn$W1 <- nn$W1 - nn$learning_rate * dW1
  nn$b1 <- nn$b1 - nn$learning_rate * db1
  
  nn
}

# Training function
train_nn <- function(nn, X, y, epochs = 1000) {
  for (i in 1:epochs) {
    # Forward propagation
    forward_result <- forward_prop(nn, X)
    
    # Backward propagation
    nn <- backward_prop(nn, X, y, forward_result)
    
    # Print progress
    if (i %% 100 == 0) {
      cost <- mean((forward_result$a2 - y)^2)
      cat("Epoch:", i, "Cost:", round(cost, 4), "\n")
    }
  }
  nn
}

# Manual implementation example
# Prepare data
X <- as.matrix(train_data[, c("x1", "x2")])
y <- as.matrix(train_data$y)

# Create neural network
manual_nn <- NeuralNetwork(input_size = 2, hidden_size = 3, output_size = 1, learning_rate = 0.1)

# Train the network
manual_nn <- train_nn(manual_nn, X, y, epochs = 1000)

# Make predictions
forward_result <- forward_prop(manual_nn, as.matrix(test_data[, c("x1", "x2")]))
manual_predictions <- ifelse(forward_result$a2 > 0.5, 1, 0)

# Calculate accuracy
manual_accuracy <- mean(manual_predictions == test_data$y)
cat("Manual Implementation Accuracy:", round(manual_accuracy, 3), "\n")
```

## Key Components Explained:

### 1. **Forward Propagation**
- Input layer → Hidden layer → Output layer
- Uses sigmoid activation function
- Matrix multiplication with weights and biases

### 2. **Backward Propagation**
- Calculates gradients using chain rule
- Updates weights and biases using gradient descent
- Computes error derivatives

### 3. **Training Process**
- Iterates through epochs
- Updates weights based on prediction errors
- Reduces error over time

### 4. **Key Parameters**
- **Learning Rate**: Controls step size in weight updates
- **Hidden Layers**: Number of neurons in hidden layers
- **Epochs**: Number of training iterations

This example demonstrates both a ready-made implementation using the `neuralnet` package and a custom manual implementation of the backpropagation algorithm in R.

