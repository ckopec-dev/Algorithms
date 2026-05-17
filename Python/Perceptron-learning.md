# Perceptron Learning Algorithm Implementation

Here's a complete implementation of the Perceptron learning algorithm in Python:

```python
import numpy as np
import matplotlib.pyplot as plt

class Perceptron:
    def __init__(self, learning_rate=0.01, max_iterations=1000):
        """
        Initialize the Perceptron classifier
        
        Parameters:
        learning_rate (float): Learning rate for weight updates
        max_iterations (int): Maximum number of iterations
        """
        self.learning_rate = learning_rate
        self.max_iterations = max_iterations
        self.weights = None
        self.bias = None
        self.errors = []
    
    def fit(self, X, y):
        """
        Train the perceptron on the given data
        
        Parameters:
        X (array-like): Training features of shape (n_samples, n_features)
        y (array-like): Training labels of shape (n_samples,)
        """
        # Initialize weights and bias
        n_samples, n_features = X.shape
        self.weights = np.zeros(n_features)
        self.bias = 0
        
        # Convert labels to -1 and 1
        y_ = np.where(y <= 0, -1, 1)
        
        # Training loop
        for i in range(self.max_iterations):
            errors = 0
            
            for idx, x_i in enumerate(X):
                # Calculate prediction
                linear_output = np.dot(x_i, self.weights) + self.bias
                prediction = self._unit_step_function(linear_output)
                
                # Update weights if prediction is wrong
                if prediction != y_[idx]:
                    errors += 1
                    # Update rule: w = w + learning_rate * (y_true - y_pred) * x
                    update = self.learning_rate * (y_[idx] - prediction)
                    self.weights += update * x_i
                    self.bias += update
            
            self.errors.append(errors)
            
            # Stop if no errors (converged)
            if errors == 0:
                print(f"Converged after {i+1} iterations")
                break
    
    def _unit_step_function(self, x):
        """
        Unit step activation function
        """
        return np.where(x >= 0, 1, 0)
    
    def predict(self, X):
        """
        Make predictions on new data
        
        Parameters:
        X (array-like): Test features of shape (n_samples, n_features)
        
        Returns:
        array: Predicted labels
        """
        linear_output = np.dot(X, self.weights) + self.bias
        return np.where(linear_output >= 0, 1, 0)
    
    def predict_proba(self, X):
        """
        Get prediction probabilities (not used in perceptron but for consistency)
        """
        linear_output = np.dot(X, self.weights) + self.bias
        return np.where(linear_output >= 0, 1, 0)

# Example usage
if __name__ == "__main__":
    # Create sample data (linearly separable)
    # XOR problem - this won't work with basic perceptron
    # Let's use a simple linearly separable dataset
    
    # Generate sample data
    np.random.seed(42)
    X = np.random.randn(100, 2)
    
    # Create labels based on a linear relationship
    y = (X[:, 0] + X[:, 1] > 0).astype(int)
    
    # Split data
    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    
    # Create and train perceptron
    perceptron = Perceptron(learning_rate=0.01, max_iterations=1000)
    perceptron.fit(X_train, y_train)
    
    # Make predictions
    predictions = perceptron.predict(X_test)
    
    # Calculate accuracy
    accuracy = np.mean(predictions == y_test)
    print(f"Accuracy: {accuracy:.2f}")
    
    # Plot results
    plt.figure(figsize=(12, 5))
    
    # Plot 1: Training data and decision boundary
    plt.subplot(1, 2, 1)
    
    # Plot training data
    plt.scatter(X_train[y_train == 0, 0], X_train[y_train == 0, 1], 
               c='red', marker='o', label='Class 0')
    plt.scatter(X_train[y_train == 1, 0], X_train[y_train == 1, 1], 
               c='blue', marker='s', label='Class 1')
    
    # Plot decision boundary
    if perceptron.weights[1] != 0:
        x_boundary = np.linspace(X_train[:, 0].min(), X_train[:, 0].max(), 100)
        y_boundary = -(perceptron.weights[0] * x_boundary + perceptron.bias) / perceptron.weights[1]
        plt.plot(x_boundary, y_boundary, 'k-', linewidth=2, label='Decision Boundary')
    
    plt.xlabel('Feature 1')
    plt.ylabel('Feature 2')
    plt.title('Perceptron Decision Boundary')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Plot 2: Training errors
    plt.subplot(1, 2, 2)
    plt.plot(perceptron.errors)
    plt.xlabel('Iterations')
    plt.ylabel('Number of Errors')
    plt.title('Training Errors Over Time')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Print final weights and bias
    print(f"Final weights: {perceptron.weights}")
    print(f"Final bias: {perceptron.bias}")
```

## Key Features of This Implementation:

1. **Initialization**: Sets up weights and bias randomly
2. **Training Loop**: Iteratively updates weights based on misclassified samples
3. **Update Rule**: Uses the perceptron learning rule: `w = w + learning_rate * (y_true - y_pred) * x`
4. **Convergence**: Stops when no more errors are made
5. **Prediction**: Makes binary predictions using the trained weights
6. **Visualization**: Shows decision boundary and training progress

## How the Algorithm Works:

1. Initialize weights and bias to small random values
2. For each training example:
   - Compute prediction using current weights
   - If prediction is wrong, update weights using the learning rule
3. Repeat until convergence or maximum iterations reached

## Limitations:

- Only works for linearly separable data
- Cannot solve XOR problem (requires multi-layer networks)
- Sensitive to learning rate choice

This implementation demonstrates the fundamental perceptron learning algorithm in action!

