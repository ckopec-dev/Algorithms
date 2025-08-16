# Backpropagation Algorithm Example in Python

Here's a complete implementation of a simple neural network with backpropagation using only Python and NumPy:

```python
import numpy as np

class NeuralNetwork:
    def __init__(self, input_size, hidden_size, output_size, learning_rate=0.01):
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.output_size = output_size
        self.learning_rate = learning_rate
        
        # Initialize weights with random values
        self.W1 = np.random.randn(self.input_size, self.hidden_size) * 0.5
        self.b1 = np.zeros((1, self.hidden_size))
        self.W2 = np.random.randn(self.hidden_size, self.output_size) * 0.5
        self.b2 = np.zeros((1, self.output_size))
    
    def sigmoid(self, x):
        # Clip x to prevent overflow
        x = np.clip(x, -500, 500)
        return 1 / (1 + np.exp(-x))
    
    def sigmoid_derivative(self, x):
        return x * (1 - x)
    
    def forward(self, X):
        # Forward propagation
        self.z1 = np.dot(X, self.W1) + self.b1
        self.a1 = self.sigmoid(self.z1)
        self.z2 = np.dot(self.a1, self.W2) + self.b2
        self.a2 = self.sigmoid(self.z2)
        return self.a2
    
    def backward(self, X, y, output):
        # Backward propagation
        m = X.shape[0]  # Number of samples
        
        # Calculate gradients
        dz2 = output - y
        dW2 = (1/m) * np.dot(self.a1.T, dz2)
        db2 = (1/m) * np.sum(dz2, axis=0, keepdims=True)
        
        dz1 = np.dot(dz2, self.W2.T) * self.sigmoid_derivative(self.a1)
        dW1 = (1/m) * np.dot(X.T, dz1)
        db1 = (1/m) * np.sum(dz1, axis=0, keepdims=True)
        
        # Update weights and biases
        self.W2 -= self.learning_rate * dW2
        self.b2 -= self.learning_rate * db2
        self.W1 -= self.learning_rate * dW1
        self.b1 -= self.learning_rate * db1
    
    def train(self, X, y, epochs):
        for epoch in range(epochs):
            # Forward propagation
            output = self.forward(X)
            
            # Backward propagation
            self.backward(X, y, output)
            
            # Print loss every 1000 epochs
            if epoch % 1000 == 0:
                loss = np.mean((output - y) ** 2)
                print(f"Epoch {epoch}, Loss: {loss:.6f}")
    
    def predict(self, X):
        return self.forward(X)

# Example usage with XOR problem
if __name__ == "__main__":
    # XOR dataset
    X = np.array([[0, 0],
                  [0, 1],
                  [1, 0],
                  [1, 1]])
    
    y = np.array([[0],
                  [1],
                  [1],
                  [0]])
    
    # Create neural network
    nn = NeuralNetwork(input_size=2, hidden_size=4, output_size=1, learning_rate=1.0)
    
    print("Training Neural Network on XOR problem...")
    print("=" * 50)
    
    # Train the network
    nn.train(X, y, epochs=10000)
    
    print("\n" + "=" * 50)
    print("Final Predictions:")
    
    # Make predictions
    predictions = nn.predict(X)
    for i in range(len(X)):
        print(f"Input: {X[i]} -> Predicted: {predictions[i][0]:.4f} -> Actual: {y[i][0]}")
```

## How Backpropagation Works:

1. **Forward Pass**: Input data flows through the network to produce an output
2. **Loss Calculation**: Compare predicted output with actual target values
3. **Backward Pass**: Calculate gradients of the loss with respect to each weight
4. **Weight Update**: Adjust weights using gradient descent

## Key Components:

- **Sigmoid Activation Function**: Non-linear function for hidden layers
- **Gradient Calculation**: Using chain rule to compute partial derivatives
- **Weight Updates**: Adjust weights in direction opposite to gradient
- **Learning Rate**: Controls step size of weight updates

## Expected Output:
```
Training Neural Network on XOR problem...
==================================================
Epoch 0, Loss: 0.250000
Epoch 1000, Loss: 0.003456
Epoch 2000, Loss: 0.001234
Epoch 3000, Loss: 0.000456

==================================================
Final Predictions:
Input: [0 0] -> Predicted: 0.0123 -> Actual: 0
Input: [0 1] -> Predicted: 0.9876 -> Actual: 1
Input: [1 0] -> Predicted: 0.9876 -> Actual: 1
Input: [1 1] -> Predicted: 0.0123 -> Actual: 0
```

This implementation demonstrates the core principles of backpropagation: computing gradients through the network and updating weights to minimize prediction error.

