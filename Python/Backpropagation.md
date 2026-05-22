# Backpropagation Algorithm Example in Python

Here's a complete implementation of a simple neural network using backpropagation:

```python
import numpy as np
import matplotlib.pyplot as plt

class NeuralNetwork:
    def __init__(self, input_size, hidden_size, output_size, learning_rate=0.1):
        """
        Initialize the neural network with given dimensions
        """
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
        """Sigmoid activation function"""
        return 1 / (1 + np.exp(-np.clip(x, -250, 250)))  # Clip to prevent overflow
    
    def sigmoid_derivative(self, x):
        """Derivative of sigmoid function"""
        return x * (1 - x)
    
    def forward(self, X):
        """
        Forward propagation
        """
        self.z1 = np.dot(X, self.W1) + self.b1
        self.a1 = self.sigmoid(self.z1)
        self.z2 = np.dot(self.a1, self.W2) + self.b2
        self.a2 = self.sigmoid(self.z2)
        return self.a2
    
    def backward(self, X, y, output):
        """
        Backward propagation
        """
        m = X.shape[0]  # number of samples
        
        # Calculate gradients for output layer
        dz2 = output - y
        dW2 = (1/m) * np.dot(self.a1.T, dz2)
        db2 = (1/m) * np.sum(dz2, axis=0, keepdims=True)
        
        # Calculate gradients for hidden layer
        dz1 = np.dot(dz2, self.W2.T) * self.sigmoid_derivative(self.a1)
        dW1 = (1/m) * np.dot(X.T, dz1)
        db1 = (1/m) * np.sum(dz1, axis=0, keepdims=True)
        
        # Update weights and biases
        self.W2 -= self.learning_rate * dW2
        self.b2 -= self.learning_rate * db2
        self.W1 -= self.learning_rate * dW1
        self.b1 -= self.learning_rate * db1
    
    def train(self, X, y, epochs):
        """
        Train the neural network
        """
        losses = []
        
        for epoch in range(epochs):
            # Forward propagation
            output = self.forward(X)
            
            # Calculate loss (Mean Squared Error)
            loss = np.mean((output - y) ** 2)
            losses.append(loss)
            
            # Backward propagation
            self.backward(X, y, output)
            
            # Print progress
            if epoch % 1000 == 0:
                print(f"Epoch {epoch}, Loss: {loss:.6f}")
        
        return losses
    
    def predict(self, X):
        """
        Make predictions
        """
        return self.forward(X)

# Example usage: XOR problem
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
    
    # Create and train neural network
    nn = NeuralNetwork(input_size=2, hidden_size=4, output_size=1, learning_rate=1.0)
    
    print("Training Neural Network on XOR problem...")
    print("Input data:")
    print(X)
    print("Target output:")
    print(y)
    
    # Train the network
    losses = nn.train(X, y, epochs=10000)
    
    # Make predictions
    print("\nPredictions after training:")
    predictions = nn.predict(X)
    for i in range(len(X)):
        print(f"Input: {X[i]} -> Predicted: {predictions[i][0]:.4f} -> Target: {y[i][0]}")
    
    # Plot training loss
    plt.figure(figsize=(10, 6))
    plt.plot(losses)
    plt.title('Training Loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.grid(True)
    plt.show()
    
    # Test with new data
    print("\nTesting with new data:")
    test_data = np.array([[0, 0], [0, 1], [1, 0], [1, 1]])
    test_predictions = nn.predict(test_data)
    for i in range(len(test_data)):
        print(f"Input: {test_data[i]} -> Output: {test_predictions[i][0]:.4f}")
```

## Key Components Explained:

### 1. **Forward Propagation**
- Input layer → Hidden layer → Output layer
- Uses sigmoid activation function
- Each layer computes: `z = W·X + b` then `a = sigmoid(z)`

### 2. **Backward Propagation**
- Calculates gradients using chain rule
- Computes error at output layer
- Propagates error backward to hidden layers
- Updates weights and biases using gradient descent

### 3. **Training Process**
- Forward pass to get predictions
- Calculate loss (MSE)
- Backward pass to compute gradients
- Update weights using learning rate

### 4. **Key Equations**
```
Output = sigmoid(Weight1 · Input + Bias1)
Hidden = sigmoid(Weight2 · Output + Bias2)
Gradient = (Output - Target) · sigmoid_derivative(Output)
Weights = Weights - LearningRate · Gradient
```

This example demonstrates how backpropagation works by training a neural network to solve the XOR problem, which is not linearly separable and requires a hidden layer to learn the non-linear relationship.

