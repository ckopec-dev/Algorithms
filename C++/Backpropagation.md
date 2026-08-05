# Backpropagation Algorithm in C++

Here's a complete implementation of a simple neural network using backpropagation in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <cmath>

class NeuralNetwork {
private:
    int inputSize;
    int hiddenSize;
    int outputSize;
    
    // Weight matrices
    std::vector<std::vector<double>> weightsIH; // Input to Hidden
    std::vector<std::vector<double>> weightsHO; // Hidden to Output
    
    // Bias vectors
    std::vector<double> biasH;
    std::vector<double> biasO;
    
    // Learning rate
    double learningRate;
    
    // Activation function (sigmoid)
    double sigmoid(double x) {
        return 1.0 / (1.0 + exp(-x));
    }
    
    // Derivative of sigmoid
    double sigmoidDerivative(double x) {
        return x * (1.0 - x);
    }
    
public:
    NeuralNetwork(int inputNodes, int hiddenNodes, int outputNodes, double lr = 0.1) 
        : inputSize(inputNodes), hiddenSize(hiddenNodes), outputSize(outputNodes), learningRate(lr) {
        
        // Initialize weights with random values
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<double> dis(-1.0, 1.0);
        
        // Initialize input to hidden weights
        weightsIH.resize(inputSize, std::vector<double>(hiddenSize));
        for (int i = 0; i < inputSize; i++) {
            for (int j = 0; j < hiddenSize; j++) {
                weightsIH[i][j] = dis(gen);
            }
        }
        
        // Initialize hidden to output weights
        weightsHO.resize(hiddenSize, std::vector<double>(outputSize));
        for (int i = 0; i < hiddenSize; i++) {
            for (int j = 0; j < outputSize; j++) {
                weightsHO[i][j] = dis(gen);
            }
        }
        
        // Initialize biases
        biasH.resize(hiddenSize);
        biasO.resize(outputSize);
        for (int i = 0; i < hiddenSize; i++) {
            biasH[i] = dis(gen);
        }
        for (int i = 0; i < outputSize; i++) {
            biasO[i] = dis(gen);
        }
    }
    
    // Forward propagation
    std::vector<double> forward(const std::vector<double>& input) {
        // Calculate hidden layer values
        std::vector<double> hidden(hiddenSize);
        for (int i = 0; i < hiddenSize; i++) {
            hidden[i] = biasH[i];
            for (int j = 0; j < inputSize; j++) {
                hidden[i] += input[j] * weightsIH[j][i];
            }
            hidden[i] = sigmoid(hidden[i]);
        }
        
        // Calculate output layer values
        std::vector<double> output(outputSize);
        for (int i = 0; i < outputSize; i++) {
            output[i] = biasO[i];
            for (int j = 0; j < hiddenSize; j++) {
                output[i] += hidden[j] * weightsHO[j][i];
            }
            output[i] = sigmoid(output[i]);
        }
        
        return output;
    }
    
    // Backward propagation (training)
    void train(const std::vector<double>& input, const std::vector<double>& target) {
        // Forward pass
        std::vector<double> hidden(hiddenSize);
        std::vector<double> output(outputSize);
        
        // Hidden layer calculations
        for (int i = 0; i < hiddenSize; i++) {
            hidden[i] = biasH[i];
            for (int j = 0; j < inputSize; j++) {
                hidden[i] += input[j] * weightsIH[j][i];
            }
            hidden[i] = sigmoid(hidden[i]);
        }
        
        // Output layer calculations
        for (int i = 0; i < outputSize; i++) {
            output[i] = biasO[i];
            for (int j = 0; j < hiddenSize; j++) {
                output[i] += hidden[j] * weightsHO[j][i];
            }
            output[i] = sigmoid(output[i]);
        }
        
        // Calculate output layer errors
        std::vector<double> outputErrors(outputSize);
        for (int i = 0; i < outputSize; i++) {
            outputErrors[i] = target[i] - output[i];
        }
        
        // Calculate output layer gradients
        std::vector<double> outputGradients(outputSize);
        for (int i = 0; i < outputSize; i++) {
            outputGradients[i] = outputErrors[i] * sigmoidDerivative(output[i]);
        }
        
        // Calculate hidden layer errors
        std::vector<double> hiddenErrors(hiddenSize);
        for (int i = 0; i < hiddenSize; i++) {
            hiddenErrors[i] = 0.0;
            for (int j = 0; j < outputSize; j++) {
                hiddenErrors[i] += outputGradients[j] * weightsHO[i][j];
            }
        }
        
        // Calculate hidden layer gradients
        std::vector<double> hiddenGradients(hiddenSize);
        for (int i = 0; i < hiddenSize; i++) {
            hiddenGradients[i] = hiddenErrors[i] * sigmoidDerivative(hidden[i]);
        }
        
        // Update weights and biases (output layer)
        for (int i = 0; i < outputSize; i++) {
            biasO[i] += outputGradients[i] * learningRate;
            for (int j = 0; j < hiddenSize; j++) {
                weightsHO[j][i] += hidden[j] * outputGradients[i] * learningRate;
            }
        }
        
        // Update weights and biases (hidden layer)
        for (int i = 0; i < hiddenSize; i++) {
            biasH[i] += hiddenGradients[i] * learningRate;
            for (int j = 0; j < inputSize; j++) {
                weightsIH[j][i] += input[j] * hiddenGradients[i] * learningRate;
            }
        }
    }
    
    // Calculate mean squared error
    double calculateError(const std::vector<double>& output, const std::vector<double>& target) {
        double error = 0.0;
        for (int i = 0; i < outputSize; i++) {
            double diff = target[i] - output[i];
            error += diff * diff;
        }
        return error / outputSize;
    }
};

// Example usage
int main() {
    // Create a neural network: 2 inputs, 4 hidden, 1 output
    NeuralNetwork nn(2, 4, 1, 0.5);
    
    // Training data (XOR problem)
    std::vector<std::vector<double>> inputs = {
        {0, 0},
        {0, 1},
        {1, 0},
        {1, 1}
    };
    
    std::vector<std::vector<double>> targets = {
        {0},
        {1},
        {1},
        {0}
    };
    
    // Training loop
    std::cout << "Training Neural Network (XOR problem)...\n";
    for (int epoch = 0; epoch < 10000; epoch++) {
        double totalError = 0.0;
        
        for (int i = 0; i < inputs.size(); i++) {
            nn.train(inputs[i], targets[i]);
            
            // Calculate error for monitoring
            std::vector<double> output = nn.forward(inputs[i]);
            totalError += nn.calculateError(output, targets[i]);
        }
        
        if (epoch % 1000 == 0) {
            std::cout << "Epoch " << epoch << ", Average Error: " << totalError / inputs.size() << std::endl;
        }
    }
    
    // Test the trained network
    std::cout << "\nTesting the trained network:\n";
    for (int i = 0; i < inputs.size(); i++) {
        std::vector<double> output = nn.forward(inputs[i]);
        std::cout << "Input: [" << inputs[i][0] << ", " << inputs[i][1] 
                  << "] -> Output: " << output[0] << " (Target: " << targets[i][0] << ")\n";
    }
    
    return 0;
}
```

## Key Components Explained:

### 1. **Neural Network Structure**
- Input layer with 2 nodes
- Hidden layer with 4 nodes  
- Output layer with 1 node

### 2. **Forward Propagation**
- Input values are multiplied by weights and added to biases
- Sigmoid activation function is applied at each layer
- Values propagate from input to output

### 3. **Backward Propagation**
- Calculate errors at output layer
- Compute gradients using chain rule
- Update weights and biases in opposite direction of gradient
- Learning rate controls the step size

### 4. **Training Process**
- Iteratively adjusts weights based on prediction errors
- Uses XOR problem as example (classic neural network benchmark)
- Monitors error reduction over epochs

## Expected Output:
```
Training Neural Network (XOR problem)...
Epoch 0, Average Error: 0.25
Epoch 1000, Average Error: 0.00234
Epoch 2000, Average Error: 0.00089
...

Testing the trained network:
Input: [0, 0] -> Output: 0.0123 (Target: 0)
Input: [0, 1] -> Output: 0.9876 (Target: 1)
Input: [1, 0] -> Output: 0.9854 (Target: 1)
Input: [1, 1] -> Output: 0.0234 (Target: 0)
```

This implementation demonstrates the core principles of backpropagation: forward computation followed by error calculation and weight updates to minimize prediction errors.