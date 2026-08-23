# Perceptron Learning Algorithm in C++

Here's a complete implementation of the Perceptron learning algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <cmath>

class Perceptron {
private:
    std::vector<double> weights;
    double bias;
    double learning_rate;

public:
    // Constructor
    Perceptron(int input_size, double lr = 0.1) : learning_rate(lr) {
        // Initialize weights and bias with small random values
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(-0.5, 0.5);
        
        weights.resize(input_size);
        for (int i = 0; i < input_size; i++) {
            weights[i] = dis(gen);
        }
        bias = dis(gen);
    }
    
    // Activation function (step function)
    int activation(double sum) {
        return (sum >= 0) ? 1 : -1;
    }
    
    // Predict function
    int predict(const std::vector<double>& inputs) {
        double sum = bias;
        for (int i = 0; i < inputs.size(); i++) {
            sum += weights[i] * inputs[i];
        }
        return activation(sum);
    }
    
    // Training function using Perceptron Learning Rule
    void train(const std::vector<std::vector<double>>& inputs, 
               const std::vector<int>& targets, 
               int epochs = 100) {
        
        for (int epoch = 0; epoch < epochs; epoch++) {
            bool all_correct = true;
            
            for (int i = 0; i < inputs.size(); i++) {
                // Make prediction
                int prediction = predict(inputs[i]);
                int target = targets[i];
                
                // Check if prediction is correct
                if (prediction != target) {
                    all_correct = false;
                    
                    // Update weights and bias using Perceptron Learning Rule
                    for (int j = 0; j < weights.size(); j++) {
                        weights[j] += learning_rate * (target - prediction) * inputs[i][j];
                    }
                    bias += learning_rate * (target - prediction);
                }
            }
            
            // If all examples are classified correctly, stop training
            if (all_correct) {
                std::cout << "Training completed in " << epoch + 1 << " epochs\n";
                break;
            }
        }
    }
    
    // Print weights and bias
    void print_weights() {
        std::cout << "Weights: ";
        for (int i = 0; i < weights.size(); i++) {
            std::cout << weights[i] << " ";
        }
        std::cout << "\nBias: " << bias << std::endl;
    }
};

// Example usage
int main() {
    // XOR problem example
    std::vector<std::vector<double>> inputs = {
        {0, 0},
        {0, 1},
        {1, 0},
        {1, 1}
    };
    
    // XOR target outputs (we'll use -1 for 0 and 1 for 1)
    std::vector<int> targets = {-1, 1, 1, -1};
    
    // Create perceptron with 2 inputs
    Perceptron perceptron(2, 0.1);
    
    std::cout << "Initial weights and bias:\n";
    perceptron.print_weights();
    
    std::cout << "\nTraining the perceptron...\n";
    perceptron.train(inputs, targets, 1000);
    
    std::cout << "\nFinal weights and bias:\n";
    perceptron.print_weights();
    
    std::cout << "\nTesting the trained perceptron:\n";
    for (int i = 0; i < inputs.size(); i++) {
        int prediction = perceptron.predict(inputs[i]);
        std::cout << "Input: [" << inputs[i][0] << ", " << inputs[i][1] 
                  << "] -> Prediction: " << prediction 
                  << " (Target: " << targets[i] << ")\n";
    }
    
    return 0;
}
```

## Key Components Explained:

### 1. **Perceptron Class**
- **Weights**: Array of weights for each input feature
- **Bias**: Single bias term
- **Learning Rate**: Controls the step size during learning

### 2. **Core Functions**

**Activation Function**: Uses a step function that outputs +1 if the weighted sum is ≥ 0, otherwise -1

**Predict Function**: Computes the weighted sum and applies activation function

**Train Function**: Implements the Perceptron Learning Rule:
```
w_i = w_i + η(y - ŷ)x_i
b = b + η(y - ŷ)
```

### 3. **Perceptron Learning Rule**
The algorithm updates weights when a misclassification occurs:
- If prediction is wrong, adjust weights in the direction that would make the prediction correct
- The update is proportional to the learning rate and the error

### 4. **Important Notes**
- This implementation can only solve linearly separable problems
- XOR problem requires a multi-layer perceptron (MLP) as it's not linearly separable
- For XOR, you'd need to modify the approach or use a neural network with hidden layers

This example demonstrates the fundamental concept of how a single-layer perceptron learns to classify data by adjusting its weights and bias through iterative updates.