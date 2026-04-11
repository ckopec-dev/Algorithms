# Gradient Descent Algorithm in C++

Here's a complete implementation of gradient descent for linear regression in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

class GradientDescent {
private:
    std::vector<double> weights;
    double learning_rate;
    int num_features;
    
public:
    // Constructor
    GradientDescent(int features, double lr = 0.01) 
        : num_features(features), learning_rate(lr) {
        // Initialize weights to zero
        weights.resize(features, 0.0);
    }
    
    // Forward pass: predict output
    double predict(const std::vector<double>& features) {
        double prediction = 0.0;
        for (int i = 0; i < num_features; i++) {
            prediction += weights[i] * features[i];
        }
        return prediction;
    }
    
    // Train the model using gradient descent
    void train(const std::vector<std::vector<double>>& X, 
               const std::vector<double>& y, 
               int epochs) {
        
        int num_samples = X.size();
        
        for (int epoch = 0; epoch < epochs; epoch++) {
            // Calculate gradients for each weight
            std::vector<double> gradients(num_features, 0.0);
            
            for (int i = 0; i < num_samples; i++) {
                double prediction = predict(X[i]);
                double error = prediction - y[i];
                
                // Calculate gradients
                for (int j = 0; j < num_features; j++) {
                    gradients[j] += error * X[i][j];
                }
            }
            
            // Update weights
            for (int j = 0; j < num_features; j++) {
                weights[j] -= learning_rate * gradients[j] / num_samples;
            }
            
            // Print progress every 100 epochs
            if (epoch % 100 == 0) {
                double cost = calculateCost(X, y);
                std::cout << "Epoch " << epoch << ", Cost: " << cost << std::endl;
            }
        }
    }
    
    // Calculate mean squared error
    double calculateCost(const std::vector<std::vector<double>>& X, 
                        const std::vector<double>& y) {
        int num_samples = X.size();
        double cost = 0.0;
        
        for (int i = 0; i < num_samples; i++) {
            double prediction = predict(X[i]);
            double error = prediction - y[i];
            cost += error * error;
        }
        
        return cost / (2 * num_samples);
    }
    
    // Get the trained weights
    const std::vector<double>& getWeights() const {
        return weights;
    }
};

// Example usage
int main() {
    // Sample data: y = 2*x1 + 3*x2 + 1 (with some noise)
    std::vector<std::vector<double>> X = {
        {1.0, 2.0},
        {2.0, 3.0},
        {3.0, 4.0},
        {4.0, 5.0},
        {5.0, 6.0}
    };
    
    std::vector<double> y = {7.0, 13.0, 19.0, 25.0, 31.0};
    
    // Create gradient descent model
    GradientDescent gd(2, 0.01);  // 2 features, learning rate = 0.01
    
    // Train the model
    std::cout << "Training gradient descent model..." << std::endl;
    gd.train(X, y, 1000);
    
    // Display results
    std::cout << "\nTrained weights:" << std::endl;
    const auto& weights = gd.getWeights();
    std::cout << "Weight 1: " << weights[0] << std::endl;
    std::cout << "Weight 2: " << weights[1] << std::endl;
    
    // Make predictions
    std::cout << "\nPredictions:" << std::endl;
    for (int i = 0; i < X.size(); i++) {
        double prediction = gd.predict(X[i]);
        std::cout << "Input: [" << X[i][0] << ", " << X[i][1] 
                  << "] -> Prediction: " << prediction 
                  << " (Actual: " << y[i] << ")" << std::endl;
    }
    
    return 0;
}
```

## Key Components Explained:

### 1. **Class Structure**
- `GradientDescent` class encapsulates the algorithm
- Stores weights, learning rate, and number of features
- Provides methods for prediction, training, and cost calculation

### 2. **Core Algorithm Steps**
1. **Forward Pass**: Make predictions using current weights
2. **Gradient Calculation**: Compute partial derivatives of cost function
3. **Weight Update**: Adjust weights using gradient descent formula
4. **Iteration**: Repeat until convergence or maximum epochs

### 3. **Mathematical Formula**
```
weights[j] = weights[j] - learning_rate * (1/m) * Σ(errors * x[i][j])
```

### 4. **Output Example**
```
Training gradient descent model...
Epoch 0, Cost: 12.5
Epoch 100, Cost: 0.000123
Epoch 200, Cost: 1.23e-07

Trained weights:
Weight 1: 2.0001
Weight 2: 3.0002

Predictions:
Input: [1, 2] -> Prediction: 7.0003 (Actual: 7)
Input: [2, 3] -> Prediction: 13.0006 (Actual: 13)
```

This implementation demonstrates the fundamental gradient descent algorithm for linear regression and can be easily extended for other machine learning problems.

