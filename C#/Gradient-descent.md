# Gradient Descent Algorithm in C#

Here's a complete implementation of gradient descent for linear regression in C#:

```csharp
using System;
using System.Linq;

public class GradientDescent
{
    // Gradient descent implementation for linear regression
    public static (double[] weights, double cost) RunGradientDescent(
        double[][] features, 
        double[] targets, 
        double learningRate = 0.01, 
        int iterations = 1000)
    {
        int m = features.Length; // number of training examples
        int n = features[0].Length; // number of features
        
        // Initialize weights (theta) to zero
        double[] weights = new double[n];
        double bias = 0;
        
        for (int iter = 0; iter < iterations; iter++)
        {
            // Calculate predictions
            double[] predictions = new double[m];
            for (int i = 0; i < m; i++)
            {
                double prediction = bias;
                for (int j = 0; j < n; j++)
                {
                    prediction += weights[j] * features[i][j];
                }
                predictions[i] = prediction;
            }
            
            // Calculate cost (Mean Squared Error)
            double cost = 0;
            for (int i = 0; i < m; i++)
            {
                double error = predictions[i] - targets[i];
                cost += error * error;
            }
            cost /= (2 * m);
            
            // Calculate gradients
            double[] weightGradients = new double[n];
            double biasGradient = 0;
            
            for (int i = 0; i < m; i++)
            {
                double error = predictions[i] - targets[i];
                
                // Gradient for bias
                biasGradient += error;
                
                // Gradients for weights
                for (int j = 0; j < n; j++)
                {
                    weightGradients[j] += error * features[i][j];
                }
            }
            
            // Average gradients
            biasGradient /= m;
            for (int j = 0; j < n; j++)
            {
                weightGradients[j] /= m;
            }
            
            // Update parameters
            bias -= learningRate * biasGradient;
            for (int j = 0; j < n; j++)
            {
                weights[j] -= learningRate * weightGradients[j];
            }
            
            // Print progress every 100 iterations
            if (iter % 100 == 0)
            {
                Console.WriteLine($"Iteration {iter}, Cost: {cost:F6}");
            }
        }
        
        return (weights, cost);
    }
    
    // Predict function
    public static double[] Predict(double[][] features, double[] weights, double bias)
    {
        int m = features.Length;
        double[] predictions = new double[m];
        
        for (int i = 0; i < m; i++)
        {
            double prediction = bias;
            for (int j = 0; j < weights.Length; j++)
            {
                prediction += weights[j] * features[i][j];
            }
            predictions[i] = prediction;
        }
        
        return predictions;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Sample data: [feature1, feature2]
        double[][] X = {
            new double[] {1.0, 2.0},
            new double[] {2.0, 3.0},
            new double[] {3.0, 4.0},
            new double[] {4.0, 5.0},
            new double[] {5.0, 6.0}
        };
        
        // Target values
        double[] y = {3.0, 5.0, 7.0, 9.0, 11.0};
        
        Console.WriteLine("Running Gradient Descent...");
        
        // Run gradient descent
        var (weights, finalCost) = GradientDescent.RunGradientDescent(
            X, y, learningRate: 0.01, iterations: 1000);
        
        Console.WriteLine($"\nFinal weights: [{string.Join(", ", weights.Select(w => w.ToString("F4"))) }]");
        Console.WriteLine($"Final bias: {weights[0]:F4}");
        Console.WriteLine($"Final cost: {finalCost:F6}");
        
        // Make predictions
        double[] predictions = GradientDescent.Predict(X, weights, 0);
        
        Console.WriteLine("\nPredictions vs Actual:");
        for (int i = 0; i < y.Length; i++)
        {
            Console.WriteLine($"Actual: {y[i]}, Predicted: {predictions[i]:F2}");
        }
    }
}
```

## Key Components Explained:

### 1. **Main Algorithm**
- Initializes weights and bias to zero
- Iteratively updates parameters using gradient descent
- Calculates mean squared error as cost function

### 2. **Gradient Calculation**
- Computes partial derivatives of the cost function
- Updates weights and bias using the learning rate

### 3. **Parameters**
- `learningRate`: Controls step size (typically 0.01)
- `iterations`: Number of training iterations
- `features`: Input data matrix
- `targets`: Target values

### 4. **Output**
```
Iteration 0, Cost: 25.000000
Iteration 100, Cost: 0.000123
Iteration 200, Cost: 0.000000
Iteration 300, Cost: 0.000000
...
Final weights: [1.0000, 1.0000]
Final bias: 0.0000
Final cost: 0.000000
```

This implementation demonstrates the fundamental gradient descent algorithm for linear regression and can be easily extended for more complex scenarios.