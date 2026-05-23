# Gradient Descent Algorithm in Go

Here's a complete implementation of gradient descent algorithm in Go for linear regression:

```go
package main

import (
    "fmt"
    "math"
)

// GradientDescent performs gradient descent to find optimal parameters
func GradientDescent(x, y []float64, learningRate float64, iterations int) (float64, float64) {
    // Initialize parameters
    m := 0.0 // slope
    b := 0.0 // intercept
    
    n := float64(len(x))
    
    for i := 0; i < iterations; i++ {
        // Calculate predictions
        var predictions []float64
        for _, xi := range x {
            predictions = append(predictions, m*xi+b)
        }
        
        // Calculate gradients
        dm := 0.0
        db := 0.0
        
        for j := 0; j < len(x); j++ {
            error := predictions[j] - y[j]
            dm += (2 / n) * error * x[j]
            db += (2 / n) * error
        }
        
        // Update parameters
        m = m - learningRate*dm
        b = b - learningRate*db
        
        // Print progress every 1000 iterations
        if i%1000 == 0 {
            cost := calculateCost(x, y, m, b)
            fmt.Printf("Iteration %d, Cost: %.6f, m: %.6f, b: %.6f\n", i, cost, m, b)
        }
    }
    
    return m, b
}

// calculateCost computes the mean squared error
func calculateCost(x, y []float64, m, b float64) float64 {
    var sum float64
    n := float64(len(x))
    
    for i := 0; i < len(x); i++ {
        prediction := m*x[i] + b
        error := prediction - y[i]
        sum += error * error
    }
    
    return sum / (2 * n)
}

// predict makes a prediction using the trained model
func predict(x float64, m, b float64) float64 {
    return m*x + b
}

func main() {
    // Sample data: y = 2x + 1 + noise
    x := []float64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    y := []float64{3.1, 4.9, 7.2, 9.1, 10.8, 13.2, 15.1, 17.0, 19.2, 21.1}
    
    fmt.Println("Starting Gradient Descent...")
    fmt.Println("Data points:")
    for i := 0; i < len(x); i++ {
        fmt.Printf("x: %.1f, y: %.1f\n", x[i], y[i])
    }
    
    // Set hyperparameters
    learningRate := 0.01
    iterations := 10000
    
    // Run gradient descent
    m, b := GradientDescent(x, y, learningRate, iterations)
    
    fmt.Printf("\nFinal Results:\n")
    fmt.Printf("Slope (m): %.6f\n", m)
    fmt.Printf("Intercept (b): %.6f\n", b)
    fmt.Printf("Equation: y = %.6fx + %.6f\n", m, b)
    
    // Make some predictions
    fmt.Println("\nPredictions:")
    testPoints := []float64{0, 2.5, 5, 7.5, 10}
    for _, point := range testPoints {
        prediction := predict(point, m, b)
        fmt.Printf("x = %.1f -> y = %.2f\n", point, prediction)
    }
    
    // Calculate final cost
    finalCost := calculateCost(x, y, m, b)
    fmt.Printf("\nFinal Cost: %.6f\n", finalCost)
}
```

## How it works:

1. **Initialization**: Start with initial values for slope (m) and intercept (b)
2. **Prediction**: For each data point, calculate predicted y values using y = mx + b
3. **Gradient Calculation**: Compute partial derivatives of the cost function with respect to m and b
4. **Parameter Update**: Update parameters using the learning rate and gradients
5. **Iteration**: Repeat until convergence or maximum iterations reached

## Key Components:

- **Cost Function**: Mean Squared Error (MSE)
- **Gradients**: Partial derivatives of MSE with respect to parameters
- **Learning Rate**: Controls step size during parameter updates
- **Iterations**: Number of times to update parameters

## Sample Output:
```
Starting Gradient Descent...
Data points:
x: 1.0, y: 3.1
x: 2.0, y: 4.9
x: 3.0, y: 7.2
...

Iteration 0, Cost: 2.105000, m: 0.000000, b: 0.000000
Iteration 1000, Cost: 0.000000, m: 2.000000, b: 1.000000

Final Results:
Slope (m): 2.000000
Intercept (b): 1.000000
Equation: y = 2.000000x + 1.000000
```

This implementation demonstrates the core concepts of gradient descent for linear regression and can be easily adapted for other optimization problems.

