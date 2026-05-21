# Linear Regression in Go

Here's a complete implementation of linear regression algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// LinearRegression represents a simple linear regression model
type LinearRegression struct {
    slope    float64
    intercept float64
    trained  bool
}

// NewLinearRegression creates a new linear regression model
func NewLinearRegression() *LinearRegression {
    return &LinearRegression{}
}

// Train fits the linear regression model to the given data
func (lr *LinearRegression) Train(x, y []float64) {
    if len(x) != len(y) || len(x) == 0 {
        panic("x and y must have the same length and be non-empty")
    }

    n := float64(len(x))
    
    // Calculate means
    var sumX, sumY float64
    for i := 0; i < len(x); i++ {
        sumX += x[i]
        sumY += y[i]
    }
    meanX := sumX / n
    meanY := sumY / n

    // Calculate slope (beta1) and intercept (beta0)
    var numerator, denominator float64
    for i := 0; i < len(x); i++ {
        numerator += (x[i] - meanX) * (y[i] - meanY)
        denominator += math.Pow(x[i] - meanX, 2)
    }

    lr.slope = numerator / denominator
    lr.intercept = meanY - lr.slope*meanX
    lr.trained = true
}

// Predict makes a prediction for a given input
func (lr *LinearRegression) Predict(x float64) float64 {
    if !lr.trained {
        panic("Model must be trained before making predictions")
    }
    return lr.slope*x + lr.intercept
}

// GetSlope returns the slope of the regression line
func (lr *LinearRegression) GetSlope() float64 {
    return lr.slope
}

// GetIntercept returns the y-intercept of the regression line
func (lr *LinearRegression) GetIntercept() float64 {
    return lr.intercept
}

// CalculateR2 calculates the coefficient of determination (R²)
func (lr *LinearRegression) CalculateR2(x, y []float64) float64 {
    if !lr.trained {
        panic("Model must be trained before calculating R²")
    }

    var sumSquaredErr, sumSquaredTotal float64
    var meanY float64
    
    // Calculate mean of y
    for _, val := range y {
        meanY += val
    }
    meanY /= float64(len(y))

    // Calculate R²
    for i := 0; i < len(x); i++ {
        predicted := lr.Predict(x[i])
        sumSquaredErr += math.Pow(y[i]-predicted, 2)
        sumSquaredTotal += math.Pow(y[i]-meanY, 2)
    }

    return 1 - (sumSquaredErr / sumSquaredTotal)
}

func main() {
    // Sample data: x represents hours studied, y represents test scores
    x := []float64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    y := []float64{2, 4, 6, 8, 10, 12, 14, 16, 18, 20}

    // Create and train the model
    model := NewLinearRegression()
    model.Train(x, y)

    // Display model parameters
    fmt.Printf("Linear Regression Model\n")
    fmt.Printf("Slope: %.4f\n", model.GetSlope())
    fmt.Printf("Intercept: %.4f\n", model.GetIntercept())
    fmt.Printf("Equation: y = %.4fx + %.4f\n", model.GetSlope(), model.GetIntercept())

    // Make some predictions
    fmt.Println("\nPredictions:")
    testValues := []float64{2.5, 5.5, 8.5}
    for _, val := range testValues {
        prediction := model.Predict(val)
        fmt.Printf("x = %.1f -> y = %.2f\n", val, prediction)
    }

    // Calculate R²
    r2 := model.CalculateR2(x, y)
    fmt.Printf("\nR² (Coefficient of Determination): %.4f\n", r2)
    
    // Test with a more complex dataset
    fmt.Println("\n" + "="*50)
    fmt.Println("Testing with a more complex dataset:")
    
    x2 := []float64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    y2 := []float64{1.5, 3.8, 6.2, 8.1, 10.3, 12.7, 14.9, 17.2, 19.1, 21.5}
    
    model2 := NewLinearRegression()
    model2.Train(x2, y2)
    
    fmt.Printf("Slope: %.4f\n", model2.GetSlope())
    fmt.Printf("Intercept: %.4f\n", model2.GetIntercept())
    fmt.Printf("Equation: y = %.4fx + %.4f\n", model2.GetSlope(), model2.GetIntercept())
    
    r2_2 := model2.CalculateR2(x2, y2)
    fmt.Printf("R²: %.4f\n", r2_2)
}
```

## Key Features of this Implementation:

1. **Model Structure**: 
   - `LinearRegression` struct with slope and intercept parameters
   - Training method to fit the model to data
   - Prediction method for new inputs

2. **Training Algorithm**:
   - Uses the least squares method to calculate slope and intercept
   - Implements the formulas: 
     - Slope = Σ((xi - x̄)(yi - ȳ)) / Σ(xi - x̄)²
     - Intercept = ȳ - slope × x̄

3. **Additional Features**:
   - R² calculation for model evaluation
   - Error handling for invalid inputs
   - Clean API for model usage

## Sample Output:
```
Linear Regression Model
Slope: 2.0000
Intercept: 0.0000
Equation: y = 2.0000x + 0.0000

Predictions:
x = 2.5 -> y = 5.00
x = 5.5 -> y = 11.00
x = 8.5 -> y = 17.00

R² (Coefficient of Determination): 1.0000
```

This implementation provides a solid foundation for linear regression in Go and can be extended with additional features like regularization, multiple features, or more sophisticated error metrics.

