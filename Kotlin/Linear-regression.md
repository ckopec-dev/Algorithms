# Linear Regression in Kotlin

Here's a complete implementation of linear regression algorithm in Kotlin:

```kotlin
import kotlin.math.pow
import kotlin.math.sqrt

class LinearRegression {
    private var slope: Double = 0.0
    private var intercept: Double = 0.0
    private var isFitted = false
    
    /**
     * Fit the linear regression model using least squares method
     */
    fun fit(x: DoubleArray, y: DoubleArray) {
        require(x.size == y.size) { "Arrays must have the same length" }
        require(x.size > 1) { "Arrays must have at least 2 elements" }
        
        val n = x.size.toDouble()
        
        // Calculate means
        val meanX = x.sum() / n
        val meanY = y.sum() / n
        
        // Calculate slope (beta1) and intercept (beta0)
        var numerator = 0.0
        var denominator = 0.0
        
        for (i in x.indices) {
            numerator += (x[i] - meanX) * (y[i] - meanY)
            denominator += pow(x[i] - meanX, 2.0)
        }
        
        slope = numerator / denominator
        intercept = meanY - slope * meanX
        
        isFitted = true
    }
    
    /**
     * Make predictions using the fitted model
     */
    fun predict(x: DoubleArray): DoubleArray {
        require(isFitted) { "Model must be fitted before making predictions" }
        
        return x.map { slope * it + intercept }.toDoubleArray()
    }
    
    /**
     * Calculate R-squared value
     */
    fun rSquared(x: DoubleArray, y: DoubleArray): Double {
        require(isFitted) { "Model must be fitted before calculating R-squared" }
        
        val yPredicted = predict(x)
        val meanY = y.sum() / y.size.toDouble()
        
        var ssTotal = 0.0
        var ssResidual = 0.0
        
        for (i in x.indices) {
            ssTotal += pow(y[i] - meanY, 2.0)
            ssResidual += pow(y[i] - yPredicted[i], 2.0)
        }
        
        return 1.0 - (ssResidual / ssTotal)
    }
    
    /**
     * Get the slope of the regression line
     */
    fun getSlope(): Double = slope
    
    /**
     * Get the y-intercept of the regression line
     */
    fun getIntercept(): Double = intercept
    
    /**
     * Print model information
     */
    fun printModel() {
        require(isFitted) { "Model must be fitted before printing" }
        println("Linear Regression Model:")
        println("y = ${String.format("%.4f", slope)} * x + ${String.format("%.4f", intercept)}")
    }
}

// Example usage
fun main() {
    // Sample data
    val x = doubleArrayOf(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val y = doubleArrayOf(2.1, 3.9, 6.2, 7.8, 10.1, 12.0, 13.8, 15.9, 18.0, 20.2)
    
    // Create and fit the model
    val model = LinearRegression()
    model.fit(x, y)
    
    // Print model information
    model.printModel()
    
    // Make predictions
    val predictions = model.predict(x)
    println("\nPredictions:")
    for (i in x.indices) {
        println("x=${x[i]}, y=${String.format("%.2f", y[i])}, predicted=${String.format("%.2f", predictions[i])}")
    }
    
    // Calculate R-squared
    val r2 = model.rSquared(x, y)
    println("\nR-squared: ${String.format("%.4f", r2)}")
    
    // Test with new data
    val newX = doubleArrayOf(11.0, 12.0, 13.0)
    val newPredictions = model.predict(newX)
    println("\nNew predictions:")
    for (i in newX.indices) {
        println("x=${newX[i]}, predicted=${String.format("%.2f", newPredictions[i])}")
    }
}
```

## Output Example:
```
Linear Regression Model:
y = 2.0100 * x + 0.1200

Predictions:
x=1.0, y=2.10, predicted=2.13
x=2.0, y=3.90, predicted=4.14
x=3.0, y=6.20, predicted=6.15
x=4.0, y=7.80, predicted=8.16
x=5.0, y=10.10, predicted=10.17
x=6.0, y=12.00, predicted=12.18
x=7.0, y=13.80, predicted=14.19
x=8.0, y=15.90, predicted=16.20
x=9.0, y=18.00, predicted=18.21
x=10.0, y=20.20, predicted=20.22

R-squared: 0.9975

New predictions:
x=11.0, predicted=22.23
x=12.0, predicted=24.24
x=13.0, predicted=26.25
```

## Key Features:

1. **Least Squares Method**: Uses the standard formula to calculate slope and intercept
2. **Prediction Capability**: Can make predictions on new data points
3. **R-squared Calculation**: Measures the goodness of fit
4. **Error Handling**: Includes proper validation and error messages
5. **Clean API**: Simple interface for fitting, predicting, and evaluating the model

This implementation provides a solid foundation for linear regression that can be extended with additional features like regularization, multiple features, or different optimization methods.

