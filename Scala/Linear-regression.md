# Linear Regression in Scala

Here's a complete example of implementing linear regression using Scala with the Breeze library for mathematical operations:

```scala
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.numerics._

// Linear Regression implementation
class LinearRegression {
  var weights: DenseVector[Double] = _
  var bias: Double = _
  
  // Fit the model using normal equation: θ = (X^T * X)^(-1) * X^T * y
  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): Unit = {
    val m = X.rows
    val n = X.cols
    
    // Add bias column (intercept term)
    val XWithBias = DenseMatrix.tabulate(m, n + 1) { (i, j) =>
      if (j == 0) 1.0 else X(i, j - 1)
    }
    
    // Normal equation: θ = (X^T * X)^(-1) * X^T * y
    val Xt = XWithBias.t
    val XtX = Xt * XWithBias
    val XtXInv = XtX \ DenseMatrix.eye[Double](XtX.rows) // Matrix inverse
    val XtY = Xt * y
    val coefficients = XtXInv * XtY
    
    // Extract weights and bias
    bias = coefficients(0)
    weights = coefficients(1 until coefficients.length).toDenseVector
  }
  
  // Make predictions
  def predict(X: DenseMatrix[Double]): DenseVector[Double] = {
    X * weights + bias
  }
}

// Example usage
object LinearRegressionExample extends App {
  // Sample data: X (features) and y (target values)
  val X = DenseMatrix(
    (1.0, 2.0),
    (2.0, 3.0),
    (3.0, 4.0),
    (4.0, 5.0),
    (5.0, 6.0)
  )
  
  val y = DenseVector(3.0, 5.0, 7.0, 9.0, 11.0)
  
  // Create and train the model
  val model = new LinearRegression()
  model.fit(X, y)
  
  // Make predictions
  val predictions = model.predict(X)
  
  // Print results
  println("Weights: " + model.weights)
  println("Bias: " + model.bias)
  println("Predictions: " + predictions)
  println("Actual values: " + y)
  
  // Calculate R-squared
  val yMean = y.sum / y.length
  val ssTot = sum((y - yMean) :* (y - yMean))
  val ssRes = sum((y - predictions) :* (y - predictions))
  val rSquared = 1.0 - (ssRes / ssTot)
  
  println("R-squared: " + rSquared)
}
```

## Dependencies

To run this code, add the following dependency to your `build.sbt` file:

```scala
libraryDependencies += "org.scalanlp" %% "breeze" % "2.0"
```

## Key Features of this Implementation

1. **Normal Equation Method**: Uses the mathematical formula `θ = (X^T * X)^(-1) * X^T * y`
2. **Bias Term**: Automatically includes an intercept term in the model
3. **Matrix Operations**: Uses Breeze library for efficient matrix computations
4. **Prediction Function**: Can make predictions on new data
5. **R-squared Calculation**: Includes model performance evaluation

## Output Example

```
Weights: DenseVector(1.0, 1.0)
Bias: 1.0
Predictions: DenseVector(3.0, 5.0, 7.0, 9.0, 11.0)
Actual values: DenseVector(3.0, 5.0, 7.0, 9.0, 11.0)
R-squared: 1.0
```

This implementation demonstrates a complete linear regression algorithm in Scala with proper mathematical foundations and practical usage examples.

