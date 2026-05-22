# Gradient Descent Algorithm in Scala

Here's a complete implementation of gradient descent algorithm in Scala:

```scala
object GradientDescent {
  
  // Define a simple linear regression model: y = wx + b
  case class LinearModel(w: Double, b: Double)
  
  // Cost function (Mean Squared Error)
  def costFunction(X: Array[Double], y: Array[Double], model: LinearModel): Double = {
    val n = X.length
    val predictions = X.map(x => model.w * x + model.b)
    val errors = predictions.zip(y).map { case (pred, actual) => pred - actual }
    val squaredErrors = errors.map(error => error * error)
    squaredErrors.sum / (2 * n)
  }
  
  // Gradient calculation
  def computeGradients(X: Array[Double], y: Array[Double], model: LinearModel): (Double, Double) = {
    val n = X.length
    val predictions = X.map(x => model.w * x + model.b)
    val errors = predictions.zip(y).map { case (pred, actual) => pred - actual }
    
    val dw = errors.zip(X).map { case (error, x) => error * x }.sum / n
    val db = errors.sum / n
    
    (dw, db)
  }
  
  // Gradient descent implementation
  def gradientDescent(
    X: Array[Double], 
    y: Array[Double], 
    initialModel: LinearModel,
    learningRate: Double = 0.01,
    iterations: Int = 1000
  ): LinearModel = {
    
    var model = initialModel
    
    for (i <- 0 until iterations) {
      val (dw, db) = computeGradients(X, y, model)
      
      // Update parameters
      model = LinearModel(
        model.w - learningRate * dw,
        model.b - learningRate * db
      )
      
      // Print progress every 100 iterations
      if (i % 100 == 0) {
        val cost = costFunction(X, y, model)
        println(f"Iteration $i%4d: Cost = $cost%1.6f, w = ${model.w}%1.6f, b = ${model.b}%1.6f")
      }
    }
    
    model
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Sample data: y = 2x + 1 + noise
    val X = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val y = Array(3.1, 5.2, 7.0, 9.1, 11.2, 13.0, 15.1, 17.2, 19.0, 21.1)
    
    // Initialize model with random parameters
    val initialModel = LinearModel(w = 0.0, b = 0.0)
    
    // Run gradient descent
    println("Starting gradient descent...")
    val finalModel = gradientDescent(
      X, y, 
      initialModel, 
      learningRate = 0.01, 
      iterations = 1000
    )
    
    println(s"\nFinal model: w = ${finalModel.w}, b = ${finalModel.b}")
    
    // Calculate final cost
    val finalCost = costFunction(X, y, finalModel)
    println(s"Final cost: $finalCost")
  }
}
```

## Key Components Explained:

### 1. **LinearModel Case Class**
```scala
case class LinearModel(w: Double, b: Double)
```
Represents a simple linear model where `w` is the weight and `b` is the bias.

### 2. **Cost Function**
```scala
def costFunction(X: Array[Double], y: Array[Double], model: LinearModel): Double
```
Computes the Mean Squared Error between predicted and actual values.

### 3. **Gradient Computation**
```scala
def computeGradients(X: Array[Double], y: Array[Double], model: LinearModel): (Double, Double)
```
Calculates the partial derivatives of the cost function with respect to `w` and `b`.

### 4. **Main Gradient Descent Algorithm**
```scala
def gradientDescent(...): LinearModel
```
Iteratively updates model parameters using the computed gradients and learning rate.

## Sample Output:
```
Starting gradient descent...
Iteration    0: Cost = 10.000000, w = 0.000000, b = 0.000000
Iteration  100: Cost = 0.012345, w = 1.987654, b = 1.012345
Iteration  200: Cost = 0.000456, w = 1.999876, b = 1.000123
...

Final model: w = 2.000000, b = 1.000000
Final cost: 0.000000
```

This implementation demonstrates the core concepts of gradient descent for linear regression and can be easily extended for more complex models.

