# Monte Carlo Method in Scala

Here's an example of using the Monte Carlo method to estimate π using Scala:

```scala
import scala.util.Random

object MonteCarloPi {
  
  /**
   * Estimate π using Monte Carlo method
   * @param numSamples Number of random samples to generate
   * @return Estimated value of π
   */
  def estimatePi(numSamples: Int): Double = {
    val random = new Random()
    
    // Count points inside the unit circle
    val pointsInsideCircle = (1 to numSamples).par
      .map { _ =>
        val x = random.nextDouble() * 2 - 1  // Range [-1, 1]
        val y = random.nextDouble() * 2 - 1  // Range [-1, 1]
        
        // Check if point is inside unit circle (distance from origin <= 1)
        if (x * x + y * y <= 1.0) 1 else 0
      }
      .sum
    
    // π ≈ 4 * (points inside circle / total points)
    4.0 * pointsInsideCircle / numSamples
  }
  
  /**
   * More efficient version using a single loop
   */
  def estimatePiEfficient(numSamples: Int): Double = {
    val random = new Random()
    var insideCircle = 0
    
    for (_ <- 1 to numSamples) {
      val x = random.nextDouble() * 2 - 1
      val y = random.nextDouble() * 2 - 1
      
      if (x * x + y * y <= 1.0) {
        insideCircle += 1
      }
    }
    
    4.0 * insideCircle / numSamples
  }
  
  def main(args: Array[String]): Unit = {
    val numSamples = 1000000
    
    println(s"Estimating π using $numSamples samples...")
    
    // Using parallel processing
    val piEstimate = estimatePi(numSamples)
    println(f"Estimated π: $piEstimate%.6f")
    println(f"Actual π:    ${Math.PI}%.6f")
    println(f"Difference:  ${math.abs(piEstimate - Math.PI)}%.6f")
    
    // Using efficient version
    val piEstimateEfficient = estimatePiEfficient(numSamples)
    println(f"Efficient method: $piEstimateEfficient%.6f")
  }
}
```

## Alternative Example: Integration using Monte Carlo

```scala
import scala.util.Random

object MonteCarloIntegration {
  
  /**
   * Estimate integral of f(x) = x^2 from 0 to 1 using Monte Carlo
   * @param numSamples Number of random samples
   * @return Estimated integral value
   */
  def integrate(xMin: Double, xMax: Double, yMin: Double, yMax: Double, 
               numSamples: Int): Double = {
    
    val random = new Random()
    var underCurve = 0
    
    for (_ <- 1 to numSamples) {
      val x = random.nextDouble() * (xMax - xMin) + xMin
      val y = random.nextDouble() * (yMax - yMin) + yMin
      
      // For f(x) = x^2, we check if y <= x^2
      if (y <= x * x) {
        underCurve += 1
      }
    }
    
    // Area of rectangle * fraction under curve
    val rectangleArea = (xMax - xMin) * (yMax - yMin)
    rectangleArea * underCurve / numSamples
  }
  
  def main(args: Array[String]): Unit = {
    val samples = 1000000
    val result = integrate(0, 1, 0, 1, samples)
    
    println(s"Integral of x² from 0 to 1: $result")
    println(s"Exact value: 1/3 = ${1.0/3}")
    println(s"Difference: ${math.abs(result - 1.0/3)}")
  }
}
```

## Key Features of this Implementation:

1. **Random Number Generation**: Uses Scala's `Random` class for generating uniform random numbers
2. **Parallel Processing**: Demonstrates using `.par` for parallel computation
3. **Mathematical Foundation**: Implements the core Monte Carlo principle of using random sampling
4. **Scalability**: Can handle large numbers of samples efficiently
5. **Accuracy**: Shows how accuracy improves with more samples

## How it works:
- For π estimation: Generate random points in a square and count how many fall inside the inscribed circle
- The ratio of points inside the circle to total points approximates π/4
- Multiply by 4 to get π estimate

The Monte Carlo method is particularly useful for problems where analytical solutions are difficult or impossible to obtain.

