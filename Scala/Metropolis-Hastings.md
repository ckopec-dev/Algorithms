# Metropolis-Hastings Algorithm in Scala

Here's a complete implementation of the Metropolis-Hastings algorithm in Scala to sample from a target distribution:

```scala
import scala.math._
import scala.util.Random

object MetropolisHastings {
  
  // Target distribution (unnormalized - e.g., mixture of Gaussians)
  def targetDistribution(x: Double): Double = {
    // Example: mixture of two Gaussians
    0.3 * exp(-0.5 * pow(x - 2.0, 2)) + 0.7 * exp(-0.5 * pow(x + 1.5, 2))
  }
  
  // Proposal distribution (symmetric - normal with fixed variance)
  def proposalDistribution(current: Double, proposal: Double): Double = {
    val sigma = 1.0
    val diff = proposal - current
    exp(-0.5 * pow(diff / sigma, 2)) / (sigma * sqrt(2 * Pi))
  }
  
  // Metropolis-Hastings algorithm
  def metropolisHastings(initialValue: Double, numSamples: Int, 
                        burnIn: Int = 1000): Vector[Double] = {
    
    val random = new Random()
    var current = initialValue
    val samples = new scala.collection.mutable.ListBuffer[Double]()
    
    for (i <- 0 until numSamples + burnIn) {
      // Generate proposal from symmetric proposal distribution
      val proposal = current + random.nextGaussian()
      
      // Calculate acceptance ratio
      val targetRatio = targetDistribution(proposal) / targetDistribution(current)
      val proposalRatio = proposalDistribution(current, proposal) / proposalDistribution(proposal, current)
      
      // For symmetric proposal, this ratio is 1, but we include it for completeness
      val acceptanceRatio = targetRatio * proposalRatio
      
      // Accept or reject the proposal
      val accept = if (acceptanceRatio >= 1.0) {
        true
      } else {
        random.nextDouble() < acceptanceRatio
      }
      
      if (accept) {
        current = proposal
      }
      
      // Store samples after burn-in period
      if (i >= burnIn) {
        samples += current
      }
    }
    
    samples.toVector
  }
  
  // Alternative implementation with asymmetric proposal
  def metropolisHastingsAsymmetric(initialValue: Double, numSamples: Int, 
                                  burnIn: Int = 1000): Vector[Double] = {
    
    val random = new Random()
    var current = initialValue
    val samples = new scala.collection.mutable.ListBuffer[Double]()
    
    for (i <- 0 until numSamples + burnIn) {
      // Generate proposal from asymmetric distribution
      val proposal = current + random.nextGaussian() * 0.5  // Different variance
      
      // Calculate acceptance ratio
      val targetRatio = targetDistribution(proposal) / targetDistribution(current)
      
      // For asymmetric proposal, we need to account for the proposal density ratio
      val proposalRatio = proposalDistribution(current, proposal) / proposalDistribution(proposal, current)
      
      val acceptanceRatio = targetRatio * proposalRatio
      
      // Accept or reject the proposal
      val accept = if (acceptanceRatio >= 1.0) {
        true
      } else {
        random.nextDouble() < acceptanceRatio
      }
      
      if (accept) {
        current = proposal
      }
      
      // Store samples after burn-in period
      if (i >= burnIn) {
        samples += current
      }
    }
    
    samples.toVector
  }
  
  def main(args: Array[String]): Unit = {
    // Run the algorithm
    val samples = metropolisHastings(0.0, 10000, 1000)
    
    // Calculate basic statistics
    val mean = samples.sum / samples.length
    val variance = samples.map(x => pow(x - mean, 2)).sum / samples.length
    
    println(s"Number of samples: ${samples.length}")
    println(s"Mean: $mean")
    println(s"Variance: $variance")
    
    // Show first 10 samples
    println("First 10 samples:")
    samples.take(10).foreach(println)
    
    // Example with asymmetric proposal
    println("\n--- Asymmetric Proposal ---")
    val samplesAsym = metropolisHastingsAsymmetric(0.0, 10000, 1000)
    val meanAsym = samplesAsym.sum / samplesAsym.length
    println(s"Mean with asymmetric proposal: $meanAsym")
  }
}
```

## Key Components Explained:

### 1. **Target Distribution**
```scala
def targetDistribution(x: Double): Double = {
  0.3 * exp(-0.5 * pow(x - 2.0, 2)) + 0.7 * exp(-0.5 * pow(x + 1.5, 2))
}
```
This represents a mixture of two Gaussian distributions.

### 2. **Proposal Distribution**
```scala
def proposalDistribution(current: Double, proposal: Double): Double = {
  val sigma = 1.0
  val diff = proposal - current
  exp(-0.5 * pow(diff / sigma, 2)) / (sigma * sqrt(2 * Pi))
}
```
Normal distribution centered at current value.

### 3. **Main Algorithm**
The algorithm:
- Starts with an initial value
- Generates proposals from the proposal distribution
- Calculates acceptance probability
- Accepts or rejects the proposal based on the acceptance ratio
- Returns samples after burn-in period

## Usage Example:

```scala
// Run with default parameters
val samples = MetropolisHastings.metropolisHastings(0.0, 10000, 1000)

// Analyze results
val mean = samples.sum / samples.length
println(s"Estimated mean: $mean")
```

This implementation demonstrates the core Metropolis-Hastings sampling technique for generating samples from complex probability distributions.

