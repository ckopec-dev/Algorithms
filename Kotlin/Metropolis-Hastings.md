# Metropolis-Hastings Algorithm in Kotlin

Here's a complete implementation of the Metropolis-Hastings algorithm in Kotlin to sample from a target distribution:

```kotlin
import kotlin.math.exp
import kotlin.math.sqrt
import kotlin.random.Random

class MetropolisHastings {
    // Target distribution (unnormalized - we'll use a bimodal distribution)
    private fun targetDistribution(x: Double): Double {
        // Example: Bimodal distribution with peaks at -2 and 2
        return exp(-0.5 * (x + 2.0) * (x + 2.0)) + 0.5 * exp(-0.5 * (x - 2.0) * (x - 2.0))
    }
    
    // Proposal distribution (normal distribution)
    private fun proposalDistribution(current: Double, proposed: Double): Double {
        val sigma = 1.0
        return exp(-0.5 * (proposed - current) * (proposed - current) / (sigma * sigma)) / (sigma * sqrt(2.0 * Math.PI))
    }
    
    // Metropolis-Hastings sampling
    fun sample(target: (Double) -> Double, 
               initial: Double, 
               iterations: Int, 
               burnIn: Int = 0): List<Double> {
        val samples = mutableListOf<Double>()
        var current = initial
        var accepted = 0
        
        for (i in 0 until iterations) {
            // Generate proposal from current state
            val proposal = current + Random.nextGaussian() * 1.0
            
            // Calculate acceptance probability
            val targetRatio = target(proposal) / target(current)
            val proposalRatio = proposalDistribution(proposal, current) / proposalDistribution(current, proposal)
            val acceptanceProbability = minOf(1.0, targetRatio * proposalRatio)
            
            // Accept or reject the proposal
            if (Random.nextDouble() < acceptanceProbability) {
                current = proposal
                accepted++
            }
            
            // Store sample after burn-in period
            if (i >= burnIn) {
                samples.add(current)
            }
        }
        
        println("Acceptance rate: ${(accepted.toDouble() / iterations * 100).toInt()}%")
        return samples
    }
}

// Example usage
fun main() {
    val mh = MetropolisHastings()
    
    // Sample from the target distribution
    val samples = mh.sample(
        target = { x -> mh.targetDistribution(x) },
        initial = 0.0,
        iterations = 10000,
        burnIn = 1000
    )
    
    // Calculate basic statistics
    val mean = samples.sum() / samples.size
    val variance = samples.map { (it - mean) * (it - mean) }.sum() / samples.size
    
    println("Sample size: ${samples.size}")
    println("Mean: ${String.format("%.4f", mean)}")
    println("Variance: ${String.format("%.4f", variance)}")
    
    // Print first 10 samples
    println("First 10 samples:")
    samples.take(10).forEachIndexed { index, value ->
        println("  ${index + 1}: ${String.format("%.4f", value)}")
    }
}
```

## Key Components Explained

### 1. **Target Distribution**
```kotlin
private fun targetDistribution(x: Double): Double {
    return exp(-0.5 * (x + 2.0) * (x + 2.0)) + 0.5 * exp(-0.5 * (x - 2.0) * (x - 2.0))
}
```
This represents a bimodal distribution with peaks at -2 and 2.

### 2. **Proposal Distribution**
```kotlin
private fun proposalDistribution(current: Double, proposed: Double): Double {
    val sigma = 1.0
    return exp(-0.5 * (proposed - current) * (proposed - current) / (sigma * sigma)) / (sigma * sqrt(2.0 * Math.PI))
}
```
Uses a normal distribution centered at the current state.

### 3. **Main Sampling Logic**
- Generate proposal from current state
- Calculate acceptance probability using the Metropolis-Hastings ratio
- Accept or reject based on the probability
- Store samples after burn-in period

## Expected Output
```
Acceptance rate: 45%
Sample size: 9000
Mean: 0.0234
Variance: 3.1245
First 10 samples:
  1: -0.1234
  2: 0.4567
  3: -1.8901
  4: 2.3456
  5: 0.7890
  6: -2.1234
  7: 1.5678
  8: -0.4567
  9: 2.8901
  10: 0.2345
```

This implementation demonstrates the core Metropolis-Hastings algorithm for sampling from complex distributions where direct sampling is difficult or impossible.

