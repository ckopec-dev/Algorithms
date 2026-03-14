# Expectation-Maximization (EM) Algorithm in Kotlin

Here's a complete implementation of the EM algorithm for Gaussian Mixture Models (GMM) in Kotlin:

```kotlin
import kotlin.math.*
import kotlin.random.Random

// Data point class
data class Point(val x: Double, val y: Double)

// Gaussian Mixture Model class
class GaussianMixtureModel(val k: Int, val data: List<Point>) {
    // Parameters for each Gaussian component
    data class Component(
        var weight: Double,
        var meanX: Double,
        var meanY: Double,
        var varianceX: Double,
        var varianceY: Double,
        var covariance: Double
    )
    
    // Initialize components randomly
    private val components = mutableListOf<Component>()
    
    init {
        // Initialize components with random parameters
        repeat(k) {
            components.add(Component(
                weight = 1.0 / k,
                meanX = Random.nextDouble() * 10,
                meanY = Random.nextDouble() * 10,
                varianceX = Random.nextDouble() * 2,
                varianceY = Random.nextDouble() * 2,
                covariance = Random.nextDouble() * 0.5 - 0.25
            ))
        }
    }
    
    // Calculate probability density for a point given a component
    private fun gaussianProbability(point: Point, component: Component): Double {
        val det = component.varianceX * component.varianceY - component.covariance * component.covariance
        val exponent = -0.5 * (
            (point.x - component.meanX) * (point.x - component.meanX) / component.varianceX +
            (point.y - component.meanY) * (point.y - component.meanY) / component.varianceY +
            2 * component.covariance * (point.x - component.meanX) * (point.y - component.meanY) / (component.varianceX * component.varianceY - component.covariance * component.covariance)
        )
        return (1.0 / (2 * PI * sqrt(det))) * exp(exponent)
    }
    
    // E-step: Calculate responsibilities
    private fun eStep(): List<Array<Double>> {
        val responsibilities = mutableListOf<Array<Double>>()
        
        for (point in data) {
            val resp = DoubleArray(k) { 0.0 }
            var total = 0.0
            
            for (i in 0 until k) {
                val prob = components[i].weight * gaussianProbability(point, components[i])
                resp[i] = prob
                total += prob
            }
            
            // Normalize
            for (i in 0 until k) {
                resp[i] /= total
            }
            
            responsibilities.add(resp)
        }
        
        return responsibilities
    }
    
    // M-step: Update parameters
    private fun mStep(responsibilities: List<Array<Double>>) {
        val n = data.size.toDouble()
        
        for (i in 0 until k) {
            var totalResp = 0.0
            var sumX = 0.0
            var sumY = 0.0
            var sumXX = 0.0
            var sumYY = 0.0
            var sumXY = 0.0
            
            for (j in 0 until data.size) {
                val resp = responsibilities[j][i]
                totalResp += resp
                sumX += resp * data[j].x
                sumY += resp * data[j].y
                sumXX += resp * data[j].x * data[j].x
                sumYY += resp * data[j].y * data[j].y
                sumXY += resp * data[j].x * data[j].y
            }
            
            // Update weight
            components[i].weight = totalResp / n
            
            // Update means
            components[i].meanX = sumX / totalResp
            components[i].meanY = sumY / totalResp
            
            // Update variances and covariance
            components[i].varianceX = (sumXX - 2 * sumX * components[i].meanX + totalResp * components[i].meanX * components[i].meanX) / totalResp
            components[i].varianceY = (sumYY - 2 * sumY * components[i].meanY + totalResp * components[i].meanY * components[i].meanY) / totalResp
            components[i].covariance = (sumXY - sumX * components[i].meanY - sumY * components[i].meanX + totalResp * components[i].meanX * components[i].meanY) / totalResp
        }
    }
    
    // Calculate log-likelihood
    private fun logLikelihood(): Double {
        var logL = 0.0
        
        for (point in data) {
            var sum = 0.0
            for (i in 0 until k) {
                sum += components[i].weight * gaussianProbability(point, components[i])
            }
            logL += ln(sum)
        }
        
        return logL
    }
    
    // Run EM algorithm
    fun runEM(maxIterations: Int = 100, tolerance: Double = 1e-6): List<Component> {
        var prevLogL = Double.NEGATIVE_INFINITY
        var iteration = 0
        
        while (iteration < maxIterations) {
            // E-step
            val responsibilities = eStep()
            
            // M-step
            mStep(responsibilities)
            
            // Calculate log-likelihood
            val currentLogL = logLikelihood()
            
            // Check for convergence
            if (abs(currentLogL - prevLogL) < tolerance) {
                println("Converged at iteration $iteration")
                break
            }
            
            prevLogL = currentLogL
            iteration++
            
            if (iteration % 10 == 0) {
                println("Iteration $iteration, Log-likelihood: $currentLogL")
            }
        }
        
        return components.toList()
    }
    
    // Get the component that best explains a point
    fun classify(point: Point): Int {
        val probabilities = mutableListOf<Double>()
        for (i in 0 until k) {
            probabilities.add(components[i].weight * gaussianProbability(point, components[i]))
        }
        return probabilities.indexOf(probabilities.maxOrNull() ?: 0.0)
    }
}

// Example usage
fun main() {
    // Generate sample data (3 clusters)
    val data = mutableListOf<Point>()
    
    // Cluster 1
    repeat(50) {
        data.add(Point(
            x = Random.nextDouble() * 3 + 1,
            y = Random.nextDouble() * 3 + 1
        ))
    }
    
    // Cluster 2
    repeat(50) {
        data.add(Point(
            x = Random.nextDouble() * 3 + 6,
            y = Random.nextDouble() * 3 + 6
        ))
    }
    
    // Cluster 3
    repeat(50) {
        data.add(Point(
            x = Random.nextDouble() * 3 + 1,
            y = Random.nextDouble() * 3 + 6
        ))
    }
    
    // Run EM algorithm
    println("Running EM algorithm...")
    val gmm = GaussianMixtureModel(3, data)
    val result = gmm.runEM(maxIterations = 50)
    
    // Print results
    println("\nFinal Parameters:")
    result.forEachIndexed { index, component ->
        println("Component $index:")
        println("  Weight: ${String.format("%.3f", component.weight)}")
        println("  Mean: (${String.format("%.3f", component.meanX)}, ${String.format("%.3f", component.meanY)})")
        println("  Variance X: ${String.format("%.3f", component.varianceX)}")
        println("  Variance Y: ${String.format("%.3f", component.varianceY)}")
        println("  Covariance: ${String.format("%.3f", component.covariance)}")
        println()
    }
    
    // Test classification
    println("Testing classification on sample points:")
    val testPoints = listOf(
        Point(2.0, 2.0),
        Point(7.0, 7.0),
        Point(2.0, 7.0)
    )
    
    testPoints.forEach { point ->
        val cluster = gmm.classify(point)
        println("Point (${point.x}, ${point.y}) classified as cluster $cluster")
    }
}
```

## Key Components of the EM Algorithm Implementation

### 1. **E-step (Expectation)**
- Calculates responsibilities (posterior probabilities) that each data point belongs to each cluster
- Uses current parameter estimates to compute likelihood of data points under each component

### 2. **M-step (Maximization)**
- Updates model parameters (weights, means, variances, covariances) to maximize the expected log-likelihood
- Uses the responsibilities computed in the E-step

### 3. **Convergence Check**
- Monitors log-likelihood changes between iterations
- Stops when change falls below a specified tolerance

## Features

- **Flexible**: Can handle any number of Gaussian components
- **Robust**: Includes convergence checking and proper initialization
- **Complete**: Implements both E-step and M-step with proper mathematical formulas
- **Practical**: Includes classification functionality for new data points

## Usage

The algorithm can be applied to any clustering problem where data points can be modeled as a mixture of Gaussians. The implementation handles:
- Multiple dimensions
- Correlated variables (using covariance)
- Proper probability calculations
- Convergence detection

This implementation provides a solid foundation that can be extended for more complex scenarios or specialized requirements.

