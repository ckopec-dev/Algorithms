# Monte Carlo Method Implementation in Kotlin

Here's an example of using the Monte Carlo method to estimate the value of π:

```kotlin
import kotlin.math.sqrt
import kotlin.random.Random

fun estimatePi(numPoints: Int): Double {
    var pointsInsideCircle = 0
    
    for (i in 0 until numPoints) {
        // Generate random point in unit square [0,1] x [0,1]
        val x = Random.nextDouble()
        val y = Random.nextDouble()
        
        // Calculate distance from origin
        val distance = sqrt(x * x + y * y)
        
        // Check if point is inside unit circle
        if (distance <= 1.0) {
            pointsInsideCircle++
        }
    }
    
    // π ≈ 4 * (points inside circle / total points)
    return 4.0 * pointsInsideCircle.toDouble() / numPoints
}

fun main() {
    val numPoints = 1000000
    val piEstimate = estimatePi(numPoints)
    
    println("Estimated π using $numPoints points: $piEstimate")
    println("Actual π: ${Math.PI}")
    println("Error: ${abs(piEstimate - Math.PI)}")
}

fun abs(x: Double): Double = if (x < 0) -x else x
```

## How it works:

1. **Random Point Generation**: Generate random points (x, y) in the unit square [0,1] × [0,1]
2. **Circle Test**: Calculate the distance from origin using the formula √(x² + y²)
3. **Counting**: If distance ≤ 1, the point is inside the unit circle
4. **Estimation**: π ≈ 4 × (points inside circle / total points)

## Alternative Implementation with Better Random Generation:

```kotlin
import kotlin.math.sqrt
import kotlin.random.Random

fun estimatePiOptimized(numPoints: Int): Double {
    var pointsInsideCircle = 0
    val random = Random
    
    repeat(numPoints) {
        val x = random.nextDouble(-1.0, 1.0)
        val y = random.nextDouble(-1.0, 1.0)
        
        if (x * x + y * y <= 1.0) {
            pointsInsideCircle++
        }
    }
    
    return 4.0 * pointsInsideCircle.toDouble() / numPoints
}

// Usage example
fun main() {
    val estimates = mutableListOf<Double>()
    
    // Run multiple trials to see convergence
    for (i in 1..10) {
        val estimate = estimatePiOptimized(100000 * i)
        estimates.add(estimate)
        println("Trial ${i}: π ≈ $estimate")
    }
}
```

## Key Features:

- **Random Number Generation**: Uses Kotlin's `Random.nextDouble()`
- **Convergence**: More points = better accuracy
- **Mathematical Foundation**: Based on the ratio of circle area to square area
- **Scalable**: Can be easily modified for different precision requirements

The Monte Carlo method demonstrates how random sampling can be used to solve mathematical problems that might be difficult to solve analytically.

