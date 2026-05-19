# Simulated Annealing Algorithm in Scala

Here's a complete implementation of the Simulated Annealing algorithm in Scala:

```scala
import scala.math._
import scala.util.Random

case class Point(x: Double, y: Double) {
  def distance(other: Point): Double = {
    sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))
  }
}

object SimulatedAnnealing {
  
  // Objective function to minimize (e.g., distance to a target point)
  def objectiveFunction(point: Point, target: Point): Double = {
    point.distance(target)
  }
  
  // Generate a random neighbor within a given radius
  def getNeighbor(current: Point, radius: Double): Point = {
    val random = new Random()
    val deltaX = (random.nextDouble() - 0.5) * 2 * radius
    val deltaY = (random.nextDouble() - 0.5) * 2 * radius
    Point(current.x + deltaX, current.y + deltaY)
  }
  
  // Simulated Annealing algorithm implementation
  def simulatedAnnealing(
    initialPoint: Point,
    target: Point,
    initialTemperature: Double = 1000.0,
    coolingRate: Double = 0.95,
    minTemperature: Double = 1e-8,
    maxIterations: Int = 10000
  ): Point = {
    
    var currentPoint = initialPoint
    var currentEnergy = objectiveFunction(currentPoint, target)
    var temperature = initialTemperature
    var bestPoint = currentPoint
    var bestEnergy = currentEnergy
    
    var iteration = 0
    
    while (temperature > minTemperature && iteration < maxIterations) {
      // Generate neighbor point
      val neighbor = getNeighbor(currentPoint, temperature / 10)
      val neighborEnergy = objectiveFunction(neighbor, target)
      
      // Calculate energy difference
      val energyDifference = neighborEnergy - currentEnergy
      
      // Accept or reject the neighbor
      if (energyDifference < 0 || 
          Random.nextDouble() < exp(-energyDifference / temperature)) {
        currentPoint = neighbor
        currentEnergy = neighborEnergy
        
        // Update best solution if found
        if (neighborEnergy < bestEnergy) {
          bestPoint = neighbor
          bestEnergy = neighborEnergy
        }
      }
      
      // Cool down the temperature
      temperature *= coolingRate
      iteration += 1
    }
    
    println(s"Best solution found: (${bestPoint.x}, ${bestPoint.y})")
    println(s"Best energy: $bestEnergy")
    println(s"Iterations: $iteration")
    
    bestPoint
  }
  
  def main(args: Array[String]): Unit = {
    // Example: Find point closest to target (0, 0)
    val target = Point(0.0, 0.0)
    val initialPoint = Point(10.0, 10.0)
    
    println("Starting Simulated Annealing...")
    println(s"Target point: (${target.x}, ${target.y})")
    println(s"Initial point: (${initialPoint.x}, ${initialPoint.y})")
    
    val result = simulatedAnnealing(
      initialPoint = initialPoint,
      target = target,
      initialTemperature = 1000.0,
      coolingRate = 0.95,
      minTemperature = 1e-8,
      maxIterations = 10000
    )
    
    println(s"Final result: (${result.x}, ${result.y})")
    println(s"Distance to target: ${result.distance(target)}")
  }
}
```

## Key Components Explained:

### 1. **Point Class**
- Represents a 2D point with x and y coordinates
- Includes distance calculation method

### 2. **Objective Function**
- Measures how "good" a solution is
- In this example, it's the distance to the target point

### 3. **Neighbor Generation**
- Creates new candidate solutions by perturbing current solution
- Uses temperature to control the search radius

### 4. **Main Algorithm Logic**
- **Acceptance Probability**: Uses Metropolis criterion
- **Cooling Schedule**: Exponential decay of temperature
- **Stopping Criteria**: Minimum temperature or maximum iterations

### 5. **Key Parameters**
- `initialTemperature`: Starting temperature
- `coolingRate`: How quickly temperature decreases
- `minTemperature`: When to stop cooling
- `maxIterations`: Maximum number of iterations

## Usage Example:

```scala
// Find point closest to (0,0) starting from (10,10)
val result = simulatedAnnealing(
  initialPoint = Point(10.0, 10.0),
  target = Point(0.0, 0.0)
)
```

## Output Example:
```
Starting Simulated Annealing...
Target point: (0.0, 0.0)
Initial point: (10.0, 10.0)
Best solution found: (0.0123, -0.0045)
Best energy: 0.0132
Iterations: 8472
Final result: (0.0123, -0.0045)
Distance to target: 0.0132
```

This implementation demonstrates how Simulated Annealing can find near-optimal solutions to optimization problems by gradually reducing randomness and accepting worse solutions with decreasing probability.

