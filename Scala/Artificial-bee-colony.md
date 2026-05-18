# Artificial Bee Colony Algorithm in Scala

Here's an implementation of the Artificial Bee Colony (ABC) algorithm in Scala for solving optimization problems:

```scala
import scala.util.Random

case class Solution(position: Array[Double], fitness: Double)

class ArtificialBeeColony(
    dimensions: Int,
    searchSpace: Array[(Double, Double)],
    maxIterations: Int,
    populationSize: Int,
    limit: Int
) {
  
  private val random = new Random()
  
  // Initialize a random solution within search space
  private def randomSolution(): Solution = {
    val position = Array.fill(dimensions) {
      val (min, max) = searchSpace(0)
      min + random.nextDouble() * (max - min)
    }
    val fitness = calculateFitness(position)
    Solution(position, fitness)
  }
  
  // Example objective function (minimize sphere function)
  private def calculateFitness(position: Array[Double]): Double = {
    position.map(x => x * x).sum
  }
  
  // Generate a new solution using neighbor search
  private def generateNewSolution(employedBee: Solution, index: Int): Solution = {
    val newSolution = employedBee.position.clone()
    val k = random.nextInt(dimensions)
    val j = random.nextInt(dimensions)
    
    // Generate new solution using employed bee's rule
    val phi = random.nextDouble() * 2.0 - 1.0 // random value in [-1, 1]
    newSolution(k) = employedBee.position(k) + phi * (employedBee.position(k) - employedBee.position(j))
    
    // Ensure bounds
    val (min, max) = searchSpace(k)
    newSolution(k) = math.max(min, math.min(max, newSolution(k)))
    
    val fitness = calculateFitness(newSolution)
    Solution(newSolution, fitness)
  }
  
  def optimize(): Solution = {
    // Initialize population
    var population = Array.fill(populationSize)(randomSolution())
    var globalBest = population.minBy(_.fitness)
    var trialCounts = Array.fill(populationSize)(0)
    
    for (iteration <- 1 to maxIterations) {
      // Employed Bee Phase
      for (i <- population.indices) {
        val newSolution = generateNewSolution(population(i), i)
        
        if (newSolution.fitness < population(i).fitness) {
          population(i) = newSolution
          trialCounts(i) = 0
        } else {
          trialCounts(i) += 1
        }
      }
      
      // Calculate probabilities for onlooker bees
      val fitnessValues = population.map(_.fitness)
      val maxFitness = fitnessValues.max
      val probabilities = fitnessValues.map(f => 1.0 / (1.0 + math.exp(-f + maxFitness)))
      val totalProbability = probabilities.sum
      
      // Onlooker Bee Phase
      for (i <- 0 until populationSize) {
        val rand = random.nextDouble() * totalProbability
        var cumulative = 0.0
        var selected = 0
        
        while (cumulative < rand && selected < populationSize) {
          cumulative += probabilities(selected)
          selected += 1
        }
        selected -= 1
        
        val newSolution = generateNewSolution(population(selected), selected)
        
        if (newSolution.fitness < population(selected).fitness) {
          population(selected) = newSolution
          trialCounts(selected) = 0
        } else {
          trialCounts(selected) += 1
        }
      }
      
      // Scout Bee Phase
      for (i <- population.indices) {
        if (trialCounts(i) >= limit) {
          population(i) = randomSolution()
          trialCounts(i) = 0
        }
      }
      
      // Update global best
      val currentBest = population.minBy(_.fitness)
      if (currentBest.fitness < globalBest.fitness) {
        globalBest = currentBest
      }
      
      // Print progress
      if (iteration % 100 == 0) {
        println(s"Iteration $iteration: Best fitness = ${globalBest.fitness}")
      }
    }
    
    globalBest
  }
}

// Usage example
object ABCExample extends App {
  // Define problem: minimize sphere function in 10 dimensions
  val dimensions = 10
  val searchSpace = Array.fill(dimensions)((-5.0, 5.0)) // Each dimension: [-5, 5]
  
  val abc = new ArtificialBeeColony(
    dimensions = dimensions,
    searchSpace = searchSpace,
    maxIterations = 1000,
    populationSize = 50,
    limit = 100
  )
  
  val result = abc.optimize()
  
  println("=== Artificial Bee Colony Results ===")
  println(s"Best solution fitness: ${result.fitness}")
  println(s"Best solution position: ${result.position.mkString(", ")}")
  println(s"Expected minimum: 0.0")
  
  // Verify solution quality
  val error = math.abs(result.fitness - 0.0)
  println(s"Error from optimal: $error")
}
```

## Key Components Explained

### 1. **Solution Class**
- Represents a candidate solution with position and fitness values
- `position`: Array of decision variables
- `fitness`: Objective function value

### 2. **Algorithm Phases**
- **Employed Bee Phase**: Each employed bee searches for better solutions in its neighborhood
- **Onlooker Bee Phase**: Bees select solutions probabilistically based on fitness
- **Scout Bee Phase**: Abandoned solutions are replaced with new random solutions

### 3. **Parameters**
- `dimensions`: Number of decision variables
- `searchSpace`: Bounds for each dimension
- `maxIterations`: Maximum number of iterations
- `populationSize`: Number of bees in colony
- `limit`: Maximum trials before abandoning a solution

### 4. **Key Features**
- **Boundary Handling**: Ensures solutions stay within valid ranges
- **Probabilistic Selection**: Onlooker bees choose better solutions with higher probability
- **Diversity Maintenance**: Scout bees prevent premature convergence
- **Progress Tracking**: Monitors optimization progress

This implementation provides a complete ABC algorithm framework that can be adapted for various optimization problems by modifying the objective function and search space parameters.

