# Artificial Bee Colony Algorithm in Kotlin

```kotlin
import kotlin.math.*

class ArtificialBeeColony(
    private val objectiveFunction: (DoubleArray) -> Double,
    private val bounds: Array<DoubleArray>,
    private val populationSize: Int = 20,
    private val maxIterations: Int = 1000
) {
    
    data class Bee(val position: DoubleArray, var fitness: Double = 0.0)
    
    private val dimension = bounds.size
    private val employedBees = mutableListOf<Bee>()
    private val onlookerBees = mutableListOf<Bee>()
    private val scoutBees = mutableListOf<Bee>()
    
    private var globalBest: Bee? = null
    
    fun optimize(): DoubleArray {
        initializePopulation()
        var iteration = 0
        
        while (iteration < maxIterations) {
            // Employed Bee Phase
            for (i in employedBees.indices) {
                val newSolution = generateNewSolution(employedBees[i])
                val newFitness = objectiveFunction(newSolution)
                
                if (newFitness < employedBees[i].fitness) {
                    employedBees[i] = Bee(newSolution, newFitness)
                    if (newFitness < globalBest?.fitness ?: Double.MAX_VALUE) {
                        globalBest = Bee(newSolution, newFitness)
                    }
                }
            }
            
            // Calculate probabilities for onlooker bees
            val totalFitness = employedBees.sumOf { 1.0 / (it.fitness + 1e-10) }
            val probabilities = employedBees.map { 1.0 / (it.fitness + 1e-10) / totalFitness }
            
            // Onlooker Bee Phase
            for (i in 0 until populationSize / 2) {
                val selectedBeeIndex = selectBee(probabilities)
                val newSolution = generateNewSolution(employedBees[selectedBeeIndex])
                val newFitness = objectiveFunction(newSolution)
                
                if (newFitness < employedBees[selectedBeeIndex].fitness) {
                    employedBees[selectedBeeIndex] = Bee(newSolution, newFitness)
                    if (newFitness < globalBest?.fitness ?: Double.MAX_VALUE) {
                        globalBest = Bee(newSolution, newFitness)
                    }
                }
            }
            
            // Scout Bee Phase
            for (i in employedBees.indices) {
                if (isScoutNeeded(i)) {
                    employedBees[i] = generateRandomBee()
                }
            }
            
            iteration++
        }
        
        return globalBest?.position ?: DoubleArray(dimension) { 0.0 }
    }
    
    private fun initializePopulation() {
        for (i in 0 until populationSize) {
            val bee = generateRandomBee()
            val fitness = objectiveFunction(bee.position)
            employedBees.add(Bee(bee.position, fitness))
            
            if (globalBest == null || fitness < globalBest!!.fitness) {
                globalBest = Bee(bee.position, fitness)
            }
        }
    }
    
    private fun generateRandomBee(): Bee {
        val position = DoubleArray(dimension) { 
            val (min, max) = bounds[it]
            min + (max - min) * Math.random()
        }
        return Bee(position, 0.0)
    }
    
    private fun generateNewSolution(bee: Bee): DoubleArray {
        val newSolution = bee.position.copyOf()
        val randomIndex = (0 until dimension).random()
        val randomBeeIndex = (0 until populationSize).random()
        
        val (min, max) = bounds[randomIndex]
        val phi = (Math.random() * 2.0 - 1.0) * 0.5 // -0.5 to 0.5
        
        newSolution[randomIndex] = bee.position[randomIndex] + 
            phi * (bee.position[randomIndex] - employedBees[randomBeeIndex].position[randomIndex])
        
        // Ensure bounds
        newSolution[randomIndex] = newSolution[randomIndex].coerceIn(min, max)
        
        return newSolution
    }
    
    private fun selectBee(probabilities: List<Double>): Int {
        val rand = Math.random()
        var cumulative = 0.0
        
        for ((index, prob) in probabilities.withIndex()) {
            cumulative += prob
            if (rand <= cumulative) {
                return index
            }
        }
        return probabilities.size - 1
    }
    
    private fun isScoutNeeded(index: Int): Boolean {
        // Simple scout condition - in real implementation, this would be based on
        // how long a bee hasn't improved
        return Math.random() < 0.1 // 10% chance of becoming scout
    }
}

// Example usage with a simple optimization problem
fun main() {
    // Example: Minimize the Rastrigin function
    val rastriginFunction = { x: DoubleArray ->
        val A = 10.0
        val sum = x.sumOf { it * it - A * cos(2 * PI * it) }
        A * x.size + sum
    }
    
    // Define bounds for each dimension (e.g., -5.12 to 5.12 for Rastrigin)
    val bounds = Array(2) { doubleArrayOf(-5.12, 5.12) }
    
    val abc = ArtificialBeeColony(
        objectiveFunction = rastriginFunction,
        bounds = bounds,
        populationSize = 20,
        maxIterations = 1000
    )
    
    val result = abc.optimize()
    val resultValue = rastriginFunction(result)
    
    println("Optimization Result:")
    println("Best position: [${result.joinToString(", ")}]")
    println("Best fitness: $resultValue")
    
    // Example with Sphere function
    val sphereFunction = { x: DoubleArray ->
        x.sumOf { it * it }
    }
    
    val sphereBounds = Array(3) { doubleArrayOf(-5.0, 5.0) }
    
    val abcSphere = ArtificialBeeColony(
        objectiveFunction = sphereFunction,
        bounds = sphereBounds,
        populationSize = 30,
        maxIterations = 500
    )
    
    val sphereResult = abcSphere.optimize()
    val sphereResultValue = sphereFunction(sphereResult)
    
    println("\nSphere Function Result:")
    println("Best position: [${sphereResult.joinToString(", ")}]")
    println("Best fitness: $sphereResultValue")
}
```

## Key Features of this Implementation:

1. **Core ABC Components**:
   - Employed bees that explore solutions
   - Onlooker bees that probabilistically select good solutions
   - Scout bees that replace abandoned solutions

2. **Algorithm Phases**:
   - **Employed Bee Phase**: Each employed bee searches for better solutions
   - **Onlooker Bee Phase**: Onlookers select bees based on fitness probability
   - **Scout Bee Phase**: Abandoned solutions are replaced

3. **Key Parameters**:
   - Population size
   - Maximum iterations
   - Search space bounds
   - Objective function to minimize

4. **Example Functions**:
   - Rastrigin function (multi-modal optimization)
   - Sphere function (simple quadratic optimization)

This implementation demonstrates the fundamental principles of the Artificial Bee Colony algorithm while being practical for real optimization problems.

