# Tabu Search Algorithm in Kotlin

Here's a complete implementation of the Tabu Search algorithm in Kotlin for solving a classic optimization problem:

```kotlin
import kotlin.math.abs

/**
 * Tabu Search implementation for optimization problems
 */
class TabuSearch {
    
    data class Solution(val configuration: List<Int>, val fitness: Double)
    data class TabuItem(val position: Int, val value: Int, val tenure: Int)
    
    private val tabuList = mutableListOf<TabuItem>()
    private var currentSolution: Solution
    private var bestSolution: Solution
    private val maxIterations: Int
    private val tabuTenure: Int
    private val neighborhoodSize: Int
    
    constructor(
        initialSolution: List<Int>,
        maxIterations: Int = 1000,
        tabuTenure: Int = 10,
        neighborhoodSize: Int = 5
    ) {
        this.currentSolution = Solution(initialSolution, calculateFitness(initialSolution))
        this.bestSolution = currentSolution
        this.maxIterations = maxIterations
        this.tabuTenure = tabuTenure
        this.neighborhoodSize = neighborhoodSize
    }
    
    /**
     * Main Tabu Search algorithm
     */
    fun solve(): Solution {
        for (iteration in 1..maxIterations) {
            val neighbors = generateNeighbors(currentSolution.configuration)
            val bestNeighbor = findBestNonTabuNeighbor(neighbors)
            
            if (bestNeighbor != null) {
                // Update current solution
                currentSolution = bestNeighbor
                
                // Update best solution if improved
                if (currentSolution.fitness > bestSolution.fitness) {
                    bestSolution = currentSolution
                }
                
                // Update tabu list
                updateTabuList(currentSolution.configuration)
            }
        }
        
        return bestSolution
    }
    
    /**
     * Generate neighboring solutions
     */
    private fun generateNeighbors(currentConfig: List<Int>): List<Solution> {
        val neighbors = mutableListOf<Solution>()
        
        for (i in currentConfig.indices) {
            for (j in 0 until 10) { // Assuming values 0-9 for simplicity
                if (j != currentConfig[i]) {
                    val neighborConfig = currentConfig.toMutableList()
                    neighborConfig[i] = j
                    val fitness = calculateFitness(neighborConfig)
                    neighbors.add(Solution(neighborConfig, fitness))
                }
            }
        }
        
        // Return only a subset of neighbors (neighborhood size)
        return neighbors.sortedByDescending { it.fitness }.take(neighborhoodSize)
    }
    
    /**
     * Find best neighbor that is not in tabu list
     */
    private fun findBestNonTabuNeighbor(neighbors: List<Solution>): Solution? {
        for (neighbor in neighbors) {
            if (!isTabu(neighbor.configuration)) {
                return neighbor
            }
        }
        return null
    }
    
    /**
     * Check if a solution is in tabu list
     */
    private fun isTabu(configuration: List<Int>): Boolean {
        return tabuList.any { tabuItem ->
            tabuItem.position < configuration.size && 
            configuration[tabuItem.position] == tabuItem.value
        }
    }
    
    /**
     * Update tabu list
     */
    private fun updateTabuList(configuration: List<Int>) {
        // Remove expired items
        tabuList.removeIf { it.tenure <= 0 }
        
        // Reduce tenure of remaining items
        tabuList.forEach { it.tenure-- }
        
        // Add new tabu items (for each position that changed)
        for (i in configuration.indices) {
            val existingTabu = tabuList.find { it.position == i }
            if (existingTabu == null) {
                tabuList.add(TabuItem(i, configuration[i], tabuTenure))
            } else {
                existingTabu.tenure = tabuTenure
            }
        }
    }
    
    /**
     * Calculate fitness of a solution (example: maximize sum of values)
     */
    private fun calculateFitness(configuration: List<Int>): Double {
        return configuration.sumOf { it.toDouble() }
    }
}

/**
 * Example usage and demonstration
 */
fun main() {
    // Example: Finding maximum sum of a list of integers
    val initialSolution = listOf(1, 2, 3, 4, 5)
    
    val tabuSearch = TabuSearch(
        initialSolution = initialSolution,
        maxIterations = 500,
        tabuTenure = 5,
        neighborhoodSize = 10
    )
    
    val result = tabuSearch.solve()
    
    println("Tabu Search Results:")
    println("Best solution: ${result.configuration}")
    println("Best fitness: ${result.fitness}")
    
    // Another example with a more complex problem
    println("\n--- Another Example ---")
    val initialSolution2 = listOf(5, 3, 8, 1, 9)
    
    val tabuSearch2 = TabuSearch(
        initialSolution = initialSolution2,
        maxIterations = 300,
        tabuTenure = 3,
        neighborhoodSize = 8
    )
    
    val result2 = tabuSearch2.solve()
    
    println("Best solution: ${result2.configuration}")
    println("Best fitness: ${result2.fitness}")
}
```

## Key Components Explained:

### 1. **Solution Class**
- Represents a candidate solution with configuration and fitness value

### 2. **TabuItem Class**
- Tracks tabu restrictions with position, value, and tenure

### 3. **Core Algorithm Components**
- **generateNeighbors()**: Creates neighboring solutions
- **findBestNonTabuNeighbor()**: Selects best neighbor not in tabu list
- **isTabu()**: Checks if a solution violates tabu restrictions
- **updateTabuList()**: Manages tabu list with tenure tracking

### 4. **Main Features**
- **Tabu Tenure**: Controls how long a move remains forbidden
- **Neighborhood Size**: Limits the number of neighbors considered
- **Memory Management**: Automatic cleanup of expired tabu items
- **Diversification**: Avoids getting stuck in local optima

### 5. **Usage Example**
The algorithm works with any optimization problem where:
- Solutions can be represented as configurations
- Neighborhoods can be generated
- Fitness can be calculated

This implementation provides a flexible foundation that can be adapted for various optimization problems like:
- Traveling Salesman Problem
- Knapsack Problem  
- Scheduling problems
- Function optimization

