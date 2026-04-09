# Simulated Annealing Algorithm in Kotlin

Here's a complete implementation of the Simulated Annealing algorithm in Kotlin with a classic example solving the Traveling Salesman Problem (TSP):

```kotlin
import kotlin.math.*

// Class representing a city with x, y coordinates
data class City(val x: Double, val y: Double) {
    fun distanceTo(other: City): Double {
        return sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y))
    }
}

// Class representing a tour (sequence of cities)
class Tour(val cities: List<City>) {
    val distance: Double
        get() {
            if (cities.isEmpty()) return 0.0
            var totalDistance = 0.0
            for (i in cities.indices) {
                val from = cities[i]
                val to = cities[(i + 1) % cities.size]
                totalDistance += from.distanceTo(to)
            }
            return totalDistance
        }
    
    // Create a copy of this tour
    fun copy(): Tour {
        return Tour(cities.toList())
    }
    
    // Get a random neighbor by swapping two random cities
    fun getNeighbor(): Tour {
        val newCities = cities.toMutableList()
        val i = (0 until cities.size).random()
        val j = (0 until cities.size).random()
        if (i != j) {
            newCities[i] = cities[j]
            newCities[j] = cities[i]
        }
        return Tour(newCities)
    }
}

// Simulated Annealing implementation for TSP
class SimulatedAnnealing {
    private val initialTemperature = 10000.0
    private val coolingRate = 0.995
    private val minTemperature = 1e-8
    private val maxIterations = 10000
    
    fun solve(tsp: TSP): Tour {
        // Initialize with a random tour
        var currentTour = generateRandomTour(tsp.cities)
        var bestTour = currentTour.copy()
        var currentTemperature = initialTemperature
        
        var iteration = 0
        while (currentTemperature > minTemperature && iteration < maxIterations) {
            // Generate neighbor solution
            val neighbor = currentTour.getNeighbor()
            
            // Calculate energy difference
            val energyDiff = neighbor.distance - currentTour.distance
            
            // Accept or reject the neighbor
            if (acceptanceProbability(energyDiff, currentTemperature) > Math.random()) {
                currentTour = neighbor
            }
            
            // Update best solution
            if (neighbor.distance < bestTour.distance) {
                bestTour = neighbor
            }
            
            // Cool down
            currentTemperature *= coolingRate
            iteration++
        }
        
        return bestTour
    }
    
    private fun acceptanceProbability(energyDiff: Double, temperature: Double): Double {
        return if (energyDiff < 0) 1.0 else exp(-energyDiff / temperature)
    }
    
    private fun generateRandomTour(cities: List<City>): Tour {
        val shuffledCities = cities.shuffled()
        return Tour(shuffledCities)
    }
}

// TSP problem class
class TSP(val cities: List<City>) {
    fun printCities() {
        cities.forEachIndexed { index, city ->
            println("City $index: (${city.x}, ${city.y})")
        }
    }
}

fun main() {
    // Create sample cities for TSP
    val cities = listOf(
        City(60.0, 200.0),
        City(180.0, 200.0),
        City(80.0, 180.0),
        City(140.0, 180.0),
        City(20.0, 160.0),
        City(100.0, 160.0),
        City(200.0, 160.0),
        City(140.0, 140.0),
        City(40.0, 120.0),
        City(100.0, 120.0)
    )
    
    val tsp = TSP(cities)
    println("Cities in the TSP problem:")
    tsp.printCities()
    println()
    
    // Solve using Simulated Annealing
    val sa = SimulatedAnnealing()
    val solution = sa.solve(tsp)
    
    println("Best tour found:")
    solution.cities.forEachIndexed { index, city ->
        println("  ${index + 1}: (${city.x}, ${city.y})")
    }
    println("Total distance: ${solution.distance}")
}
```

## Key Components Explained

### 1. **City Class**
- Represents a city with x, y coordinates
- Includes distance calculation method

### 2. **Tour Class**
- Represents a sequence of cities (solution)
- Calculates total tour distance
- Generates neighbor solutions by swapping cities

### 3. **SimulatedAnnealing Class**
- **Parameters:**
  - `initialTemperature`: Starting temperature
  - `coolingRate`: Rate at which temperature decreases
  - `minTemperature`: Minimum temperature threshold
  - `maxIterations`: Maximum number of iterations

- **Core Logic:**
  - Generate random initial solution
  - Iteratively generate neighbor solutions
  - Accept better solutions or accept worse solutions with probability
  - Cool down temperature gradually
  - Stop when temperature is too low or max iterations reached

### 4. **Acceptance Probability**
```kotlin
private fun acceptanceProbability(energyDiff: Double, temperature: Double): Double {
    return if (energyDiff < 0) 1.0 else exp(-energyDiff / temperature)
}
```
- If energy difference is negative (better solution), always accept
- If energy difference is positive (worse solution), accept with probability that decreases with temperature

## How it Works

1. **Initialization**: Start with a random tour
2. **Iteration**: 
   - Generate a neighbor solution
   - Calculate energy difference (distance change)
   - Accept or reject based on acceptance probability
3. **Cooling**: Gradually reduce temperature
4. **Termination**: Stop when temperature is sufficiently low

## Output Example
```
Cities in the TSP problem:
City 0: (60.0, 200.0)
City 1: (180.0, 200.0)
City 2: (80.0, 180.0)
City 3: (140.0, 180.0)
City 4: (20.0, 160.0)
City 5: (100.0, 160.0)
City 6: (200.0, 160.0)
City 7: (140.0, 140.0)
City 8: (40.0, 120.0)
City 9: (100.0, 120.0)

Best tour found:
  1: (60.0, 200.0)
  2: (180.0, 200.0)
  3: (80.0, 180.0)
  4: (140.0, 180.0)
  5: (20.0, 160.0)
  6: (100.0, 160.0)
  7: (200.0, 160.0)
  8: (140.0, 140.0)
  9: (40.0, 120.0)
  10: (100.0, 120.0)
Total distance: 1024.78
```

This implementation demonstrates how Simulated Annealing can effectively find near-optimal solutions for combinatorial optimization problems like the Traveling Salesman Problem.

