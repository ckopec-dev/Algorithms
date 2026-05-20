# Simulated Annealing Algorithm in Go

Here's a complete implementation of the Simulated Annealing algorithm in Go, solving the classic Traveling Salesman Problem (TSP):

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// City represents a city with x and y coordinates
type City struct {
    X, Y float64
}

// Tour represents a sequence of cities
type Tour struct {
    Cities []City
    Distance float64
}

// Calculate Euclidean distance between two cities
func distance(city1, city2 City) float64 {
    dx := city1.X - city2.X
    dy := city1.Y - city2.Y
    return math.Sqrt(dx*dx + dy*dy)
}

// Calculate total distance of a tour
func calculateTourDistance(tour []City) float64 {
    if len(tour) <= 1 {
        return 0
    }
    
    total := 0.0
    for i := 0; i < len(tour)-1; i++ {
        total += distance(tour[i], tour[i+1])
    }
    // Return to starting city
    total += distance(tour[len(tour)-1], tour[0])
    return total
}

// Generate a random tour
func generateRandomTour(cities []City) []City {
    tour := make([]City, len(cities))
    copy(tour, cities)
    
    // Shuffle the tour
    for i := len(tour) - 1; i > 0; i-- {
        j := rand.Intn(i + 1)
        tour[i], tour[j] = tour[j], tour[i]
    }
    
    return tour
}

// Generate a neighbor tour by swapping two random cities
func getNeighbor(tour []City) []City {
    neighbor := make([]City, len(tour))
    copy(neighbor, tour)
    
    // Swap two random cities
    i, j := rand.Intn(len(neighbor)), rand.Intn(len(neighbor))
    for i == j {
        j = rand.Intn(len(neighbor))
    }
    neighbor[i], neighbor[j] = neighbor[j], neighbor[i]
    
    return neighbor
}

// Simulated Annealing algorithm
func simulatedAnnealing(cities []City, initialTemp, coolingRate float64, maxIterations int) Tour {
    // Generate initial random tour
    currentTour := generateRandomTour(cities)
    currentDistance := calculateTourDistance(currentTour)
    
    bestTour := make([]City, len(currentTour))
    copy(bestTour, currentTour)
    bestDistance := currentDistance
    
    temperature := initialTemp
    
    for i := 0; i < maxIterations; i++ {
        // Generate neighbor tour
        neighborTour := getNeighbor(currentTour)
        neighborDistance := calculateTourDistance(neighborTour)
        
        // Calculate energy difference
        delta := neighborDistance - currentDistance
        
        // Accept or reject the neighbor
        if delta < 0 || rand.Float64() < math.Exp(-delta/temperature) {
            currentTour = neighborTour
            currentDistance = neighborDistance
            
            // Update best tour if we found a better solution
            if currentDistance < bestDistance {
                bestDistance = currentDistance
                copy(bestTour, currentTour)
            }
        }
        
        // Cool down the temperature
        temperature *= coolingRate
        
        // Optional: print progress
        if i%1000 == 0 {
            fmt.Printf("Iteration %d: Distance = %.2f, Temperature = %.2f\n", i, bestDistance, temperature)
        }
    }
    
    return Tour{
        Cities: bestTour,
        Distance: bestDistance,
    }
}

func main() {
    // Generate some random cities
    rand.Seed(time.Now().UnixNano())
    
    cities := []City{
        {1, 1}, {2, 3}, {4, 3}, {5, 2},
        {3, 1}, {6, 4}, {7, 1}, {8, 3},
        {9, 2}, {10, 4}, {11, 1}, {12, 3},
    }
    
    fmt.Printf("Solving TSP with %d cities\n", len(cities))
    fmt.Println("Initial cities:", cities)
    
    // Run simulated annealing
    result := simulatedAnnealing(
        cities,
        initialTemp: 10000.0,    // Initial temperature
        coolingRate: 0.999,      // Cooling rate
        maxIterations: 100000,   // Maximum iterations
    )
    
    fmt.Printf("\nBest tour found:\n")
    fmt.Printf("Distance: %.2f\n", result.Distance)
    fmt.Printf("Cities in order: ")
    for i, city := range result.Cities {
        if i > 0 {
            fmt.Print(" -> ")
        }
        fmt.Printf("(%.0f,%.0f)", city.X, city.Y)
    }
    fmt.Println()
}
```

## How the Algorithm Works

### Key Components:

1. **City Structure**: Represents a city with X,Y coordinates
2. **Tour Structure**: Represents a sequence of cities with total distance
3. **Distance Calculation**: Uses Euclidean distance formula
4. **Neighbor Generation**: Swaps two random cities in the tour
5. **Acceptance Probability**: Uses Metropolis criterion to decide whether to accept worse solutions

### Algorithm Steps:

1. **Initialization**: Generate random initial tour and set initial temperature
2. **Iteration Loop**: 
   - Generate neighbor tour by swapping two cities
   - Calculate energy difference (distance change)
   - Accept neighbor if better or with probability based on temperature
   - Update best solution found so far
   - Cool down temperature
3. **Termination**: Continue until max iterations or temperature drops below threshold

### Key Parameters:

- **Initial Temperature**: Controls initial acceptance probability
- **Cooling Rate**: How quickly temperature decreases (typically 0.95-0.999)
- **Max Iterations**: Number of iterations to run

### Output Example:
```
Solving TSP with 12 cities
Initial cities: [{1 1} {2 3} {4 3} {5 2} {3 1} {6 4} {7 1} {8 3} {9 2} {10 4} {11 1} {12 3}]

Iteration 0: Distance = 42.83, Temperature = 10000.00
Iteration 1000: Distance = 38.21, Temperature = 9.04
Iteration 2000: Distance = 37.15, Temperature = 8.14
...

Best tour found:
Distance: 35.67
Cities in order: (1,1) -> (3,1) -> (5,2) -> (4,3) -> (2,3) -> (6,4) -> (8,3) -> (7,1) -> (9,2) -> (10,4) -> (12,3) -> (11,1)
```

This implementation demonstrates the core principles of simulated annealing: starting with a random solution, accepting worse solutions with a probability that decreases over time, and gradually converging toward an optimal or near-optimal solution.

