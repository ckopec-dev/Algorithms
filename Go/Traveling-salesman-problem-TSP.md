# Traveling Salesman Problem (TSP) in Go

Here's a complete implementation of a TSP solver using a brute force approach with permutation generation:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "sort"
    "time"
)

// Point represents a city with x, y coordinates
type Point struct {
    X, Y float64
}

// Distance calculates the Euclidean distance between two points
func (p Point) Distance(other Point) float64 {
    dx := p.X - other.X
    dy := p.Y - other.Y
    return math.Sqrt(dx*dx + dy*dy)
}

// TSPSolver represents a TSP solver
type TSPSolver struct {
    cities []Point
}

// NewTSPSolver creates a new TSP solver with given cities
func NewTSPSolver(cities []Point) *TSPSolver {
    return &TSPSolver{cities: cities}
}

// CalculateTotalDistance calculates the total distance of a given route
func (tsp *TSPSolver) CalculateTotalDistance(route []int) float64 {
    if len(route) < 2 {
        return 0
    }
    
    total := 0.0
    for i := 0; i < len(route)-1; i++ {
        total += tsp.cities[route[i]].Distance(tsp.cities[route[i+1]])
    }
    // Return to starting city
    total += tsp.cities[route[len(route)-1]].Distance(tsp.cities[route[0]])
    return total
}

// GeneratePermutations generates all permutations of city indices
func (tsp *TSPSolver) GeneratePermutations(n int) [][]int {
    if n <= 1 {
        return [][]int{{0}}
    }
    
    // Generate permutations for n-1 cities
    smallerPerms := tsp.GeneratePermutations(n - 1)
    var result [][]int
    
    for _, perm := range smallerPerms {
        // Insert the nth city at all possible positions
        for i := 0; i <= len(perm); i++ {
            newPerm := make([]int, len(perm)+1)
            copy(newPerm[:i], perm[:i])
            newPerm[i] = n - 1
            copy(newPerm[i+1:], perm[i:])
            result = append(result, newPerm)
        }
    }
    
    return result
}

// BruteForceSolve finds the optimal solution using brute force
func (tsp *TSPSolver) BruteForceSolve() ([]int, float64) {
    n := len(tsp.cities)
    if n <= 1 {
        return []int{}, 0
    }
    
    // Generate all permutations of cities (excluding first city since it's fixed)
    permutations := tsp.GeneratePermutations(n)
    
    bestRoute := []int{}
    bestDistance := math.MaxFloat64
    
    for _, perm := range permutations {
        // Create route starting with city 0
        route := append([]int{0}, perm...)
        distance := tsp.CalculateTotalDistance(route)
        
        if distance < bestDistance {
            bestDistance = distance
            bestRoute = append([]int(nil), route...)
        }
    }
    
    return bestRoute, bestDistance
}

// NearestNeighborHeuristic provides a quick approximation
func (tsp *TSPSolver) NearestNeighborHeuristic() ([]int, float64) {
    n := len(tsp.cities)
    if n <= 1 {
        return []int{}, 0
    }
    
    unvisited := make([]bool, n)
    route := []int{0} // Start from city 0
    unvisited[0] = true
    
    current := 0
    totalDistance := 0.0
    
    for len(route) < n {
        nearest := -1
        nearestDistance := math.MaxFloat64
        
        for i := 0; i < n; i++ {
            if !unvisited[i] {
                distance := tsp.cities[current].Distance(tsp.cities[i])
                if distance < nearestDistance {
                    nearestDistance = distance
                    nearest = i
                }
            }
        }
        
        route = append(route, nearest)
        unvisited[nearest] = true
        totalDistance += nearestDistance
        current = nearest
    }
    
    // Return to starting city
    totalDistance += tsp.cities[current].Distance(tsp.cities[0])
    
    return route, totalDistance
}

// GenerateRandomCities creates n random cities
func GenerateRandomCities(n int) []Point {
    rand.Seed(time.Now().UnixNano())
    cities := make([]Point, n)
    for i := 0; i < n; i++ {
        cities[i] = Point{
            X: rand.Float64() * 100,
            Y: rand.Float64() * 100,
        }
    }
    return cities
}

func main() {
    // Create sample cities
    cities := []Point{
        {X: 0, Y: 0},
        {X: 1, Y: 2},
        {X: 3, Y: 1},
        {X: 5, Y: 3},
        {X: 2, Y: 4},
    }
    
    fmt.Println("Cities:")
    for i, city := range cities {
        fmt.Printf("City %d: (%.2f, %.2f)\n", i, city.X, city.Y)
    }
    
    solver := NewTSPSolver(cities)
    
    fmt.Println("\n--- Brute Force Solution ---")
    route, distance := solver.BruteForceSolve()
    fmt.Printf("Optimal route: %v\n", route)
    fmt.Printf("Total distance: %.2f\n", distance)
    
    fmt.Println("\n--- Nearest Neighbor Heuristic ---")
    heuristicRoute, heuristicDistance := solver.NearestNeighborHeuristic()
    fmt.Printf("Heuristic route: %v\n", heuristicRoute)
    fmt.Printf("Total distance: %.2f\n", heuristicDistance)
    
    // Example with more cities
    fmt.Println("\n--- Larger Example (5 cities) ---")
    largeCities := GenerateRandomCities(5)
    largeSolver := NewTSPSolver(largeCities)
    
    fmt.Println("Generated cities:")
    for i, city := range largeCities {
        fmt.Printf("City %d: (%.2f, %.2f)\n", i, city.X, city.Y)
    }
    
    route2, distance2 := largeSolver.BruteForceSolve()
    fmt.Printf("\nOptimal route: %v\n", route2)
    fmt.Printf("Total distance: %.2f\n", distance2)
}
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `Point` struct to represent cities with X,Y coordinates
- `TSPSolver` struct to encapsulate the solver logic

### 2. **Core Functions**
- `Distance()`: Calculates Euclidean distance between two points
- `CalculateTotalDistance()`: Computes total route distance
- `GeneratePermutations()`: Generates all possible city orderings
- `BruteForceSolve()`: Finds optimal solution using exhaustive search
- `NearestNeighborHeuristic()`: Quick approximation algorithm

### 3. **Algorithms Used**
- **Brute Force**: Tries all possible permutations (O(n!) complexity)
- **Nearest Neighbor**: Greedy heuristic (O(n²) complexity)

### 4. **Usage Example**
The code demonstrates both small and larger examples, showing:
- Optimal solutions for small instances
- Approximate solutions for larger instances
- Performance comparison between methods

## Important Notes:

1. **Brute Force Limitations**: Only practical for small instances (n ≤ 10) due to factorial time complexity
2. **Heuristic Value**: The nearest neighbor provides quick but not optimal solutions
3. **Scalability**: For larger problems, consider more advanced algorithms like genetic algorithms, simulated annealing, or dynamic programming approaches

This implementation provides a solid foundation for understanding TSP solving in Go and can be extended with more sophisticated optimization techniques.

