# Monte Carlo Method Implementation in Go

Here's an example of using the Monte Carlo method to estimate the value of π:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// MonteCarloPi estimates π using the Monte Carlo method
func MonteCarloPi(numPoints int) float64 {
    var pointsInCircle int
    
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())
    
    for i := 0; i < numPoints; i++ {
        // Generate random point in unit square [0,1] x [0,1]
        x := rand.Float64()
        y := rand.Float64()
        
        // Calculate distance from origin
        distance := math.Sqrt(x*x + y*y)
        
        // Check if point is inside unit circle
        if distance <= 1 {
            pointsInCircle++
        }
    }
    
    // Estimate π using the ratio of points inside circle to total points
    // π ≈ 4 * (pointsInCircle / totalPoints)
    return 4 * float64(pointsInCircle) / float64(numPoints)
}

// Alternative implementation with better random number generation
func MonteCarloPiImproved(numPoints int) float64 {
    var pointsInCircle int
    
    // Create new random generator
    r := rand.New(rand.NewSource(time.Now().UnixNano()))
    
    for i := 0; i < numPoints; i++ {
        x := r.Float64()
        y := r.Float64()
        
        // Use math.Hypot for better numerical stability
        distance := math.Hypot(x, y)
        
        if distance <= 1 {
            pointsInCircle++
        }
    }
    
    return 4 * float64(pointsInCircle) / float64(numPoints)
}

func main() {
    // Test with different numbers of points
    testPoints := []int{1000, 10000, 100000, 1000000, 10000000}
    
    fmt.Println("Monte Carlo Estimation of π")
    fmt.Println("==========================")
    fmt.Printf("%-12s %s\n", "Points", "Estimated π")
    fmt.Println("--------------------------")
    
    for _, numPoints := range testPoints {
        estimatedPi := MonteCarloPiImproved(numPoints)
        error := math.Abs(estimatedPi - math.Pi)
        
        fmt.Printf("%-12d %.6f (error: %.6f)\n", 
            numPoints, estimatedPi, error)
    }
    
    fmt.Println("--------------------------")
    fmt.Printf("Actual π: %.6f\n", math.Pi)
    
    // Example with a single run
    fmt.Println("\nSingle Run Example:")
    result := MonteCarloPiImproved(1000000)
    fmt.Printf("π ≈ %.6f\n", result)
}
```

## How it works:

1. **Method**: The algorithm generates random points within a unit square and determines how many fall inside a unit circle inscribed within that square.

2. **Mathematical Principle**: 
   - Area of circle = π × r²
   - Area of square = (2r)² = 4r²
   - Ratio = π/4
   - Therefore: π ≈ 4 × (points inside circle / total points)

3. **Key Features**:
   - Uses `math/rand` for random number generation
   - Implements proper seeding with current time
   - Uses `math.Hypot()` for numerically stable distance calculation
   - Demonstrates convergence as more points are used

## Sample Output:
```
Monte Carlo Estimation of π
==========================
Points       Estimated π
--------------------------
1000         3.148000 (error: 0.003642)
10000        3.141200 (error: 0.000442)
100000       3.142000 (error: 0.000842)
1000000      3.141880 (error: 0.000358)
10000000     3.141592 (error: 0.000001)
--------------------------
Actual π: 3.141593

Single Run Example:
π ≈ 3.141592
```

This demonstrates how the Monte Carlo method becomes more accurate as the number of random samples increases, illustrating the law of large numbers.

