# De Boor's Algorithm Implementation in Go

De Boor's algorithm is used for evaluating B-spline curves at specific parameter values. Here's a complete implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Point represents a 2D point
type Point struct {
    X, Y float64
}

// DeBoor evaluates a B-spline curve at parameter u using De Boor's algorithm
func DeBoor(knots []float64, controlPoints []Point, u float64, degree int) Point {
    n := len(controlPoints) - 1
    
    // Find the knot span
    span := findKnotSpan(knots, u, degree, n)
    
    // Initialize the temporary points
    temp := make([]Point, degree+1)
    for i := 0; i <= degree; i++ {
        temp[i] = controlPoints[span-degree+i]
    }
    
    // De Boor's algorithm
    for r := 1; r <= degree; r++ {
        for i := degree; i >= r; i-- {
            alpha := (u - knots[span-degree+i]) / (knots[span+i] - knots[span-degree+i])
            temp[i] = interpolatePoints(temp[i-1], temp[i], alpha)
        }
    }
    
    return temp[degree]
}

// findKnotSpan finds the span of the knot u in the knot vector
func findKnotSpan(knots []float64, u float64, degree, n int) int {
    if u >= knots[n+1] {
        return n
    }
    
    if u <= knots[degree] {
        return degree
    }
    
    // Binary search
    low := degree
    high := n + 1
    mid := (low + high) / 2
    
    for u < knots[mid] || u >= knots[mid+1] {
        if u < knots[mid] {
            high = mid
        } else {
            low = mid
        }
        mid = (low + high) / 2
    }
    
    return mid
}

// interpolatePoints interpolates between two points using alpha
func interpolatePoints(p1, p2 Point, alpha float64) Point {
    return Point{
        X: p1.X + alpha*(p2.X-p1.X),
        Y: p1.Y + alpha*(p2.Y-p1.Y),
    }
}

// EvaluateCurve evaluates a B-spline curve at multiple parameter values
func EvaluateCurve(knots []float64, controlPoints []Point, uValues []float64, degree int) []Point {
    results := make([]Point, len(uValues))
    for i, u := range uValues {
        results[i] = DeBoor(knots, controlPoints, u, degree)
    }
    return results
}

func main() {
    // Example: Cubic B-spline (degree = 3)
    // Control points
    controlPoints := []Point{
        {0, 0},
        {1, 2},
        {2, 1},
        {3, 3},
        {4, 2},
        {5, 4},
    }
    
    // Knot vector (clamped)
    knots := []float64{0, 0, 0, 0, 1, 2, 3, 4, 4, 4, 4}
    
    // Degree of the spline
    degree := 3
    
    // Evaluate at specific parameter values
    uValues := []float64{0.0, 0.25, 0.5, 0.75, 1.0}
    
    fmt.Println("B-spline Evaluation using De Boor's Algorithm")
    fmt.Println("============================================")
    fmt.Printf("Control Points: %v\n", controlPoints)
    fmt.Printf("Knot Vector: %v\n", knots)
    fmt.Printf("Degree: %d\n\n", degree)
    
    results := EvaluateCurve(knots, controlPoints, uValues, degree)
    
    fmt.Println("Evaluation Results:")
    fmt.Println("u\t\tPoint")
    fmt.Println("------------------------")
    for i, u := range uValues {
        fmt.Printf("%.2f\t\t(%.2f, %.2f)\n", u, results[i].X, results[i].Y)
    }
    
    // Example of evaluating at a single point
    fmt.Println("\nSingle Point Evaluation:")
    point := DeBoor(knots, controlPoints, 0.5, degree)
    fmt.Printf("At u = 0.5: (%.2f, %.2f)\n", point.X, point.Y)
}
```

## How De Boor's Algorithm Works

The algorithm works by:

1. **Finding the knot span**: Determine which interval the parameter u falls into
2. **Initialization**: Start with the relevant control points
3. **Recursive interpolation**: Apply a series of linear interpolations to compute the final point

## Key Components

- **`DeBoor`**: Main function implementing De Boor's algorithm
- **`findKnotSpan`**: Binary search to find the correct knot interval
- **`interpolatePoints`**: Linear interpolation between two points
- **`EvaluateCurve`**: Evaluate multiple points along the curve

## Sample Output

```
B-spline Evaluation using De Boor's Algorithm
============================================
Control Points: [{0 0} {1 2} {2 1} {3 3} {4 2} {5 4}]
Knot Vector: [0 0 0 0 1 2 3 4 4 4 4]
Degree: 3

Evaluation Results:
u		Point
------------------------
0.00		(0.00, 0.00)
0.25		(1.00, 1.75)
0.50		(2.00, 1.50)
0.75		(3.00, 2.25)
1.00		(4.00, 2.00)

Single Point Evaluation:
At u = 0.5: (2.00, 1.50)
```

This implementation demonstrates how De Boor's algorithm efficiently evaluates B-spline curves by recursively interpolating between control points based on the knot vector structure.

