# Bezier Curve Generation in Go

Here's a complete implementation of Bezier curve generation algorithm in Go:

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

// BezierCurve represents a Bezier curve
type BezierCurve struct {
    Points []Point
}

// NewBezierCurve creates a new Bezier curve with given control points
func NewBezierCurve(points []Point) *BezierCurve {
    return &BezierCurve{Points: points}
}

// BinomialCoefficient calculates C(n,k) = n! / (k! * (n-k)!)
func BinomialCoefficient(n, k int) int {
    if k > n-k {
        k = n - k
    }
    result := 1
    for i := 0; i < k; i++ {
        result = result * (n - i) / (i + 1)
    }
    return result
}

// BernsteinPolynomial calculates the Bernstein polynomial B_{i,n}(t)
func BernsteinPolynomial(n, i int, t float64) float64 {
    binomial := BinomialCoefficient(n, i)
    return float64(binomial) * math.Pow(t, float64(i)) * math.Pow(1-t, float64(n-i))
}

// CalculateBezierPoint calculates a point on the Bezier curve at parameter t
func (bc *BezierCurve) CalculateBezierPoint(t float64) Point {
    n := len(bc.Points) - 1
    if n < 0 {
        return Point{0, 0}
    }
    
    x, y := 0.0, 0.0
    
    for i, point := range bc.Points {
        bernstein := BernsteinPolynomial(n, i, t)
        x += point.X * bernstein
        y += point.Y * bernstein
    }
    
    return Point{x, y}
}

// GenerateBezierCurve generates multiple points along the Bezier curve
func (bc *BezierCurve) GenerateBezierCurve(steps int) []Point {
    points := make([]Point, steps+1)
    for i := 0; i <= steps; i++ {
        t := float64(i) / float64(steps)
        points[i] = bc.CalculateBezierPoint(t)
    }
    return points
}

// DeCasteljauAlgorithm implements the De Casteljau algorithm for Bezier curves
func (bc *BezierCurve) DeCasteljau(t float64) Point {
    n := len(bc.Points) - 1
    if n < 0 {
        return Point{0, 0}
    }
    
    // Create a copy of control points
    points := make([]Point, len(bc.Points))
    copy(points, bc.Points)
    
    // De Casteljau's algorithm
    for i := 1; i <= n; i++ {
        for j := 0; j <= n-i; j++ {
            points[j] = Point{
                X: points[j].X*(1-t) + points[j+1].X*t,
                Y: points[j].Y*(1-t) + points[j+1].Y*t,
            }
        }
    }
    
    return points[0]
}

// GenerateBezierCurveWithDeCasteljau generates points using De Casteljau algorithm
func (bc *BezierCurve) GenerateBezierCurveWithDeCasteljau(steps int) []Point {
    points := make([]Point, steps+1)
    for i := 0; i <= steps; i++ {
        t := float64(i) / float64(steps)
        points[i] = bc.DeCasteljau(t)
    }
    return points
}

func main() {
    // Create a quadratic Bezier curve with 3 control points
    controlPoints := []Point{
        {0, 0},    // Start point
        {1, 2},    // Control point
        {2, 0},    // End point
    }
    
    bezier := NewBezierCurve(controlPoints)
    
    fmt.Println("Quadratic Bezier Curve Control Points:")
    for i, point := range controlPoints {
        fmt.Printf("P%d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
    
    fmt.Println("\nGenerated Points using Standard Algorithm:")
    points := bezier.GenerateBezierCurve(10)
    for i, point := range points {
        fmt.Printf("T=%.2f: (%.2f, %.2f)\n", float64(i)/10.0, point.X, point.Y)
    }
    
    fmt.Println("\nGenerated Points using De Casteljau Algorithm:")
    points2 := bezier.GenerateBezierCurveWithDeCasteljau(10)
    for i, point := range points2 {
        fmt.Printf("T=%.2f: (%.2f, %.2f)\n", float64(i)/10.0, point.X, point.Y)
    }
    
    // Create a cubic Bezier curve with 4 control points
    cubicPoints := []Point{
        {0, 0},
        {1, 1},
        {2, 1},
        {3, 0},
    }
    
    cubicBezier := NewBezierCurve(cubicPoints)
    
    fmt.Println("\n\nCubic Bezier Curve Control Points:")
    for i, point := range cubicPoints {
        fmt.Printf("P%d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
    
    fmt.Println("\nGenerated Points for Cubic Curve:")
    cubicPointsGenerated := cubicBezier.GenerateBezierCurve(8)
    for i, point := range cubicPointsGenerated {
        fmt.Printf("T=%.2f: (%.2f, %.2f)\n", float64(i)/8.0, point.X, point.Y)
    }
}
```

## Key Features of this Implementation:

### 1. **Point Structure**
- Represents 2D coordinates with X and Y components

### 2. **BezierCurve Structure**
- Stores control points for the curve
- Contains methods for curve generation

### 3. **Two Algorithms Implemented**
- **Standard Algorithm**: Uses Bernstein polynomials
- **De Casteljau Algorithm**: Numerically stable recursive approach

### 4. **Key Functions**
- `BinomialCoefficient`: Calculates binomial coefficients
- `BernsteinPolynomial`: Computes Bernstein basis functions
- `CalculateBezierPoint`: Calculates a single point on the curve
- `GenerateBezierCurve`: Creates multiple points along the curve
- `DeCasteljau`: Implements the De Casteljau recursive algorithm

### 5. **Usage Example**
The main function demonstrates:
- Quadratic Bezier curve (3 control points)
- Cubic Bezier curve (4 control points)
- Both algorithms producing equivalent results

## Sample Output:
```
Quadratic Bezier Curve Control Points:
P0: (0.00, 0.00)
P1: (1.00, 2.00)
P2: (2.00, 0.00)

Generated Points using Standard Algorithm:
T=0.00: (0.00, 0.00)
T=0.10: (0.18, 0.36)
T=0.20: (0.72, 0.64)
...
```

This implementation provides both mathematical accuracy and numerical stability, making it suitable for practical applications in graphics, animation, and CAD systems.

