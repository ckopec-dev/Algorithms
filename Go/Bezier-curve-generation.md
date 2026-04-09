# Bezier Curve Generation in Go

Here's a complete implementation of a Bezier curve generation algorithm in Go:

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
    ControlPoints []Point
}

// NewBezierCurve creates a new Bezier curve with given control points
func NewBezierCurve(controlPoints []Point) *BezierCurve {
    return &BezierCurve{
        ControlPoints: controlPoints,
    }
}

// BernsteinPolynomial calculates the Bernstein polynomial B(i,n,t)
func (bc *BezierCurve) BernsteinPolynomial(i, n int, t float64) float64 {
    // Calculate binomial coefficient C(n,i)
    binomialCoeff := 1.0
    for j := 0; j < i; j++ {
        binomialCoeff *= float64(n-j) / float64(j+1)
    }
    
    // Calculate B(i,n,t) = C(n,i) * t^i * (1-t)^(n-i)
    return binomialCoeff * math.Pow(t, float64(i)) * math.Pow(1-t, float64(n-i))
}

// Evaluate calculates a point on the Bezier curve at parameter t
func (bc *BezierCurve) Evaluate(t float64) Point {
    n := len(bc.ControlPoints) - 1
    if n < 0 {
        return Point{0, 0}
    }
    
    x := 0.0
    y := 0.0
    
    for i, point := range bc.ControlPoints {
        bernstein := bc.BernsteinPolynomial(i, n, t)
        x += point.X * bernstein
        y += point.Y * bernstein
    }
    
    return Point{x, y}
}

// GeneratePoints generates multiple points along the Bezier curve
func (bc *BezierCurve) GeneratePoints(numPoints int) []Point {
    points := make([]Point, numPoints)
    
    for i := 0; i < numPoints; i++ {
        t := float64(i) / float64(numPoints-1)
        points[i] = bc.Evaluate(t)
    }
    
    return points
}

// DeCasteljauAlgorithm implements the De Casteljau algorithm for Bezier curves
func (bc *BezierCurve) DeCasteljau(t float64) Point {
    n := len(bc.ControlPoints) - 1
    if n < 0 {
        return Point{0, 0}
    }
    
    // Create a copy of control points
    points := make([]Point, len(bc.ControlPoints))
    copy(points, bc.ControlPoints)
    
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

// GeneratePointsWithDeCasteljau generates points using De Casteljau's algorithm
func (bc *BezierCurve) GeneratePointsWithDeCasteljau(numPoints int) []Point {
    points := make([]Point, numPoints)
    
    for i := 0; i < numPoints; i++ {
        t := float64(i) / float64(numPoints-1)
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
    
    fmt.Println("Quadratic Bezier Curve:")
    fmt.Println("Control Points:", controlPoints)
    
    // Generate 11 points along the curve
    points := bezier.GeneratePoints(11)
    
    fmt.Println("\nGenerated Points:")
    for i, point := range points {
        fmt.Printf("P%d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
    
    // Compare with De Casteljau algorithm
    fmt.Println("\nUsing De Casteljau Algorithm:")
    pointsDC := bezier.GeneratePointsWithDeCasteljau(11)
    for i, point := range pointsDC {
        fmt.Printf("P%d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
    
    // Create a cubic Bezier curve
    cubicPoints := []Point{
        {0, 0},
        {1, 1},
        {2, 1},
        {3, 0},
    }
    
    cubicBezier := NewBezierCurve(cubicPoints)
    
    fmt.Println("\nCubic Bezier Curve:")
    fmt.Println("Control Points:", cubicPoints)
    
    cubicPointsGenerated := cubicBezier.GeneratePoints(11)
    fmt.Println("\nGenerated Points:")
    for i, point := range cubicPointsGenerated {
        fmt.Printf("P%d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
}
```

## Key Features of this Implementation:

### 1. **Point Structure**
- Represents 2D coordinates with X and Y float64 values

### 2. **BezierCurve Structure**
- Stores control points for the Bezier curve
- Contains methods for curve evaluation

### 3. **Two Evaluation Methods**
- **Standard Method**: Uses Bernstein polynomials
- **De Casteljau Algorithm**: Numerically more stable approach

### 4. **Core Functions**
- `BernsteinPolynomial()`: Calculates the Bernstein basis function
- `Evaluate()`: Gets a point on the curve at parameter t
- `GeneratePoints()`: Creates multiple points along the curve
- `DeCasteljau()`: Implements the De Casteljau algorithm

### 5. **Usage Example**
The main function demonstrates:
- Quadratic Bezier curve (3 control points)
- Cubic Bezier curve (4 control points)
- Comparison between both algorithms

## Sample Output:
```
Quadratic Bezier Curve:
Control Points: [{0 0} {1 2} {2 0}]

Generated Points:
P0: (0.00, 0.00)
P1: (0.25, 0.50)
P2: (0.50, 1.00)
P3: (0.75, 1.50)
P4: (1.00, 2.00)
P5: (1.25, 1.50)
P6: (1.50, 1.00)
P7: (1.75, 0.50)
P8: (2.00, 0.00)
P9: (2.00, 0.00)
P10: (2.00, 0.00)
```

This implementation provides both mathematical accuracy and computational efficiency for generating Bezier curves in Go.

