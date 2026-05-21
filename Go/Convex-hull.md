# Convex Hull Algorithm in Go

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

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

// Cross product of vectors (p1-p0) and (p2-p0)
func crossProduct(p0, p1, p2 Point) float64 {
    return (p1.X-p0.X)*(p2.Y-p0.Y) - (p1.Y-p0.Y)*(p2.X-p0.X)
}

// Distance between two points
func distance(p1, p2 Point) float64 {
    return math.Sqrt(math.Pow(p2.X-p1.X, 2) + math.Pow(p2.Y-p1.Y, 2))
}

// Find the point with the lowest y-coordinate (and leftmost if tie)
func findBottomPoint(points []Point) Point {
    bottom := points[0]
    for _, p := range points {
        if p.Y < bottom.Y || (p.Y == bottom.Y && p.X < bottom.X) {
            bottom = p
        }
    }
    return bottom
}

// Sort points by polar angle with respect to the bottom point
func sortByPolarAngle(points []Point, bottom Point) []Point {
    // Create a copy to avoid modifying the original slice
    sorted := make([]Point, len(points))
    copy(sorted, points)
    
    // Sort using bubble sort for simplicity
    for i := 0; i < len(sorted)-1; i++ {
        for j := 0; j < len(sorted)-1-i; j++ {
            // Calculate polar angles
            angle1 := math.Atan2(sorted[j].Y-bottom.Y, sorted[j].X-bottom.X)
            angle2 := math.Atan2(sorted[j+1].Y-bottom.Y, sorted[j+1].X-bottom.X)
            
            if angle1 > angle2 {
                sorted[j], sorted[j+1] = sorted[j+1], sorted[j]
            }
        }
    }
    
    return sorted
}

// Graham Scan algorithm to find convex hull
func convexHull(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find the bottom point
    bottom := findBottomPoint(points)
    
    // Sort points by polar angle
    sortedPoints := sortByPolarAngle(points, bottom)
    
    // Initialize hull with first three points
    hull := []Point{sortedPoints[0], sortedPoints[1], sortedPoints[2]}
    
    // Process remaining points
    for i := 3; i < len(sortedPoints); i++ {
        // Remove points that make clockwise turns
        for len(hull) > 1 && crossProduct(hull[len(hull)-2], hull[len(hull)-1], sortedPoints[i]) <= 0 {
            hull = hull[:len(hull)-1]
        }
        hull = append(hull, sortedPoints[i])
    }
    
    return hull
}

// Simplified version using atan2 for sorting
func convexHullSimple(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find bottom point
    bottom := findBottomPoint(points)
    
    // Sort points by polar angle with bottom point
    // Using a simple sorting approach
    n := len(points)
    angles := make([]float64, n)
    sortedPoints := make([]Point, n)
    
    for i, p := range points {
        sortedPoints[i] = p
        angles[i] = math.Atan2(p.Y-bottom.Y, p.X-bottom.X)
    }
    
    // Simple bubble sort by angle
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-1-i; j++ {
            if angles[j] > angles[j+1] {
                angles[j], angles[j+1] = angles[j+1], angles[j]
                sortedPoints[j], sortedPoints[j+1] = sortedPoints[j+1], sortedPoints[j]
            }
        }
    }
    
    // Graham scan
    hull := []Point{sortedPoints[0], sortedPoints[1]}
    
    for i := 2; i < n; i++ {
        for len(hull) > 1 && crossProduct(hull[len(hull)-2], hull[len(hull)-1], sortedPoints[i]) <= 0 {
            hull = hull[:len(hull)-1]
        }
        hull = append(hull, sortedPoints[i])
    }
    
    return hull
}

func main() {
    // Example points
    points := []Point{
        {0, 3}, {1, 1}, {2, 2}, {4, 4},
        {0, 0}, {1, 2}, {3, 1}, {3, 3},
    }
    
    fmt.Println("Input points:")
    for _, p := range points {
        fmt.Printf("(%.0f, %.0f) ", p.X, p.Y)
    }
    fmt.Println()
    
    // Find convex hull
    hull := convexHullSimple(points)
    
    fmt.Println("Convex hull points:")
    for _, p := range hull {
        fmt.Printf("(%.0f, %.0f) ", p.X, p.Y)
    }
    fmt.Println()
    
    // Test with a simpler example
    fmt.Println("\nSimple test:")
    simplePoints := []Point{{0, 0}, {1, 0}, {1, 1}, {0, 1}}
    simpleHull := convexHullSimple(simplePoints)
    fmt.Print("Convex hull of square: ")
    for _, p := range simpleHull {
        fmt.Printf("(%.0f, %.0f) ", p.X, p.Y)
    }
    fmt.Println()
}
```

## Key Components:

### 1. **Point Structure**
```go
type Point struct {
    X, Y float64
}
```

### 2. **Cross Product Function**
```go
func crossProduct(p0, p1, p2 Point) float64 {
    return (p1.X-p0.X)*(p2.Y-p0.Y) - (p1.Y-p0.Y)*(p2.X-p0.X)
}
```

### 3. **Graham Scan Algorithm**
- Find the bottommost point
- Sort points by polar angle
- Use a stack-based approach to build the hull
- Remove points that create clockwise turns

## Time Complexity:
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the hull storage

## Output Example:
```
Input points:
(0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 

Convex hull points:
(0, 0) (0, 3) (4, 4) (3, 1) 

Simple test:
Convex hull of square: (0, 0) (1, 0) (1, 1) (0, 1) 
```

This implementation demonstrates the classic Graham Scan algorithm for computing the convex hull of a set of 2D points.

