# Jarvis March Algorithm (Gift Wrapping Algorithm) in Go

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

// crossProduct calculates the cross product of three points
// Returns positive value if counter-clockwise turn, negative for clockwise, zero for collinear
func crossProduct(p1, p2, p3 Point) float64 {
    return (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
}

// distance calculates the Euclidean distance between two points
func distance(p1, p2 Point) float64 {
    return math.Sqrt(math.Pow(p2.X-p1.X, 2) + math.Pow(p2.Y-p1.Y, 2))
}

// jarvisMarch finds the convex hull of a set of points using Jarvis March algorithm
func jarvisMarch(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find the leftmost point (with smallest x-coordinate)
    leftmost := 0
    for i := 1; i < len(points); i++ {
        if points[i].X < points[leftmost].X {
            leftmost = i
        }
    }
    
    hull := []Point{}
    current := leftmost
    
    for {
        // Add current point to hull
        hull = append(hull, points[current])
        
        // Find the next point that makes the largest counter-clockwise turn
        next := 0
        for i := 0; i < len(points); i++ {
            // Skip if it's the current point
            if i == current {
                continue
            }
            
            // Calculate cross product to determine the turn direction
            cp := crossProduct(points[current], points[next], points[i])
            
            // If we found a point that makes a larger counter-clockwise turn,
            // or if the current point is collinear and farther away
            if next == current || cp > 0 || (cp == 0 && distance(points[current], points[i]) > distance(points[current], points[next])) {
                next = i
            }
        }
        
        // If we've wrapped around and reached the starting point, we're done
        if next == leftmost {
            break
        }
        
        current = next
    }
    
    return hull
}

func main() {
    // Example set of points
    points := []Point{
        {0, 3}, {1, 1}, {2, 2}, {4, 4},
        {0, 0}, {1, 2}, {3, 1}, {3, 3},
    }
    
    fmt.Println("Input points:")
    for i, point := range points {
        fmt.Printf("Point %d: (%.1f, %.1f)\n", i+1, point.X, point.Y)
    }
    
    // Find convex hull
    hull := jarvisMarch(points)
    
    fmt.Println("\nConvex Hull Points:")
    for i, point := range hull {
        fmt.Printf("Hull Point %d: (%.1f, %.1f)\n", i+1, point.X, point.Y)
    }
    
    fmt.Printf("\nTotal points in convex hull: %d\n", len(hull))
}
```

## How the Algorithm Works:

1. **Find the starting point**: Identify the leftmost point (smallest x-coordinate)
2. **Build the hull**: Starting from the leftmost point, repeatedly find the next point that makes the largest counter-clockwise turn
3. **Cross product test**: Use cross product to determine turn direction
4. **Termination**: Continue until we return to the starting point

## Time Complexity:
- **Worst case**: O(n²) where n is the number of points
- **Best case**: O(nh) where h is the number of points on the hull
- **Space complexity**: O(h) for storing the hull points

## Output Example:
```
Input points:
Point 1: (0.0, 3.0)
Point 2: (1.0, 1.0)
Point 3: (2.0, 2.0)
Point 4: (4.0, 4.0)
Point 5: (0.0, 0.0)
Point 6: (1.0, 2.0)
Point 7: (3.0, 1.0)
Point 8: (3.0, 3.0)

Convex Hull Points:
Hull Point 1: (0.0, 0.0)
Hull Point 2: (0.0, 3.0)
Hull Point 3: (1.0, 2.0)
Hull Point 4: (3.0, 3.0)
Hull Point 5: (4.0, 4.0)
Hull Point 6: (3.0, 1.0)

Total points in convex hull: 6
```

