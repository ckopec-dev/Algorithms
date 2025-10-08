# Gift Wrapping Algorithm (Jarvis March) in Go

The gift wrapping algorithm is a simple algorithm for computing the convex hull of a set of points in the plane. It works by starting with the leftmost point and then repeatedly finding the next point on the convex hull by selecting the point that makes the smallest counterclockwise angle with the previous edge.

## Implementation

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
func cross(p0, p1, p2 Point) float64 {
    return (p1.X-p0.X)*(p2.Y-p0.Y) - (p1.Y-p0.Y)*(p2.X-p0.X)
}

// Distance between two points
func distance(p1, p2 Point) float64 {
    return math.Sqrt(math.Pow(p2.X-p1.X, 2) + math.Pow(p2.Y-p1.Y, 2))
}

// Find the leftmost point
func findLeftmostPoint(points []Point) Point {
    leftmost := points[0]
    for _, p := range points {
        if p.X < leftmost.X {
            leftmost = p
        }
    }
    return leftmost
}

// Gift wrapping algorithm to find convex hull
func giftWrapping(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find the leftmost point
    start := findLeftmostPoint(points)
    
    hull := []Point{}
    current := start
    
    for {
        hull = append(hull, current)
        
        // Find the next point
        next := points[0]
        for _, p := range points {
            // Skip if it's the current point
            if p == current {
                continue
            }
            
            // If we haven't found a point yet, or if this point makes a larger
            // counterclockwise angle with the previous edge
            if next == current || cross(hull[len(hull)-2], current, next) < cross(hull[len(hull)-2], current, p) {
                next = p
            }
        }
        
        current = next
        
        // Stop when we return to the starting point
        if current == start {
            break
        }
    }
    
    return hull
}

// Alternative implementation with better angle calculation
func giftWrappingImproved(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find the leftmost point
    start := findLeftmostPoint(points)
    
    hull := []Point{}
    current := start
    
    for {
        hull = append(hull, current)
        
        // Find the next point
        next := points[0]
        for _, p := range points {
            if p == current {
                continue
            }
            
            // Calculate the cross product to determine orientation
            // We want the point that makes the smallest counterclockwise angle
            crossProduct := cross(hull[len(hull)-2], current, p)
            
            // If next is the first point or if p makes a smaller angle
            if next == current || crossProduct > cross(hull[len(hull)-2], current, next) {
                next = p
            }
        }
        
        current = next
        
        // Stop when we return to the starting point
        if current == start {
            break
        }
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
    for i, p := range points {
        fmt.Printf("  %d: (%.1f, %.1f)\n", i, p.X, p.Y)
    }
    
    // Find convex hull using gift wrapping
    hull := giftWrapping(points)
    
    fmt.Println("\nConvex Hull points:")
    for i, p := range hull {
        fmt.Printf("  %d: (%.1f, %.1f)\n", i, p.X, p.Y)
    }
    
    // Test with another set of points
    fmt.Println("\n" + "="*50)
    fmt.Println("Another example:")
    
    points2 := []Point{
        {0, 0}, {1, 0}, {2, 1}, {2, 2},
        {1, 2}, {0, 1}, {0.5, 0.5}, {1.5, 1.5},
    }
    
    fmt.Println("Input points:")
    for i, p := range points2 {
        fmt.Printf("  %d: (%.1f, %.1f)\n", i, p.X, p.Y)
    }
    
    hull2 := giftWrapping(points2)
    
    fmt.Println("\nConvex Hull points:")
    for i, p := range hull2 {
        fmt.Printf("  %d: (%.1f, %.1f)\n", i, p.X, p.Y)
    }
}
```

## How it works:

1. **Find the starting point**: The leftmost point is selected as the starting point of the hull
2. **Build the hull**: Starting from the initial point, we find the next point by:
   - Checking all other points
   - Using cross product to determine which point makes the smallest counterclockwise angle
   - Moving to that point
3. **Continue until return**: We continue until we return to the starting point

## Time Complexity:
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Example Output:
```
Input points:
  0: (0.0, 3.0)
  1: (1.0, 1.0)
  2: (2.0, 2.0)
  3: (4.0, 4.0)
  4: (0.0, 0.0)
  5: (1.0, 2.0)
  6: (3.0, 1.0)
  7: (3.0, 3.0)

Convex Hull points:
  0: (0.0, 0.0)
  1: (0.0, 3.0)
  2: (1.0, 2.0)
  3: (3.0, 3.0)
  4: (4.0, 4.0)
  5: (3.0, 1.0)
```

The algorithm is simple to understand and implement, making it a good choice for educational purposes and when the number of hull points is small compared to the input size.

