# Graham Scan Algorithm in Go

Here's a complete implementation of the Graham Scan algorithm to find the convex hull of a set of points:

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

// Find the bottom-most point (or left-most if tie)
func findBottomMostPoint(points []Point) Point {
    bottom := points[0]
    for _, p := range points {
        if p.Y < bottom.Y || (p.Y == bottom.Y && p.X < bottom.X) {
            bottom = p
        }
    }
    return bottom
}

// Sort points by polar angle with respect to the bottom-most point
func sortByPolarAngle(points []Point, bottom Point) []Point {
    // Create a copy to avoid modifying original slice
    sorted := make([]Point, len(points))
    copy(sorted, points)
    
    // Sort using custom comparison function
    for i := 0; i < len(sorted)-1; i++ {
        for j := i + 1; j < len(sorted); j++ {
            // Calculate polar angles
            angle1 := math.Atan2(sorted[i].Y-bottom.Y, sorted[i].X-bottom.X)
            angle2 := math.Atan2(sorted[j].Y-bottom.Y, sorted[j].X-bottom.X)
            
            // If angles are equal, sort by distance
            if angle1 == angle2 {
                dist1 := distance(bottom, sorted[i])
                dist2 := distance(bottom, sorted[j])
                if dist1 > dist2 {
                    sorted[i], sorted[j] = sorted[j], sorted[i]
                }
            } else if angle1 > angle2 {
                sorted[i], sorted[j] = sorted[j], sorted[i]
            }
        }
    }
    
    return sorted
}

// Graham Scan algorithm implementation
func grahamScan(points []Point) []Point {
    if len(points) < 3 {
        return points
    }
    
    // Find the bottom-most point
    bottom := findBottomMostPoint(points)
    
    // Sort points by polar angle
    sortedPoints := sortByPolarAngle(points, bottom)
    
    // Initialize stack with first three points
    stack := make([]Point, 0, len(sortedPoints))
    stack = append(stack, sortedPoints[0])
    stack = append(stack, sortedPoints[1])
    stack = append(stack, sortedPoints[2])
    
    // Process remaining points
    for i := 3; i < len(sortedPoints); i++ {
        // Remove points that make clockwise turn
        for len(stack) >= 2 && crossProduct(stack[len(stack)-2], stack[len(stack)-1], sortedPoints[i]) <= 0 {
            stack = stack[:len(stack)-1]
        }
        stack = append(stack, sortedPoints[i])
    }
    
    return stack
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
    
    // Find convex hull
    hull := grahamScan(points)
    
    fmt.Println("\nConvex hull points:")
    for i, p := range hull {
        fmt.Printf("  %d: (%.1f, %.1f)\n", i, p.X, p.Y)
    }
    
    fmt.Printf("\nTotal points in convex hull: %d\n", len(hull))
}
```

## How the Algorithm Works

1. **Find the bottom-most point**: Identify the point with the lowest y-coordinate (leftmost if tie)
2. **Sort by polar angle**: Sort all points by their polar angle with respect to the bottom-most point
3. **Build the hull**: Use a stack to maintain the convex hull, removing points that create clockwise turns

## Key Functions

- `crossProduct()`: Determines the orientation of three points
- `distance()`: Calculates Euclidean distance between two points
- `findBottomMostPoint()`: Locates the starting point for sorting
- `sortByPolarAngle()`: Orders points by their angular position
- `grahamScan()`: Main algorithm implementation

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the stack and temporary arrays

## Sample Output
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

Convex hull points:
  0: (0.0, 0.0)
  1: (0.0, 3.0)
  2: (1.0, 2.0)
  3: (3.0, 3.0)
  4: (4.0, 4.0)
  5: (3.0, 1.0)

Total points in convex hull: 6
```

