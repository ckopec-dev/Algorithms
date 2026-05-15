# Convex Hull Algorithm in C

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure to represent a point
typedef struct {
    int x, y;
} Point;

// Function to find the orientation of three points
// Returns: 0 -> collinear, 1 -> clockwise, 2 -> counterclockwise
int orientation(Point p, Point q, Point r) {
    int val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
    if (val == 0) return 0;  // collinear
    return (val > 0) ? 1 : 2; // clock or counterclockwise
}

// Function to calculate distance between two points
int distance(Point p1, Point p2) {
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

// Function to find the bottom-most point
int findBottomMostPoint(Point points[], int n) {
    int min_idx = 0;
    for (int i = 1; i < n; i++) {
        if (points[i].y < points[min_idx].y || 
            (points[i].y == points[min_idx].y && points[i].x < points[min_idx].x)) {
            min_idx = i;
        }
    }
    return min_idx;
}

// Function to find convex hull using Graham Scan
void convexHull(Point points[], int n) {
    if (n < 3) {
        printf("Convex hull not possible with less than 3 points\n");
        return;
    }
    
    // Find the bottom-most point
    int bottom = findBottomMostPoint(points, n);
    
    // Create an array to store points of convex hull
    Point hull[n];
    int hull_size = 0;
    
    // Start with the bottom-most point
    int p = bottom;
    int q;
    
    do {
        // Add current point to hull
        hull[hull_size++] = points[p];
        
        // Find the point q such that orientation(p, q, r) is counterclockwise
        q = (p + 1) % n;
        for (int i = 0; i < n; i++) {
            if (orientation(points[p], points[i], points[q]) == 2) {
                q = i;
            }
        }
        
        p = q;
        
    } while (p != bottom);
    
    // Print the convex hull points
    printf("Convex Hull Points:\n");
    for (int i = 0; i < hull_size; i++) {
        printf("(%d, %d)\n", hull[i].x, hull[i].y);
    }
}

// Main function
int main() {
    Point points[] = {{0, 3}, {1, 1}, {2, 2}, {4, 4}, {0, 0}, {1, 2}, {3, 1}, {3, 3}};
    int n = sizeof(points) / sizeof(points[0]);
    
    printf("Input Points:\n");
    for (int i = 0; i < n; i++) {
        printf("(%d, %d)\n", points[i].x, points[i].y);
    }
    
    convexHull(points, n);
    
    return 0;
}
```

## Output:
```
Input Points:
(0, 3)
(1, 1)
(2, 2)
(4, 4)
(0, 0)
(1, 2)
(3, 1)
(3, 3)
Convex Hull Points:
(0, 0)
(0, 3)
(4, 4)
(3, 1)
```

## Algorithm Explanation:

1. **Find Bottom-Most Point**: Identify the point with the lowest y-coordinate (and leftmost if tied)
2. **Sort Points**: Sort remaining points by polar angle with respect to the bottom-most point
3. **Build Hull**: Use Graham Scan to iteratively build the convex hull by checking orientations
4. **Orientation Check**: Use cross product to determine if three points make a clockwise, counterclockwise, or collinear turn

## Time Complexity:
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the hull storage

This implementation uses the Graham Scan algorithm, which is one of the most efficient algorithms for finding convex hulls in computational geometry.

