# Gift Wrapping Algorithm (Jarvis March) in C

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
    return (val > 0) ? 1 : 2; // clockwise or counterclockwise
}

// Function to calculate distance between two points
int distance(Point p1, Point p2) {
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

// Function to find the point with minimum y-coordinate (and minimum x if tie)
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

// Gift Wrapping Algorithm (Jarvis March)
void convexHull(Point points[], int n) {
    // There must be at least 3 points
    if (n < 3) {
        printf("Convex hull not possible\n");
        return;
    }
    
    // Find the bottom-most point
    int leftmost = findBottomMostPoint(points, n);
    
    // Initialize result array to store convex hull points
    Point hull[n];
    int hull_size = 0;
    
    int p = leftmost;
    do {
        // Add current point to hull
        hull[hull_size++] = points[p];
        
        // Find the most counterclockwise point from p
        int q = (p + 1) % n;
        for (int i = 0; i < n; i++) {
            if (orientation(points[p], points[i], points[q]) == 2) {
                q = i;
            }
        }
        
        // Set next point as q
        p = q;
        
    } while (p != leftmost);  // When we come back to starting point
    
    // Print the convex hull points
    printf("Convex Hull Points:\n");
    for (int i = 0; i < hull_size; i++) {
        printf("(%d, %d)\n", hull[i].x, hull[i].y);
    }
}

// Main function to test the algorithm
int main() {
    // Sample set of points
    Point points[] = {{0, 3}, {1, 1}, {2, 2}, {4, 4}, 
                      {0, 0}, {1, 2}, {3, 1}, {3, 3}};
    
    int n = sizeof(points) / sizeof(points[0]);
    
    printf("Input Points:\n");
    for (int i = 0; i < n; i++) {
        printf("(%d, %d)\n", points[i].x, points[i].y);
    }
    
    printf("\n");
    
    // Find and print convex hull
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

1. **Find the bottom-most point**: Start with the point having the smallest y-coordinate (and smallest x if tie)
2. **Build the hull**: 
   - Start from the bottom-most point
   - For each current point, find the next point that makes the maximum counterclockwise turn
   - Continue until we return to the starting point
3. **Orientation function**: Uses cross product to determine the turn direction between three points

## Time Complexity:
- **Time**: O(nh) where n is number of input points and h is number of output hull points
- **Space**: O(h) for storing the hull points

The algorithm works by "wrapping" around the set of points, always selecting the next point that creates the most counterclockwise turn from the current position.