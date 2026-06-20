# Jarvis March Algorithm (Gift Wrapping Algorithm) in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure to represent a point
struct Point {
    int x, y;
};

// Function to calculate cross product of three points
// Returns positive value if counter-clockwise turn,
// negative value if clockwise turn,
// zero if collinear
int orientation(struct Point p, struct Point q, struct Point r) {
    int val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
    return val;
}

// Function to calculate distance between two points
int distance(struct Point p1, struct Point p2) {
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

// Function to find the point with minimum y-coordinate
// If there are multiple points with same minimum y-coordinate,
// return the one with minimum x-coordinate
int findBottomMostPoint(struct Point points[], int n) {
    int min_idx = 0;
    
    for (int i = 1; i < n; i++) {
        if (points[i].y < points[min_idx].y || 
            (points[i].y == points[min_idx].y && points[i].x < points[min_idx].x)) {
            min_idx = i;
        }
    }
    
    return min_idx;
}

// Main function to compute convex hull using Jarvis March algorithm
void jarvisMarch(struct Point points[], int n, struct Point hull[]) {
    // Find the bottom-most point
    int leftmost = findBottomMostPoint(points, n);
    
    int p = leftmost;  // Start from leftmost point
    int next;
    int hullIndex = 0;
    
    do {
        // Add current point to hull
        hull[hullIndex++] = points[p];
        
        // Initialize next point as the first point
        next = (p + 1) % n;
        
        // Find the point that makes the largest clockwise turn
        for (int i = 0; i < n; i++) {
            // If orientation is counter-clockwise, update next
            if (orientation(points[p], points[i], points[next]) > 0) {
                next = i;
            }
        }
        
        p = next;
        
    } while (p != leftmost);  // Continue until we return to start point
    
    printf("Convex Hull Points:\n");
    for (int i = 0; i < hullIndex; i++) {
        printf("(%d, %d)\n", hull[i].x, hull[i].y);
    }
}

// Function to print all points
void printPoints(struct Point points[], int n) {
    printf("All Points:\n");
    for (int i = 0; i < n; i++) {
        printf("(%d, %d)\n", points[i].x, points[i].y);
    }
    printf("\n");
}

int main() {
    // Example set of points
    struct Point points[] = {{0, 3}, {1, 1}, {2, 2}, {4, 4}, 
                            {0, 0}, {1, 2}, {3, 1}, {3, 3}};
    
    int n = sizeof(points) / sizeof(points[0]);
    
    printf("Jarvis March (Gift Wrapping) Algorithm\n");
    printf("=====================================\n\n");
    
    printPoints(points, n);
    
    // Array to store convex hull points
    struct Point hull[n];
    
    // Compute convex hull
    jarvisMarch(points, n, hull);
    
    return 0;
}
```

## Output:
```
Jarvis March (Gift Wrapping) Algorithm
=====================================

All Points:
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

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the bottom-most point (or leftmost if there are ties)
2. **Build the hull**: Starting from this point, find the next point that makes the largest clockwise turn
3. **Continue**: Repeat until we return to the starting point
4. **Orientation check**: Uses cross product to determine turn direction

## Time Complexity:
- **Best case**: O(nh) where h is the number of points on the hull
- **Worst case**: O(n²) when all points are on the hull
- **Space complexity**: O(h) for storing the hull points

This algorithm is also known as the Gift Wrapping Algorithm because it simulates wrapping a gift with a string, starting from one corner and moving around the convex hull.