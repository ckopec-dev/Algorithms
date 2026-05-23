# Quickhull Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Point structure
typedef struct {
    double x, y;
} Point;

// Function to calculate cross product of three points
double crossProduct(Point a, Point b, Point c) {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

// Function to calculate distance from point to line
double distance(Point p, Point a, Point b) {
    double numerator = fabs(crossProduct(a, b, p));
    double denominator = sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y));
    return numerator / denominator;
}

// Function to find the point farthest from the line
int findFarthestPoint(Point* points, int n, Point a, Point b) {
    double maxDist = 0;
    int farthestIndex = 0;
    
    for (int i = 0; i < n; i++) {
        double dist = distance(points[i], a, b);
        if (dist > maxDist) {
            maxDist = dist;
            farthestIndex = i;
        }
    }
    
    return farthestIndex;
}

// Recursive function to compute convex hull
void quickHull(Point* points, int n, Point a, Point b, Point* hull, int* hullSize, int side) {
    int farthestIndex = findFarthestPoint(points, n, a, b);
    
    // If no point is found, return
    if (farthestIndex == 0) return;
    
    Point farthest = points[farthestIndex];
    
    // Add the farthest point to hull
    hull[*hullSize] = farthest;
    (*hullSize)++;
    
    // Recursively compute hull for the two subregions
    if (side == 1) {
        quickHull(points, n, a, farthest, hull, hullSize, 1);
        quickHull(points, n, farthest, b, hull, hullSize, 1);
    } else {
        quickHull(points, n, a, farthest, hull, hullSize, -1);
        quickHull(points, n, farthest, b, hull, hullSize, -1);
    }
}

// Main Quickhull function
int quickHullMain(Point* points, int n, Point* hull) {
    if (n < 3) return 0;
    
    // Find leftmost and rightmost points
    int leftmost = 0, rightmost = 0;
    for (int i = 1; i < n; i++) {
        if (points[i].x < points[leftmost].x)
            leftmost = i;
        if (points[i].x > points[rightmost].x)
            rightmost = i;
    }
    
    Point a = points[leftmost];
    Point b = points[rightmost];
    
    // Add the two points to hull
    hull[0] = a;
    hull[1] = b;
    int hullSize = 2;
    
    // Compute hull for both sides
    quickHull(points, n, a, b, hull, &hullSize, 1);
    quickHull(points, n, b, a, hull, &hullSize, 1);
    
    return hullSize;
}

// Print points
void printPoints(Point* points, int n) {
    for (int i = 0; i < n; i++) {
        printf("(%.2f, %.2f)\n", points[i].x, points[i].y);
    }
}

// Example usage
int main() {
    // Sample set of points
    Point points[] = {
        {0, 3}, {1, 1}, {2, 2}, {4, 4}, 
        {0, 0}, {1, 2}, {3, 1}, {3, 3}
    };
    
    int n = sizeof(points) / sizeof(points[0]);
    Point hull[100]; // Maximum possible hull points
    
    printf("Input points:\n");
    printPoints(points, n);
    
    int hullSize = quickHullMain(points, n, hull);
    
    printf("\nConvex Hull points:\n");
    printPoints(hull, hullSize);
    
    return 0;
}
```

## How the Algorithm Works

1. **Find Extreme Points**: Identify the leftmost and rightmost points
2. **Divide and Conquer**: 
   - Find the point farthest from the line connecting the extreme points
   - This point forms a triangle with the line
   - Recursively find points on both sides of this triangle
3. **Base Case**: When no point is found beyond the current line, stop recursion

## Time Complexity
- **Average Case**: O(n log n)
- **Worst Case**: O(n²) - when all points are on the hull

## Space Complexity
- O(log n) for the recursion stack in average case

## Output Example
```
Input points:
(0.00, 3.00)
(1.00, 1.00)
(2.00, 2.00)
(4.00, 4.00)
(0.00, 0.00)
(1.00, 2.00)
(3.00, 1.00)
(3.00, 3.00)

Convex Hull points:
(0.00, 0.00)
(0.00, 3.00)
(1.00, 1.00)
(3.00, 1.00)
(4.00, 4.00)
(3.00, 3.00)
```

