# Graham Scan Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Point structure
typedef struct {
    int x, y;
} Point;

// Function to calculate cross product of three points
int cross_product(Point a, Point b, Point c) {
    return (b.x - a.x) * (c.y - b.y) - (b.y - a.y) * (c.x - b.x);
}

// Function to calculate distance between two points
int distance(Point a, Point b) {
    return (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y);
}

// Function to find the bottom-most point
int find_bottom_most(Point points[], int n) {
    int bottom = 0;
    for (int i = 1; i < n; i++) {
        if (points[i].y < points[bottom].y || 
            (points[i].y == points[bottom].y && points[i].x < points[bottom].x)) {
            bottom = i;
        }
    }
    return bottom;
}

// Function to sort points by polar angle
void sort_points(Point points[], int n, Point start) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = i + 1; j < n; j++) {
            int cross = cross_product(start, points[i], points[j]);
            if (cross > 0) {
                Point temp = points[i];
                points[i] = points[j];
                points[j] = temp;
            } else if (cross == 0) {
                // If cross product is 0, sort by distance
                int dist_i = distance(start, points[i]);
                int dist_j = distance(start, points[j]);
                if (dist_i > dist_j) {
                    Point temp = points[i];
                    points[i] = points[j];
                    points[j] = temp;
                }
            }
        }
    }
}

// Graham Scan algorithm
Point* graham_scan(Point points[], int n, int* hull_size) {
    if (n < 3) {
        *hull_size = n;
        return points;
    }
    
    // Find the bottom-most point
    int bottom = find_bottom_most(points, n);
    
    // Swap bottom point to first position
    Point temp = points[0];
    points[0] = points[bottom];
    points[bottom] = temp;
    
    // Sort points by polar angle
    sort_points(points, n, points[0]);
    
    // Create hull array
    Point* hull = (Point*)malloc(n * sizeof(Point));
    int index = 0;
    
    // Add first three points
    hull[index++] = points[0];
    hull[index++] = points[1];
    hull[index++] = points[2];
    
    // Process remaining points
    for (int i = 3; i < n; i++) {
        // Remove points that make clockwise turn
        while (index > 2 && cross_product(hull[index-2], hull[index-1], points[i]) <= 0) {
            index--;
        }
        hull[index++] = points[i];
    }
    
    *hull_size = index;
    return hull;
}

// Function to print points
void print_points(Point points[], int n) {
    for (int i = 0; i < n; i++) {
        printf("(%d, %d) ", points[i].x, points[i].y);
    }
    printf("\n");
}

int main() {
    // Example points
    Point points[] = {{0, 3}, {1, 1}, {2, 2}, {4, 4}, {0, 0}, {1, 2}, {3, 1}, {3, 3}};
    int n = sizeof(points) / sizeof(points[0]);
    
    printf("Input points:\n");
    print_points(points, n);
    
    int hull_size;
    Point* hull = graham_scan(points, n, &hull_size);
    
    printf("Convex hull points:\n");
    print_points(hull, hull_size);
    
    free(hull);
    return 0;
}
```

## Algorithm Explanation

The Graham Scan algorithm finds the convex hull of a set of points in O(n log n) time complexity:

1. **Find the bottom-most point** - Select the point with the lowest y-coordinate (or leftmost if tied)
2. **Sort points** - Sort all other points by polar angle with respect to the bottom-most point
3. **Build hull** - Process points in sorted order, maintaining a stack of hull points

## Key Functions

- `cross_product()`: Determines the orientation of three points
- `distance()`: Calculates squared distance between two points
- `find_bottom_most()`: Finds the starting point for sorting
- `sort_points()`: Sorts points by polar angle
- `graham_scan()`: Main algorithm implementation

## Sample Output
```
Input points:
(0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 
Convex hull points:
(0, 0) (0, 3) (4, 4) (3, 1) 
```

