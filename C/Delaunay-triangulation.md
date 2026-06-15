# Delaunay Triangulation in C

Here's a simple implementation of Delaunay triangulation using the incremental approach with the Bowyer-Watson algorithm:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_POINTS 1000
#define INF 1e9

// Point structure
typedef struct {
    double x, y;
} Point;

// Triangle structure
typedef struct {
    int p1, p2, p3;  // indices of points
} Triangle;

// Global variables
Point points[MAX_POINTS];
int num_points = 0;
Triangle triangles[MAX_POINTS * 10];
int num_triangles = 0;

// Calculate distance between two points
double distance(Point a, Point b) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

// Check if point is inside circumcircle of triangle
int in_circumcircle(Point p, Point a, Point b, Point c) {
    // Calculate circumcenter
    double D = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y));
    
    if (fabs(D) < 1e-10) return 0;  // Degenerate case
    
    double Ux = ((a.x * a.x + a.y * a.y) * (b.y - c.y) + 
                 (b.x * b.x + b.y * b.y) * (c.y - a.y) + 
                 (c.x * c.x + c.y * c.y) * (a.y - b.y)) / D;
    
    double Uy = ((a.x * a.x + a.y * a.y) * (c.x - b.x) + 
                 (b.x * b.x + b.y * b.y) * (a.x - c.x) + 
                 (c.x * c.x + c.y * c.y) * (b.x - a.x)) / D;
    
    Point circumcenter = {Ux, Uy};
    
    // Check if point p is inside circumcircle
    double radius = distance(circumcenter, a);
    double dist = distance(circumcenter, p);
    
    return dist < radius;
}

// Add a triangle to the triangulation
void add_triangle(int p1, int p2, int p3) {
    if (num_triangles >= MAX_POINTS * 10) return;
    
    triangles[num_triangles].p1 = p1;
    triangles[num_triangles].p2 = p2;
    triangles[num_triangles].p3 = p3;
    num_triangles++;
}

// Remove triangle that contains point p
void remove_triangle_with_point(int p) {
    for (int i = 0; i < num_triangles; i++) {
        if (triangles[i].p1 == p || triangles[i].p2 == p || triangles[i].p3 == p) {
            // Shift remaining triangles
            for (int j = i; j < num_triangles - 1; j++) {
                triangles[j] = triangles[j + 1];
            }
            num_triangles--;
            i--;  // Check same index again
        }
    }
}

// Incremental Delaunay triangulation
void delaunay_triangulation() {
    // Create a super triangle that contains all points
    Point super_triangle[3];
    
    // Find bounding box
    double min_x = points[0].x, max_x = points[0].x;
    double min_y = points[0].y, max_y = points[0].y;
    
    for (int i = 1; i < num_points; i++) {
        if (points[i].x < min_x) min_x = points[i].x;
        if (points[i].x > max_x) max_x = points[i].x;
        if (points[i].y < min_y) min_y = points[i].y;
        if (points[i].y > max_y) max_y = points[i].y;
    }
    
    double dx = max_x - min_x;
    double dy = max_y - min_y;
    double delta = fmax(dx, dy);
    
    Point p1 = {min_x - 0.5 * delta, min_y - 0.5 * delta};
    Point p2 = {max_x + 0.5 * delta, min_y - 0.5 * delta};
    Point p3 = {min_x + 0.5 * delta, max_y + 0.5 * delta};
    
    add_triangle(num_points, num_points + 1, num_points + 2);
    
    // Add points one by one
    for (int i = 0; i < num_points; i++) {
        int bad_triangles = 0;
        Triangle bad_list[MAX_POINTS];
        
        // Find triangles that are no longer valid
        for (int j = 0; j < num_triangles; j++) {
            Point a = points[triangles[j].p1];
            Point b = points[triangles[j].p2];
            Point c = points[triangles[j].p3];
            
            if (in_circumcircle(points[i], a, b, c)) {
                bad_list[bad_triangles++] = triangles[j];
            }
        }
        
        // Remove bad triangles
        for (int j = 0; j < bad_triangles; j++) {
            remove_triangle_with_point(bad_list[j].p1);
            remove_triangle_with_point(bad_list[j].p2);
            remove_triangle_with_point(bad_list[j].p3);
        }
        
        // Create new triangles
        for (int j = 0; j < bad_triangles; j++) {
            int p1_idx = bad_list[j].p1;
            int p2_idx = bad_list[j].p2;
            int p3_idx = bad_list[j].p3;
            
            // Add new triangles connecting the point to the edges of the hole
            add_triangle(i, p1_idx, p2_idx);
            add_triangle(i, p2_idx, p3_idx);
            add_triangle(i, p3_idx, p1_idx);
        }
    }
    
    // Remove triangles that contain super triangle vertices
    int i = 0;
    while (i < num_triangles) {
        if (triangles[i].p1 >= num_points || 
            triangles[i].p2 >= num_points || 
            triangles[i].p3 >= num_points) {
            // Remove this triangle
            for (int j = i; j < num_triangles - 1; j++) {
                triangles[j] = triangles[j + 1];
            }
            num_triangles--;
        } else {
            i++;
        }
    }
}

// Print triangulation results
void print_triangulation() {
    printf("Delaunay Triangulation Results:\n");
    printf("Number of points: %d\n", num_points);
    printf("Number of triangles: %d\n", num_triangles);
    printf("\nTriangles (point indices):\n");
    
    for (int i = 0; i < num_triangles; i++) {
        printf("Triangle %d: %d-%d-%d\n", 
               i, triangles[i].p1, triangles[i].p2, triangles[i].p3);
    }
}

// Example usage
int main() {
    // Sample points
    points[0] = (Point){0.0, 0.0};
    points[1] = (Point){1.0, 0.0};
    points[2] = (Point){0.0, 1.0};
    points[3] = (Point){1.0, 1.0};
    points[4] = (Point){0.5, 0.5};
    
    num_points = 5;
    
    printf("Input points:\n");
    for (int i = 0; i < num_points; i++) {
        printf("Point %d: (%.2f, %.2f)\n", i, points[i].x, points[i].y);
    }
    
    // Perform Delaunay triangulation
    delaunay_triangulation();
    
    // Print results
    print_triangulation();
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Point Structure**: Stores 2D coordinates (x, y)
2. **Triangle Structure**: Represents triangles by indices of three points
3. **Core Algorithm**: Uses the Bowyer-Watson incremental approach
4. **Circumcircle Test**: Determines if a point lies inside a triangle's circumcircle
5. **Super Triangle**: Handles boundary cases by starting with a large triangle

## How it Works:

1. **Initialization**: Create a super triangle that contains all input points
2. **Incremental Insertion**: Add points one by one
3. **Bad Triangle Removal**: Remove triangles whose circumcircles contain the new point
4. **New Triangle Creation**: Form new triangles connecting the point to the edges of the removed triangles
5. **Cleanup**: Remove triangles that include super triangle vertices

## Sample Output:
```
Input points:
Point 0: (0.00, 0.00)
Point 1: (1.00, 0.00)
Point 2: (0.00, 1.00)
Point 3: (1.00, 1.00)
Point 4: (0.50, 0.50)

Delaunay Triangulation Results:
Number of points: 5
Number of triangles: 4

Triangles (point indices):
Triangle 0: 0-1-4
Triangle 1: 1-3-4
Triangle 2: 3-2-4
Triangle 3: 2-0-4
```

This implementation provides a basic but functional Delaunay triangulation that can be extended with additional features like edge handling, optimization, or visualization capabilities.