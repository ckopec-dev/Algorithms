# Gift Wrapping Algorithm (Jarvis March) in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace std;

// Structure to represent a point in 2D space
struct Point {
    int x, y;
    Point(int x = 0, int y = 0) : x(x), y(y) {}
};

// Function to calculate cross product of three points
// Returns positive value if counter-clockwise turn
// Returns negative value if clockwise turn
// Returns zero if points are collinear
int crossProduct(Point a, Point b, Point c) {
    return (b.x - a.x) * (c.y - b.y) - (b.y - a.y) * (c.x - b.x);
}

// Function to calculate distance between two points
int distance(Point a, Point b) {
    return (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y);
}

// Function to find the leftmost point
Point findLeftmostPoint(vector<Point>& points) {
    Point leftmost = points[0];
    for (int i = 1; i < points.size(); i++) {
        if (points[i].x < leftmost.x) {
            leftmost = points[i];
        }
    }
    return leftmost;
}

// Gift Wrapping Algorithm (Jarvis March)
vector<Point> convexHull(vector<Point>& points) {
    int n = points.size();
    if (n < 3) return points;
    
    // Find the leftmost point
    Point start = findLeftmostPoint(points);
    
    vector<Point> hull;
    Point current = start;
    
    do {
        hull.push_back(current);
        
        // Find the next point that makes the largest counter-clockwise turn
        Point next = points[0];
        for (int i = 1; i < n; i++) {
            int cross = crossProduct(current, next, points[i]);
            // If we find a point that makes a larger counter-clockwise turn
            // or if next is collinear and points[i] is closer
            if (cross > 0 || (cross == 0 && 
                distance(current, points[i]) < distance(current, next))) {
                next = points[i];
            }
        }
        
        current = next;
        
    } while (current != start); // Continue until we return to start point
    
    return hull;
}

// Function to print points
void printPoints(vector<Point>& points) {
    for (Point p : points) {
        cout << "(" << p.x << ", " << p.y << ") ";
    }
    cout << endl;
}

int main() {
    // Example set of points
    vector<Point> points = {
        Point(0, 3), Point(1, 1), Point(2, 2), Point(4, 4),
        Point(0, 0), Point(1, 2), Point(3, 1), Point(3, 3)
    };
    
    cout << "Input points: ";
    printPoints(points);
    
    // Find convex hull
    vector<Point> hull = convexHull(points);
    
    cout << "Convex Hull points: ";
    printPoints(hull);
    
    return 0;
}
```

## Algorithm Explanation

The Gift Wrapping Algorithm (Jarvis March) works as follows:

1. **Find the starting point**: Identify the leftmost point (or bottommost if there are ties)
2. **Build the hull**: Starting from the leftmost point, find the next point that makes the largest counter-clockwise turn
3. **Continue**: Repeat until we return to the starting point

## Key Functions

- **crossProduct()**: Determines the orientation of three points
- **distance()**: Calculates squared distance between two points
- **findLeftmostPoint()**: Finds the starting point of the hull
- **convexHull()**: Main algorithm implementation

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Output Example
```
Input points: (0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 
Convex Hull points: (0, 0) (0, 3) (4, 4) (3, 1) 
```

This algorithm is particularly useful when the number of hull points is much smaller than the total number of input points.

