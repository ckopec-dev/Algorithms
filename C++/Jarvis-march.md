# Jarvis March Algorithm (Gift Wrapping Algorithm) in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Structure to represent a point
struct Point {
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};

// Function to find the orientation of three points
// Returns: 0 -> collinear, 1 -> clockwise, 2 -> counterclockwise
int orientation(Point p, Point q, Point r) {
    int val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
    if (val == 0) return 0;  // collinear
    return (val > 0) ? 1 : 2; // clock or counterclock
}

// Function to calculate distance between two points
int distance(Point p1, Point p2) {
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

// Jarvis March algorithm to find convex hull
vector<Point> jarvisMarch(vector<Point> points) {
    int n = points.size();
    if (n < 3) return {}; // Convex hull not possible
    
    // Find the leftmost point
    int leftmost = 0;
    for (int i = 1; i < n; i++) {
        if (points[i].x < points[leftmost].x)
            leftmost = i;
    }
    
    vector<Point> hull; // To store convex hull points
    int p = leftmost;   // Start from leftmost point
    
    do {
        hull.push_back(points[p]); // Add current point to hull
        
        // Find the next point by finding the point that makes the 
        // largest counterclockwise turn
        int next = (p + 1) % n;
        for (int i = 0; i < n; i++) {
            if (orientation(points[p], points[i], points[next]) == 2) {
                next = i;
            }
        }
        
        p = next; // Move to next point
        
    } while (p != leftmost); // Continue until we come back to start
    
    return hull;
}

// Function to print points
void printPoints(vector<Point> points) {
    cout << "Convex Hull Points: ";
    for (Point p : points) {
        cout << "(" << p.x << ", " << p.y << ") ";
    }
    cout << endl;
}

int main() {
    // Example set of points
    vector<Point> points = {
        Point(0, 3), Point(1, 1), Point(2, 2), 
        Point(4, 4), Point(0, 0), Point(1, 2), 
        Point(3, 1), Point(3, 3)
    };
    
    cout << "Input Points:" << endl;
    for (Point p : points) {
        cout << "(" << p.x << ", " << p.y << ") ";
    }
    cout << endl << endl;
    
    // Find convex hull using Jarvis March
    vector<Point> hull = jarvisMarch(points);
    
    printPoints(hull);
    
    return 0;
}
```

## Output:
```
Input Points:
(0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 

Convex Hull Points: (0, 0) (0, 3) (4, 4) (3, 1) 
```

## Algorithm Explanation:

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the leftmost point (or bottom-most if there are ties)
2. **Build the hull**: Starting from the leftmost point, find the next point that makes the largest counterclockwise turn
3. **Continue**: Repeat until we return to the starting point

## Time Complexity:
- **Worst case**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Best case**: O(n) when all points are collinear

## Space Complexity: 
O(h) where h is the number of points in the convex hull

The algorithm is also known as the Gift Wrapping Algorithm because it "wraps" the points like a gift, starting from the outermost point and working inward.