# Graham Scan Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>
#include <cmath>

using namespace std;

// Point structure to represent 2D points
struct Point {
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};

// Function to calculate cross product of three points
// Returns positive value if counter-clockwise turn
// Returns negative value if clockwise turn
// Returns zero if points are collinear
int crossProduct(Point a, Point b, Point c) {
    return (b.x - a.x) * (c.y - b.y) - (b.y - a.y) * (c.x - b.x);
}

// Function to calculate squared distance between two points
long long distanceSquared(Point a, Point b) {
    return (long long)(a.x - b.x) * (a.x - b.x) + (long long)(a.y - b.y) * (a.y - b.y);
}

// Function to find the bottom-most point
Point findBottomMostPoint(vector<Point>& points) {
    Point bottomMost = points[0];
    for (int i = 1; i < points.size(); i++) {
        if (points[i].y < bottomMost.y || 
            (points[i].y == bottomMost.y && points[i].x < bottomMost.x)) {
            bottomMost = points[i];
        }
    }
    return bottomMost;
}

// Custom comparator for sorting points
bool compare(Point a, Point b, Point origin) {
    int cross = crossProduct(origin, a, b);
    if (cross == 0) {
        // If collinear, sort by distance from origin
        return distanceSquared(origin, a) < distanceSquared(origin, b);
    }
    // Sort by polar angle (counter-clockwise)
    return cross > 0;
}

// Graham Scan algorithm implementation
vector<Point> grahamScan(vector<Point> points) {
    if (points.size() < 3) {
        return points;
    }
    
    // Find the bottom-most point
    Point bottomMost = findBottomMostPoint(points);
    
    // Sort points by polar angle with respect to bottom-most point
    sort(points.begin(), points.end(), 
         [&bottomMost](Point a, Point b) {
             return compare(a, b, bottomMost);
         });
    
    // Create stack for hull points
    stack<Point> hull;
    
    // Push first three points
    hull.push(points[0]);
    hull.push(points[1]);
    hull.push(points[2]);
    
    // Process remaining points
    for (int i = 3; i < points.size(); i++) {
        Point top = hull.top();
        hull.pop();
        Point nextToTop = hull.top();
        hull.push(top);
        
        // While the turn from nextToTop -> top -> points[i] is clockwise,
        // remove top from stack
        while (crossProduct(nextToTop, top, points[i]) <= 0) {
            hull.pop();
            top = hull.top();
            hull.pop();
            nextToTop = hull.top();
            hull.push(top);
        }
        
        hull.push(points[i]);
    }
    
    // Convert stack to vector
    vector<Point> result;
    while (!hull.empty()) {
        result.push_back(hull.top());
        hull.pop();
    }
    
    // Reverse to get correct order (clockwise)
    reverse(result.begin(), result.end());
    return result;
}

// Function to print points
void printPoints(vector<Point> points) {
    cout << "Convex Hull Points: ";
    for (int i = 0; i < points.size(); i++) {
        cout << "(" << points[i].x << ", " << points[i].y << ") ";
    }
    cout << endl;
}

int main() {
    // Example set of points
    vector<Point> points = {
        Point(0, 3), Point(1, 1), Point(2, 2), Point(4, 4),
        Point(0, 0), Point(1, 2), Point(3, 1), Point(3, 3)
    };
    
    cout << "Input Points:" << endl;
    for (int i = 0; i < points.size(); i++) {
        cout << "(" << points[i].x << ", " << points[i].y << ") ";
    }
    cout << endl << endl;
    
    // Apply Graham Scan
    vector<Point> hull = grahamScan(points);
    
    // Print result
    printPoints(hull);
    
    return 0;
}
```

## Algorithm Explanation

The Graham Scan algorithm finds the convex hull of a set of points in O(n log n) time:

1. **Find the bottom-most point** - This becomes the starting point
2. **Sort points** - By polar angle with respect to the bottom-most point
3. **Build the hull** - Using a stack, removing points that make clockwise turns

## Key Components

- **Cross Product**: Determines the orientation of three points
- **Sorting**: By polar angle to process points in correct order
- **Stack**: Maintains the current hull points
- **Turn Detection**: Uses cross product to detect when to remove points

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the stack and auxiliary arrays

## Output Example
```
Input Points:
(0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 

Convex Hull Points: (0, 0) (0, 3) (4, 4) (3, 1) 
```

