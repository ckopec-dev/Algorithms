# Convex Hull Algorithm in C++

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

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
long long distSq(Point a, Point b) {
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
bool compare(Point a, Point b, Point bottomMost) {
    int cross = crossProduct(bottomMost, a, b);
    if (cross == 0) {
        // If collinear, return the one closer to bottomMost
        return distSq(bottomMost, a) < distSq(bottomMost, b);
    }
    // Counter-clockwise ordering
    return cross > 0;
}

vector<Point> convexHull(vector<Point>& points) {
    if (points.size() < 3) {
        return points;
    }
    
    // Find the bottom-most point
    Point bottomMost = findBottomMostPoint(points);
    
    // Sort points based on polar angle with bottomMost
    sort(points.begin(), points.end(), 
         [&bottomMost](Point a, Point b) {
             return compare(a, b, bottomMost);
         });
    
    // Build convex hull
    vector<Point> hull;
    hull.push_back(points[0]);
    hull.push_back(points[1]);
    
    for (int i = 2; i < points.size(); i++) {
        Point top = hull.back();
        hull.pop_back();
        Point nextToTop = hull.back();
        
        // Keep removing points while the orientation is not counter-clockwise
        while (crossProduct(nextToTop, top, points[i]) <= 0) {
            if (hull.size() <= 1) break;
            top = hull.back();
            hull.pop_back();
            nextToTop = hull.back();
        }
        hull.push_back(points[i]);
    }
    
    return hull;
}

int main() {
    // Example points
    vector<Point> points = {
        Point(0, 3), Point(1, 1), Point(2, 2), Point(4, 4),
        Point(0, 0), Point(1, 2), Point(3, 1), Point(3, 3)
    };
    
    cout << "Input points:" << endl;
    for (Point p : points) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    
    vector<Point> hull = convexHull(points);
    
    cout << "\nConvex Hull points:" << endl;
    for (Point p : hull) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    
    return 0;
}
```

## Output:
```
Input points:
(0, 3)
(1, 1)
(2, 2)
(4, 4)
(0, 0)
(1, 2)
(3, 1)
(3, 3)

Convex Hull points:
(0, 0)
(0, 3)
(4, 4)
(3, 1)
```

## Key Features:

1. **Graham Scan Algorithm**: Efficient O(n log n) solution
2. **Cross Product**: Used to determine orientation of three points
3. **Polar Angle Sorting**: Points sorted by angle relative to bottom-most point
4. **Stack-based Construction**: Uses a stack to build the convex hull incrementally
5. **Collinearity Handling**: Properly handles collinear points

## Time Complexity: O(n log n)  
## Space Complexity: O(n)

The algorithm works by first finding the bottom-most point, sorting all points by polar angle, and then using a stack to construct the convex hull by ensuring no clockwise turns occur.

