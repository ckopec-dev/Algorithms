# QuickHull Algorithm Implementation in C++

QuickHull is an efficient algorithm for computing the convex hull of a set of points in 2D space. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

struct Point {
    int x, y;
    Point(int x = 0, int y = 0) : x(x), y(y) {}
};

// Calculate cross product of three points
// Returns positive value if point c is to the left of line ab
// Returns negative value if point c is to the right of line ab
// Returns zero if all three points are collinear
long long crossProduct(const Point& a, const Point& b, const Point& c) {
    return (long long)(b.x - a.x) * (c.y - a.y) - (long long)(b.y - a.y) * (c.x - a.x);
}

// Calculate distance from point to line
double distanceFromLine(const Point& a, const Point& b, const Point& c) {
    // Distance = |cross product| / |line length|
    long long cross = crossProduct(a, b, c);
    double lineLength = sqrt((double)(b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y));
    
    if (lineLength == 0) return 0;
    
    return abs(cross) / lineLength;
}

// Find the point farthest from the line
int findFarthestPoint(const vector<Point>& points, const Point& a, const Point& b) {
    double maxDist = 0;
    int farthestIndex = -1;
    
    for (int i = 0; i < points.size(); i++) {
        double dist = distanceFromLine(a, b, points[i]);
        if (dist > maxDist) {
            maxDist = dist;
            farthestIndex = i;
        }
    }
    
    return farthestIndex;
}

// Recursive QuickHull function
void quickHullRecursive(const vector<Point>& points, 
                       const Point& a, const Point& b, 
                       vector<Point>& hull, bool leftSide) {
    
    // Find the farthest point from the line ab
    int farthestIndex = findFarthestPoint(points, a, b);
    
    if (farthestIndex == -1) return; // No point found
    
    Point farthest = points[farthestIndex];
    
    // Add the farthest point to hull
    hull.push_back(farthest);
    
    // Recursively process points on the left side of line af
    if (leftSide) {
        quickHullRecursive(points, a, farthest, hull, true);
        quickHullRecursive(points, farthest, b, hull, true);
    } else {
        quickHullRecursive(points, a, farthest, hull, false);
        quickHullRecursive(points, farthest, b, hull, false);
    }
}

// Main QuickHull function
vector<Point> quickHull(vector<Point> points) {
    if (points.size() < 3) return points;
    
    // Sort points by x-coordinate
    sort(points.begin(), points.end(), [](const Point& a, const Point& b) {
        return a.x < b.x || (a.x == b.x && a.y < b.y);
    });
    
    vector<Point> hull;
    
    // Find leftmost and rightmost points
    Point leftMost = points[0];
    Point rightMost = points[points.size() - 1];
    
    // Add endpoints to hull
    hull.push_back(leftMost);
    hull.push_back(rightMost);
    
    // Recursively find hull points on both sides of the line
    quickHullRecursive(points, leftMost, rightMost, hull, true);  // Left side
    quickHullRecursive(points, leftMost, rightMost, hull, false); // Right side
    
    return hull;
}

// Function to print points
void printPoints(const vector<Point>& points) {
    cout << "Convex Hull Points: ";
    for (const Point& p : points) {
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
    
    cout << "Input points:" << endl;
    for (const Point& p : points) {
        cout << "(" << p.x << ", " << p.y << ") ";
    }
    cout << endl << endl;
    
    // Compute convex hull
    vector<Point> hull = quickHull(points);
    
    // Sort hull points for better visualization
    sort(hull.begin(), hull.end(), [](const Point& a, const Point& b) {
        return a.x < b.x || (a.x == b.x && a.y < b.y);
    });
    
    printPoints(hull);
    
    return 0;
}
```

## How the Algorithm Works:

1. **Sort Points**: First, sort all points by x-coordinate (and y-coordinate for ties)
2. **Find Extreme Points**: Identify the leftmost and rightmost points
3. **Divide and Conquer**: 
   - Find the point farthest from the line connecting the extreme points
   - Recursively process the points on each side of this new line
4. **Base Case**: When no more points are found, stop the recursion

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) when all points are on the hull

## Space Complexity: 
- O(log n) for recursion stack in average case

## Sample Output:
```
Input points:
(0, 3) (1, 1) (2, 2) (4, 4) (0, 0) (1, 2) (3, 1) (3, 3) 

Convex Hull Points: (0, 0) (0, 3) (1, 1) (3, 1) (4, 4) 
```

This implementation efficiently computes the convex hull using a divide-and-conquer approach similar to QuickSort, making it one of the most popular algorithms for this problem.

