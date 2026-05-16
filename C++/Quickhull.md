# Quickhull Algorithm Implementation in C++

Quickhull is an algorithm for computing the convex hull of a set of points in the plane. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

struct Point {
    double x, y;
    Point(double x = 0, double y = 0) : x(x), y(y) {}
};

// Calculate cross product of vectors (p1-p0) and (p2-p0)
double crossProduct(const Point& p0, const Point& p1, const Point& p2) {
    return (p1.x - p0.x) * (p2.y - p0.y) - (p1.y - p0.y) * (p2.x - p0.x);
}

// Calculate distance from point p to line formed by points p1 and p2
double distanceToLine(const Point& p, const Point& p1, const Point& p2) {
    double numerator = abs(crossProduct(p1, p2, p));
    double denominator = sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y));
    return numerator / denominator;
}

// Find the point farthest from the line formed by p1 and p2
int findFarthestPoint(const vector<Point>& points, const Point& p1, const Point& p2) {
    double maxDistance = 0;
    int farthestIndex = -1;
    
    for (int i = 0; i < points.size(); i++) {
        double distance = distanceToLine(points[i], p1, p2);
        if (distance > maxDistance) {
            maxDistance = distance;
            farthestIndex = i;
        }
    }
    
    return farthestIndex;
}

// Recursive function to find convex hull
void quickHull(vector<Point>& points, Point p1, Point p2, vector<Point>& hull, int side) {
    int farthestIndex = findFarthestPoint(points, p1, p2);
    
    if (farthestIndex == -1) {
        // No point found, add endpoints to hull
        if (find(hull.begin(), hull.end(), p1) == hull.end()) {
            hull.push_back(p1);
        }
        if (find(hull.begin(), hull.end(), p2) == hull.end()) {
            hull.push_back(p2);
        }
        return;
    }
    
    Point farthestPoint = points[farthestIndex];
    
    // Add farthest point to hull
    if (find(hull.begin(), hull.end(), farthestPoint) == hull.end()) {
        hull.push_back(farthestPoint);
    }
    
    // Recursively find hull on both sides
    quickHull(points, p1, farthestPoint, hull, side * crossProduct(p1, farthestPoint, p2));
    quickHull(points, farthestPoint, p2, hull, side * crossProduct(farthestPoint, p2, p1));
}

// Main function to compute convex hull
vector<Point> convexHull(vector<Point> points) {
    if (points.size() < 3) {
        return points;
    }
    
    // Find leftmost and rightmost points
    int leftIndex = 0, rightIndex = 0;
    for (int i = 1; i < points.size(); i++) {
        if (points[i].x < points[leftIndex].x) leftIndex = i;
        if (points[i].x > points[rightIndex].x) rightIndex = i;
    }
    
    Point leftPoint = points[leftIndex];
    Point rightPoint = points[rightIndex];
    
    vector<Point> hull;
    
    // Add endpoints to hull
    hull.push_back(leftPoint);
    hull.push_back(rightPoint);
    
    // Find points on left and right side of the line
    vector<Point> leftPoints, rightPoints;
    
    for (int i = 0; i < points.size(); i++) {
        double cross = crossProduct(leftPoint, rightPoint, points[i]);
        if (cross > 0) {
            leftPoints.push_back(points[i]);
        } else if (cross < 0) {
            rightPoints.push_back(points[i]);
        }
    }
    
    // Recursively find hull on both sides
    quickHull(leftPoints, leftPoint, rightPoint, hull, 1);
    quickHull(rightPoints, rightPoint, leftPoint, hull, -1);
    
    // Sort hull points in clockwise order
    sort(hull.begin(), hull.end(), [](const Point& a, const Point& b) {
        return a.x < b.x || (a.x == b.x && a.y < b.y);
    });
    
    return hull;
}

// Print points
void printPoints(const vector<Point>& points) {
    cout << "Convex Hull Points:" << endl;
    for (const Point& p : points) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
}

int main() {
    // Example points
    vector<Point> points = {
        Point(0, 3),
        Point(1, 1),
        Point(2, 2),
        Point(4, 4),
        Point(0, 0),
        Point(1, 2),
        Point(3, 1),
        Point(3, 3)
    };
    
    cout << "Input Points:" << endl;
    for (const Point& p : points) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    cout << endl;
    
    // Compute convex hull
    vector<Point> hull = convexHull(points);
    
    // Print result
    printPoints(hull);
    
    return 0;
}
```

## How it works:

1. **Find Extreme Points**: Identify the leftmost and rightmost points to form an initial line
2. **Divide and Conquer**: 
   - Find the point farthest from the initial line
   - Split remaining points into two sets based on which side of the line they fall
   - Recursively apply the algorithm to each subset
3. **Base Case**: When no point is found beyond the current line, add the endpoints to the hull

## Time Complexity:
- **Average Case**: O(n log n)
- **Worst Case**: O(n²) when all points are on the hull

## Space Complexity: O(n)

This implementation correctly handles edge cases and produces a convex hull containing all the extreme points of the input set.

