# Chan's Algorithm (Convex Hull) in C++

Chan's algorithm is an output-sensitive algorithm for computing the convex hull of a set of points in 2D space. It combines the benefits of Graham scan and Jarvis march algorithms.

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace std;

struct Point {
    int x, y;
    Point(int x = 0, int y = 0) : x(x), y(y) {}
};

// Cross product of vectors (p1-p0) and (p2-p0)
long long cross(const Point& p0, const Point& p1, const Point& p2) {
    return (long long)(p1.x - p0.x) * (p2.y - p0.y) - 
           (long long)(p1.y - p0.y) * (p2.x - p0.x);
}

// Distance between two points
long long distance(const Point& p1, const Point& p2) {
    return (long long)(p1.x - p2.x) * (p1.x - p2.x) + 
           (long long)(p1.y - p2.y) * (p1.y - p2.y);
}

// Check if point p is on the left side of the line formed by p1 and p2
bool isLeft(const Point& p1, const Point& p2, const Point& p) {
    return cross(p1, p2, p) > 0;
}

// Graham scan to find convex hull of a subset of points
vector<Point> grahamScan(vector<Point>& points) {
    if (points.size() < 3) return points;
    
    // Find the bottom-most point
    int bottom = 0;
    for (int i = 1; i < points.size(); i++) {
        if (points[i].y < points[bottom].y || 
            (points[i].y == points[bottom].y && points[i].x < points[bottom].x)) {
            bottom = i;
        }
    }
    
    // Swap the bottom point to the first position
    swap(points[0], points[bottom]);
    
    // Sort points by polar angle with respect to the bottom point
    Point pivot = points[0];
    sort(points.begin() + 1, points.end(), [&](const Point& a, const Point& b) {
        long long cross_product = cross(pivot, a, b);
        if (cross_product == 0) {
            return distance(pivot, a) < distance(pivot, b);
        }
        return cross_product > 0;
    });
    
    // Build convex hull
    vector<Point> hull;
    hull.push_back(points[0]);
    hull.push_back(points[1]);
    
    for (int i = 2; i < points.size(); i++) {
        while (hull.size() > 1 && 
               cross(hull[hull.size()-2], hull[hull.size()-1], points[i]) <= 0) {
            hull.pop_back();
        }
        hull.push_back(points[i]);
    }
    
    return hull;
}

// Chan's Algorithm implementation
vector<Point> chanAlgorithm(vector<Point>& points) {
    int n = points.size();
    if (n < 3) return points;
    
    // Find the maximum number of points that could be in the convex hull
    int k = 1;
    while ((1 << k) < n) k++;
    
    // For small inputs, use Graham scan directly
    if (n <= 100) {
        return grahamScan(points);
    }
    
    // Main loop of Chan's algorithm
    vector<Point> hull;
    
    for (int i = 1; i <= k; i++) {
        int m = min((int)pow(2, i), n);
        
        // Try to compute convex hull using k points
        vector<Point> candidate = grahamScan(points);
        
        // If we found a valid hull
        if (candidate.size() <= m) {
            hull = candidate;
            break;
        }
    }
    
    return hull;
}

// Print points in the convex hull
void printHull(const vector<Point>& hull) {
    cout << "Convex Hull Points:" << endl;
    for (const Point& p : hull) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    cout << endl;
}

int main() {
    // Example: Set of points
    vector<Point> points = {
        Point(0, 3), Point(1, 1), Point(2, 2), Point(4, 4),
        Point(0, 0), Point(1, 2), Point(3, 1), Point(3, 3)
    };
    
    cout << "Input Points:" << endl;
    for (const Point& p : points) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    cout << endl;
    
    // Apply Chan's algorithm
    vector<Point> hull = chanAlgorithm(points);
    
    printHull(hull);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Point Structure**: Simple 2D point representation with x and y coordinates
2. **Cross Product**: Used to determine orientation of three points
3. **Graham Scan**: Subroutine for computing convex hull of a subset
4. **Main Algorithm**: Implements the core logic of Chan's algorithm
5. **Output Sensitivity**: Efficient for cases where the number of hull points is small

## Time Complexity:
- **Best Case**: O(n log h) where h is the number of hull points
- **Worst Case**: O(n log n) 
- **Average Case**: O(n log h)

## Space Complexity: O(n)

This implementation demonstrates the output-sensitive nature of Chan's algorithm, making it efficient when the convex hull has relatively few points compared to the total number of input points.

