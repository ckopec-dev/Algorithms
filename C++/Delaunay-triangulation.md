# Delaunay Triangulation in C++

Here's a complete implementation of a Delaunay triangulation algorithm using the incremental approach:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>

struct Point {
    double x, y;
    Point(double x = 0, double y = 0) : x(x), y(y) {}
    
    bool operator==(const Point& other) const {
        return std::abs(x - other.x) < 1e-9 && std::abs(y - other.y) < 1e-9;
    }
};

struct Triangle {
    Point a, b, c;
    Triangle(const Point& p1, const Point& p2, const Point& p3) 
        : a(p1), b(p2), c(p3) {}
};

class DelaunayTriangulation {
private:
    std::vector<Point> points;
    std::vector<Triangle> triangles;
    
    // Calculate the circumcircle of a triangle
    struct Circumcircle {
        Point center;
        double radius;
        Circumcircle(const Point& a, const Point& b, const Point& c) {
            // Calculate circumcenter and radius
            double d = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y));
            
            if (std::abs(d) < 1e-9) {
                // Collinear points - this shouldn't happen in valid triangulation
                center = Point(0, 0);
                radius = std::numeric_limits<double>::max();
                return;
            }
            
            double ux = (a.x * a.x + a.y * a.y) * (b.y - c.y) + 
                       (b.x * b.x + b.y * b.y) * (c.y - a.y) + 
                       (c.x * c.x + c.y * c.y) * (a.y - b.y);
            double uy = (a.x * a.x + a.y * a.y) * (c.x - b.x) + 
                       (b.x * b.x + b.y * b.y) * (a.x - c.x) + 
                       (c.x * c.x + c.y * c.y) * (b.x - a.x);
            
            center = Point(ux / d, uy / d);
            radius = std::sqrt((a.x - center.x) * (a.x - center.x) + 
                              (a.y - center.y) * (a.y - center.y));
        }
    };
    
    // Check if point is inside circumcircle of triangle
    bool isInCircumcircle(const Point& p, const Triangle& t) {
        Circumcircle circ(t.a, t.b, t.c);
        double dist = std::sqrt((p.x - circ.center.x) * (p.x - circ.center.x) + 
                               (p.y - circ.center.y) * (p.y - circ.center.y));
        return dist < circ.radius;
    }
    
    // Check if triangle is valid (not degenerate)
    bool isValidTriangle(const Triangle& t) {
        // Check if points are collinear
        double area = std::abs((t.b.x - t.a.x) * (t.c.y - t.a.y) - 
                              (t.c.x - t.a.x) * (t.b.y - t.a.y));
        return area > 1e-9;
    }
    
public:
    void addPoint(const Point& p) {
        points.push_back(p);
    }
    
    void triangulate() {
        if (points.size() < 3) return;
        
        // Create initial super triangle
        Point super_a(-1000, -1000);
        Point super_b(1000, -1000);
        Point super_c(0, 1000);
        
        triangles.push_back(Triangle(super_a, super_b, super_c));
        
        // Incremental triangulation
        for (const Point& p : points) {
            std::vector<Triangle> badTriangles;
            
            // Find all triangles whose circumcircle contains the new point
            for (size_t i = 0; i < triangles.size(); i++) {
                if (isInCircumcircle(p, triangles[i])) {
                    badTriangles.push_back(triangles[i]);
                }
            }
            
            // Create a polygonal hole
            std::vector<std::pair<Point, Point>> polygon;
            
            for (const Triangle& t : badTriangles) {
                // For each triangle, find edges that are not shared with other bad triangles
                std::vector<std::pair<Point, Point>> edges;
                edges.push_back({t.a, t.b});
                edges.push_back({t.b, t.c});
                edges.push_back({t.c, t.a});
                
                for (const auto& edge : edges) {
                    bool isShared = false;
                    for (const Triangle& other : badTriangles) {
                        if (&other != &t) {
                            if ((other.a == edge.first && other.b == edge.second) ||
                                (other.b == edge.first && other.c == edge.second) ||
                                (other.c == edge.first && other.a == edge.second) ||
                                (other.a == edge.second && other.b == edge.first) ||
                                (other.b == edge.second && other.c == edge.first) ||
                                (other.c == edge.second && other.a == edge.first)) {
                                isShared = true;
                                break;
                            }
                        }
                    }
                    
                    if (!isShared) {
                        polygon.push_back(edge);
                    }
                }
            }
            
            // Remove bad triangles
            triangles.erase(
                std::remove_if(triangles.begin(), triangles.end(),
                    [&badTriangles](const Triangle& t) {
                        return std::find(badTriangles.begin(), badTriangles.end(), t) != badTriangles.end();
                    }),
                triangles.end()
            );
            
            // Create new triangles from the polygon
            for (const auto& edge : polygon) {
                triangles.push_back(Triangle(edge.first, edge.second, p));
            }
        }
        
        // Remove triangles that contain super triangle vertices
        triangles.erase(
            std::remove_if(triangles.begin(), triangles.end(),
                [&super_a, &super_b, &super_c](const Triangle& t) {
                    return t.a == super_a || t.b == super_a || t.c == super_a ||
                           t.a == super_b || t.b == super_b || t.c == super_b ||
                           t.a == super_c || t.b == super_c || t.c == super_c;
                }),
            triangles.end()
        );
    }
    
    void printTriangles() const {
        std::cout << "Delaunay Triangles:\n";
        for (size_t i = 0; i < triangles.size(); i++) {
            const Triangle& t = triangles[i];
            std::cout << "Triangle " << i + 1 << ": (" 
                      << t.a.x << ", " << t.a.y << ") -> ("
                      << t.b.x << ", " << t.b.y << ") -> ("
                      << t.c.x << ", " << t.c.y << ")\n";
        }
    }
    
    const std::vector<Triangle>& getTriangles() const {
        return triangles;
    }
};

// Example usage
int main() {
    DelaunayTriangulation dt;
    
    // Add some sample points
    dt.addPoint(Point(0, 0));
    dt.addPoint(Point(1, 0));
    dt.addPoint(Point(0, 1));
    dt.addPoint(Point(1, 1));
    dt.addPoint(Point(0.5, 0.5));
    
    // Perform triangulation
    dt.triangulate();
    
    // Print results
    dt.printTriangles();
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Point Structure**: Simple 2D point representation with x and y coordinates
2. **Triangle Structure**: Represents a triangle with three vertices
3. **Circumcircle Calculation**: Computes circumcenter and radius for triangle validation
4. **Incremental Algorithm**: Adds points one by one and updates triangulation
5. **Bad Triangle Removal**: Identifies and removes triangles whose circumcircle contains new points
6. **Super Triangle**: Uses a large initial triangle to handle edge cases

## How it Works:

1. **Initialization**: Start with a large "super triangle" that contains all input points
2. **Point Insertion**: For each new point:
   - Find all triangles whose circumcircle contains the new point
   - Remove these "bad" triangles
   - Create new triangles by connecting the new point to the boundary edges
3. **Cleanup**: Remove triangles that reference the super triangle vertices

## Time Complexity:
- **Average case**: O(n log n)
- **Worst case**: O(n²)

## Space Complexity: O(n)

This implementation provides a solid foundation for Delaunay triangulation and can be extended with additional optimizations for production use.

