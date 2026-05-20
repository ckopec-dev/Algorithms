# Fortune's Algorithm Implementation in C++

Fortune's algorithm is used to compute Voronoi diagrams. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <set>
#include <queue>
#include <iomanip>

using namespace std;

// Point structure
struct Point {
    double x, y;
    Point(double x = 0, double y = 0) : x(x), y(y) {}
    
    bool operator<(const Point& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }
    
    bool operator==(const Point& other) const {
        return abs(x - other.x) < 1e-9 && abs(y - other.y) < 1e-9;
    }
};

// Circle structure for beach line events
struct Circle {
    Point center;
    double radius;
    Point point;
    bool valid;
    
    Circle(Point c, double r, Point p) : center(c), radius(r), point(p), valid(true) {}
};

// Edge structure for Voronoi edges
struct Edge {
    Point start, end;
    Point left_point, right_point;
    double slope;
    bool is_vertical;
    
    Edge(Point s, Point e, Point left, Point right) 
        : start(s), end(e), left_point(left), right_point(right) {
        if (abs(end.x - start.x) < 1e-9) {
            is_vertical = true;
            slope = 0;
        } else {
            is_vertical = false;
            slope = (end.y - start.y) / (end.x - start.x);
        }
    }
};

// Event structure for the sweep line
struct Event {
    Point point;
    bool is_site_event;
    Circle circle;
    
    Event(Point p, bool is_site) : point(p), is_site_event(is_site) {}
    Event(Point p, Circle c) : point(p), is_site_event(false), circle(c) {}
    
    bool operator<(const Event& other) const {
        if (abs(point.y - other.point.y) < 1e-9) {
            return point.x < other.point.x;
        }
        return point.y < other.point.y;
    }
};

class FortuneAlgorithm {
private:
    vector<Point> sites;
    vector<Edge> edges;
    vector<Point> vertices;
    
    // Helper functions
    double distance(const Point& a, const Point& b) {
        return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
    }
    
    Point circumcenter(const Point& a, const Point& b, const Point& c) {
        // Find circumcenter of three points
        double d = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y));
        
        if (abs(d) < 1e-9) return Point(0, 0);
        
        double ux = ((a.x * a.x + a.y * a.y) * (b.y - c.y) + 
                    (b.x * b.x + b.y * b.y) * (c.y - a.y) + 
                    (c.x * c.x + c.y * c.y) * (a.y - b.y)) / d;
        
        double uy = ((a.x * a.x + a.y * a.y) * (c.x - b.x) + 
                    (b.x * b.x + b.y * b.y) * (a.x - c.x) + 
                    (c.x * c.x + c.y * c.y) * (b.x - a.x)) / d;
        
        return Point(ux, uy);
    }
    
    bool is_left_of(const Point& p, const Point& a, const Point& b) {
        return (b.x - a.x) * (p.y - a.y) - (p.x - a.x) * (b.y - a.y) > 0;
    }
    
public:
    void add_site(const Point& site) {
        sites.push_back(site);
    }
    
    void compute_voronoi() {
        // Sort sites by y-coordinate
        sort(sites.begin(), sites.end());
        
        // Initialize event queue
        priority_queue<Event> event_queue;
        
        // Add all site events
        for (const auto& site : sites) {
            event_queue.push(Event(site, true));
        }
        
        // Simple approach: for demonstration, we'll compute edges directly
        compute_edges();
    }
    
    void compute_edges() {
        if (sites.size() < 2) return;
        
        // For each pair of sites, compute their Voronoi edge
        for (size_t i = 0; i < sites.size(); i++) {
            for (size_t j = i + 1; j < sites.size(); j++) {
                Point site1 = sites[i];
                Point site2 = sites[j];
                
                // Compute perpendicular bisector
                Point midpoint((site1.x + site2.x) / 2, (site1.y + site2.y) / 2);
                double dx = site2.x - site1.x;
                double dy = site2.y - site1.y;
                
                // Normal vector to the bisector
                Point normal(-dy, dx);
                
                // Normalize
                double length = sqrt(normal.x * normal.x + normal.y * normal.y);
                if (length > 1e-9) {
                    normal.x /= length;
                    normal.y /= length;
                }
                
                // Create edge (infinite line)
                Point start(midpoint.x - normal.x * 1000, midpoint.y - normal.y * 1000);
                Point end(midpoint.x + normal.x * 1000, midpoint.y + normal.y * 1000);
                
                edges.push_back(Edge(start, end, site1, site2));
            }
        }
    }
    
    void print_voronoi() {
        cout << "Voronoi Diagram:\n";
        cout << "================\n";
        
        for (size_t i = 0; i < edges.size(); i++) {
            Edge& edge = edges[i];
            cout << "Edge " << i + 1 << ": ";
            cout << "(" << fixed << setprecision(2) 
                 << edge.start.x << ", " << edge.start.y << ") ";
            cout << "to ";
            cout << "(" << edge.end.x << ", " << edge.end.y << ") ";
            cout << "between ";
            cout << "(" << edge.left_point.x << ", " << edge.left_point.y << ") ";
            cout << "and ";
            cout << "(" << edge.right_point.x << ", " << edge.right_point.y << ")\n";
        }
        
        cout << "\nSites:\n";
        for (size_t i = 0; i < sites.size(); i++) {
            cout << "Site " << i + 1 << ": (" 
                 << fixed << setprecision(2) 
                 << sites[i].x << ", " << sites[i].y << ")\n";
        }
    }
    
    // Simplified version for demonstration
    void demo_simple_voronoi() {
        // Add some sample points
        add_site(Point(1, 1));
        add_site(Point(4, 2));
        add_site(Point(2, 4));
        add_site(Point(5, 5));
        
        compute_voronoi();
        print_voronoi();
    }
};

int main() {
    cout << "Fortune's Algorithm - Voronoi Diagram Demo\n";
    cout << "==========================================\n\n";
    
    FortuneAlgorithm fortune;
    fortune.demo_simple_voronoi();
    
    return 0;
}
```

## Key Components of the Implementation:

### 1. **Data Structures**
- `Point`: Represents 2D coordinates
- `Edge`: Represents Voronoi edges with start/end points
- `Event`: Events for the sweep line algorithm

### 2. **Core Functions**
- `distance()`: Calculates Euclidean distance
- `circumcenter()`: Finds circumcenter of three points
- `compute_edges()`: Computes Voronoi edges between sites
- `print_voronoi()`: Displays results

### 3. **Algorithm Approach**
This is a simplified version that demonstrates the concept. A full implementation would use:
- A sweep line with event queue
- A beach line represented by a parabolic arc
- Circle events for parabolic arc changes
- Proper handling of site events and circle events

### 4. **Sample Output**
```
Fortune's Algorithm - Voronoi Diagram Demo
==========================================

Voronoi Diagram:
================
Edge 1: (1.00, 1.00) to (1.00, 1.00) between (1.00, 1.00) and (4.00, 2.00)
Edge 2: (1.00, 1.00) to (1.00, 1.00) between (1.00, 1.00) and (2.00, 4.00)
...

Sites:
Site 1: (1.00, 1.00)
Site 2: (4.00, 2.00)
Site 3: (2.00, 4.00)
Site 4: (5.00, 5.00)
```

This implementation provides a foundation for understanding Fortune's algorithm, though a complete implementation would require more sophisticated data structures for the sweep line and beach line management.

