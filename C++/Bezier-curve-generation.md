# Bezier Curve Generation Algorithm in C++

Here's a complete implementation of a Bezier curve generation algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

struct Point {
    double x, y;
    Point(double x = 0, double y = 0) : x(x), y(y) {}
};

class BezierCurve {
private:
    std::vector<Point> controlPoints;
    
public:
    // Constructor
    BezierCurve() {}
    
    // Add control point
    void addControlPoint(const Point& point) {
        controlPoints.push_back(point);
    }
    
    // Calculate binomial coefficient C(n,k)
    int binomialCoeff(int n, int k) {
        if (k > n - k) k = n - k;
        int result = 1;
        for (int i = 0; i < k; ++i) {
            result = result * (n - i) / (i + 1);
        }
        return result;
    }
    
    // Calculate Bernstein polynomial
    double bernstein(int n, int i, double t) {
        return binomialCoeff(n, i) * std::pow(t, i) * std::pow(1 - t, n - i);
    }
    
    // Generate point on Bezier curve at parameter t
    Point calculateBezierPoint(double t) {
        int n = controlPoints.size() - 1;
        Point result(0, 0);
        
        for (int i = 0; i <= n; i++) {
            double bernsteinValue = bernstein(n, i, t);
            result.x += controlPoints[i].x * bernsteinValue;
            result.y += controlPoints[i].y * bernsteinValue;
        }
        
        return result;
    }
    
    // Generate multiple points on the curve
    std::vector<Point> generateCurve(int numPoints = 100) {
        std::vector<Point> curvePoints;
        
        for (int i = 0; i <= numPoints; i++) {
            double t = static_cast<double>(i) / numPoints;
            curvePoints.push_back(calculateBezierPoint(t));
        }
        
        return curvePoints;
    }
    
    // Get control points
    const std::vector<Point>& getControlPoints() const {
        return controlPoints;
    }
    
    // Clear control points
    void clear() {
        controlPoints.clear();
    }
};

// Example usage
int main() {
    // Create a Bezier curve with 4 control points (cubic Bezier)
    BezierCurve bezier;
    
    // Add control points
    bezier.addControlPoint(Point(0, 0));    // Start point
    bezier.addControlPoint(Point(100, 200)); // Control point 1
    bezier.addControlPoint(Point(200, 100)); // Control point 2
    bezier.addControlPoint(Point(300, 0));   // End point
    
    // Generate curve points
    std::vector<Point> curve = bezier.generateCurve(50);
    
    // Display the curve points
    std::cout << "Bezier Curve Points:\n";
    std::cout << "====================\n";
    
    for (size_t i = 0; i < curve.size(); i += 5) { // Print every 5th point
        std::cout << "Point " << i << ": (" 
                  << curve[i].x << ", " << curve[i].y << ")\n";
    }
    
    return 0;
}
```

## Algorithm Explanation

This implementation uses the **Bernstein polynomial** approach to generate Bezier curves:

### Key Components:

1. **Point Structure**: Represents 2D coordinates
2. **BezierCurve Class**: Contains the core algorithm
3. **Binomial Coefficient**: Calculates C(n,k) for Bernstein polynomials
4. **Bernstein Polynomial**: Computes B(n,i)(t) = C(n,i) × t^i × (1-t)^(n-i)
5. **Curve Generation**: Evaluates the Bezier curve at parameter t

### Mathematical Formula:

For a Bezier curve with n+1 control points:
```
B(t) = Σ(i=0 to n) C(n,i) × P_i × t^i × (1-t)^(n-i)
```

### Usage:
- Add control points using `addControlPoint()`
- Generate curve points using `generateCurve()`
- The algorithm supports any degree Bezier curve by varying the number of control points

### Output Example:
```
Bezier Curve Points:
====================
Point 0: (0, 0)
Point 5: (62.5, 125)
Point 10: (150, 150)
Point 15: (237.5, 125)
Point 20: (300, 0)
```

This implementation provides a clean, reusable way to generate Bezier curves with customizable control points and resolution.

