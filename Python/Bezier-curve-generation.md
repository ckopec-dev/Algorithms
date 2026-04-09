# Bezier Curve Generation Algorithm in Python

```python
import numpy as np
import matplotlib.pyplot as plt

def bezier_curve(points, num_points=100):
    """
    Generate a Bezier curve from control points
    
    Args:
        points: List of control points as [x, y] coordinates
        num_points: Number of points to generate along the curve
    
    Returns:
        List of (x, y) points along the Bezier curve
    """
    n = len(points) - 1  # Degree of the curve
    curve_points = []
    
    for t in np.linspace(0, 1, num_points):
        # De Casteljau's algorithm
        temp_points = points.copy()
        
        for i in range(n):
            for j in range(n - i):
                temp_points[j] = (1 - t) * temp_points[j] + t * temp_points[j + 1]
        
        curve_points.append(temp_points[0])
    
    return curve_points

def bezier_curve_recursive(points, t):
    """
    Recursive implementation of Bezier curve calculation
    
    Args:
        points: List of control points
        t: Parameter value between 0 and 1
    
    Returns:
        Point on the curve at parameter t
    """
    if len(points) == 1:
        return points[0]
    
    # Recursively calculate the curve
    new_points = []
    for i in range(len(points) - 1):
        x = (1 - t) * points[i][0] + t * points[i + 1][0]
        y = (1 - t) * points[i][1] + t * points[i + 1][1]
        new_points.append([x, y])
    
    return bezier_curve_recursive(new_points, t)

def plot_bezier_curve(control_points, curve_points, title="Bezier Curve"):
    """
    Plot the Bezier curve with control points
    """
    # Extract x and y coordinates
    control_x = [p[0] for p in control_points]
    control_y = [p[1] for p in control_points]
    
    curve_x = [p[0] for p in curve_points]
    curve_y = [p[1] for p in curve_points]
    
    # Create plot
    plt.figure(figsize=(10, 8))
    plt.plot(curve_x, curve_y, 'b-', linewidth=2, label='Bezier Curve')
    plt.plot(control_x, control_y, 'ro-', linewidth=1, markersize=8, label='Control Points')
    
    # Connect control points with lines
    plt.plot(control_x, control_y, 'r--', alpha=0.5, linewidth=1)
    
    plt.grid(True, alpha=0.3)
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title(title)
    plt.legend()
    plt.axis('equal')
    plt.show()

# Example usage
if __name__ == "__main__":
    # Example 1: Quadratic Bezier curve (3 control points)
    print("Quadratic Bezier Curve Example:")
    control_points_1 = [[0, 0], [1, 2], [2, 0]]
    curve_points_1 = bezier_curve(control_points_1, 100)
    print(f"Control points: {control_points_1}")
    print(f"First few curve points: {curve_points_1[:5]}")
    
    # Example 2: Cubic Bezier curve (4 control points)
    print("\nCubic Bezier Curve Example:")
    control_points_2 = [[0, 0], [1, 1], [2, 1], [3, 0]]
    curve_points_2 = bezier_curve(control_points_2, 100)
    print(f"Control points: {control_points_2}")
    print(f"First few curve points: {curve_points_2[:5]}")
    
    # Example 3: Using recursive implementation
    print("\nRecursive Implementation Example:")
    point = bezier_curve_recursive(control_points_1, 0.5)
    print(f"Point at t=0.5: {point}")
    
    # Plot the curves
    plot_bezier_curve(control_points_1, curve_points_1, "Quadratic Bezier Curve")
    plot_bezier_curve(control_points_2, curve_points_2, "Cubic Bezier Curve")
```

## Key Features of This Implementation:

1. **De Casteljau's Algorithm**: Uses the numerically stable De Casteljau algorithm for curve generation
2. **Flexible Degree**: Works with any number of control points (linear, quadratic, cubic, etc.)
3. **Recursive Alternative**: Includes a recursive implementation for educational purposes
4. **Visualization**: Includes plotting functionality to visualize the curves
5. **Modular Design**: Separate functions for curve generation and visualization

## Mathematical Background:

A Bezier curve of degree n is defined by n+1 control points and uses the Bernstein polynomial basis functions:

```
B(t) = Σ(i=0 to n) C(n,i) * (1-t)^(n-i) * t^i * P_i
```

Where:
- C(n,i) is the binomial coefficient
- P_i are the control points
- t is the parameter ranging from 0 to 1

This implementation provides both computational efficiency and educational clarity for understanding Bezier curve generation.

