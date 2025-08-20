```python
def bezier_curve(points, num_points=100):
    """
    Generate a Bezier curve from control points
    
    Args:
        points: List of control points as (x, y) tuples
        num_points: Number of points to generate on the curve
    
    Returns:
        List of (x, y) tuples representing the curve
    """
    def bernstein(n, i, t):
        """Calculate Bernstein polynomial coefficient"""
        from math import factorial
        if n == 0 and i == 0:
            return 1
        if i < 0 or i > n:
            return 0
        return factorial(n) / (factorial(i) * factorial(n - i)) * (t ** i) * ((1 - t) ** (n - i))
    
    def bezier_point(t):
        """Calculate a point on the Bezier curve at parameter t"""
        n = len(points) - 1
        x, y = 0, 0
        for i, (px, py) in enumerate(points):
            b = bernstein(n, i, t)
            x += px * b
            y += py * b
        return (x, y)
    
    # Generate curve points
    curve = []
    for i in range(num_points + 1):
        t = i / num_points
        curve.append(bezier_point(t))
    
    return curve

# Example usage
if __name__ == "__main__":
    # Define control points for a quadratic Bezier curve
    control_points = [(0, 0), (2, 4), (4, 0)]
    
    # Generate the curve
    curve_points = bezier_curve(control_points, num_points=50)
    
    # Print first few points
    print("First 10 points of the Bezier curve:")
    for i, point in enumerate(curve_points[:10]):
        print(f"Point {i}: ({point[0]:.2f}, {point[1]:.2f})")
    
    # Example with cubic Bezier curve
    cubic_points = [(0, 0), (1, 3), (3, 1), (4, 0)]
    cubic_curve = bezier_curve(cubic_points, num_points=50)
    
    print("\nFirst 5 points of cubic Bezier curve:")
    for i, point in enumerate(cubic_curve[:5]):
        print(f"Point {i}: ({point[0]:.2f}, {point[1]:.2f})")
```

This implementation includes:

1. **Main function `bezier_curve()`**: Takes control points and number of curve points to generate
2. **Bernstein polynomial helper**: Calculates the basis functions needed for Bezier curves
3. **Curve point calculation**: Computes individual points on the curve using the Bernstein polynomials
4. **Example usage**: Shows both quadratic and cubic Bezier curves

The algorithm works by:
- Using the Bernstein polynomial formula to weight control points
- Interpolating between control points based on parameter t (0 to 1)
- Generating evenly spaced points along the curve path

For a quadratic Bezier with 3 control points, it creates smooth curves that pass near the control points but don't necessarily go through them.

