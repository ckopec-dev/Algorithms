# De Boor's Algorithm Implementation

De Boor's algorithm is used for evaluating B-spline curves at specific parameter values. Here's a Python implementation:

```python
import numpy as np
import matplotlib.pyplot as plt

def de_boor_cox(knots, coefficients, degree, t):
    """
    Evaluate a B-spline curve using De Boor's algorithm
    
    Parameters:
    knots: array of knot values
    coefficients: array of control points (2D)
    degree: degree of the spline
    t: parameter value to evaluate at
    
    Returns:
    Point on the curve at parameter t
    """
    
    # Find the span where t lies
    n = len(knots) - degree - 2
    if t == knots[n + 1]:
        span = n
    else:
        span = np.searchsorted(knots, t, side='right') - 1
    
    # Initialize the de Boor points
    d = np.array(coefficients[span - degree:span + 1], dtype=float)
    
    # De Boor's algorithm
    for r in range(1, degree + 1):
        for i in range(degree, r - 1, -1):
            if knots[span + i + 1] == knots[span + i - r + 1]:
                alpha = 0.0
            else:
                alpha = (t - knots[span + i - r + 1]) / (knots[span + i + 1] - knots[span + i - r + 1])
            
            d[i] = (1 - alpha) * d[i - 1] + alpha * d[i]
    
    return d[degree]

def de_boor_curve(knots, coefficients, degree, num_points=100):
    """
    Generate points along a B-spline curve
    
    Parameters:
    knots: array of knot values
    coefficients: array of control points (2D)
    degree: degree of the spline
    num_points: number of points to generate
    
    Returns:
    Array of points on the curve
    """
    
    # Find the valid parameter range
    t_min = knots[degree]
    t_max = knots[len(knots) - degree - 1]
    
    # Generate parameter values
    t_values = np.linspace(t_min, t_max, num_points)
    
    # Evaluate curve at each parameter value
    points = []
    for t in t_values:
        point = de_boor_cox(knots, coefficients, degree, t)
        points.append(point)
    
    return np.array(points)

# Example usage
if __name__ == "__main__":
    # Define control points (3D example)
    control_points = np.array([
        [0, 0],
        [1, 2],
        [2, 0],
        [3, 2],
        [4, 0]
    ])
    
    # Define knots for a cubic B-spline (degree=3)
    # Note: For a curve with n+1 control points and degree p, we need n+p+2 knots
    degree = 3
    n = len(control_points) - 1
    
    # Create clamped uniform knots
    knots = [0, 0, 0, 0]  # First 4 knots (degree + 1)
    knots.extend(range(1, n - degree + 1))  # Middle knots
    knots.extend([n - degree] * 4)  # Last 4 knots
    
    # Convert to numpy array
    knots = np.array(knots)
    
    # Generate curve points
    curve_points = de_boor_curve(knots, control_points, degree, num_points=100)
    
    # Plot the results
    plt.figure(figsize=(10, 6))
    
    # Plot control polygon
    plt.plot(control_points[:, 0], control_points[:, 1], 'ro-', label='Control Polygon')
    
    # Plot curve
    plt.plot(curve_points[:, 0], curve_points[:, 1], 'b-', linewidth=2, label='B-spline Curve')
    
    # Mark control points
    plt.scatter(control_points[:, 0], control_points[:, 1], color='red', s=50, zorder=5)
    
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('B-spline Curve using De Boor\'s Algorithm')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.axis('equal')
    plt.show()
    
    # Example: Evaluate at specific parameter
    t_eval = 2.5
    result_point = de_boor_cox(knots, control_points, degree, t_eval)
    print(f"Point on curve at t={t_eval}: ({result_point[0]:.3f}, {result_point[1]:.3f})")
```

## Key Features of this Implementation:

1. **`de_boor_cox()`**: Core function that evaluates a B-spline at a single parameter value using De Boor's algorithm
2. **`de_boor_curve()`**: Generates multiple points along the curve for plotting
3. **Knot handling**: Properly handles knot vectors with appropriate boundary conditions
4. **Numerical stability**: Uses proper division to avoid numerical errors
5. **Visualization**: Includes plotting capabilities to visualize the results

## Algorithm Steps:

1. **Find span**: Determine which segment the parameter t lies in
2. **Initialize**: Set up initial de Boor points from control points
3. **Iterate**: Apply De Boor's recurrence relation for each refinement level
4. **Return**: The final point is the result of the algorithm

This implementation demonstrates how De Boor's algorithm efficiently computes B-spline curve points by using a recursive approach that builds upon previous computations, making it more numerically stable than direct polynomial evaluation methods.

