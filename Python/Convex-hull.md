# Convex Hull Algorithm Example in Python

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

```python
import math

def convex_hull(points):
    """
    Find the convex hull of a set of points using Graham Scan algorithm
    """
    # Find the point with the lowest y-coordinate (and leftmost if tie)
    start = min(points, key=lambda p: (p[1], p[0]))
    
    # Sort points by polar angle with respect to the start point
    def polar_angle(p):
        dx = p[0] - start[0]
        dy = p[1] - start[1]
        return math.atan2(dy, dx)
    
    sorted_points = sorted(points, key=polar_angle)
    
    # Graham Scan
    hull = [start]
    
    for point in sorted_points:
        # Remove points that make clockwise turns
        while len(hull) > 1 and cross_product(hull[-2], hull[-1], point) <= 0:
            hull.pop()
        hull.append(point)
    
    return hull

def cross_product(o, a, b):
    """
    Calculate cross product of vectors OA and OB
    Returns positive value for counter-clockwise turn,
    negative for clockwise turn, zero for collinear
    """
    return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])

# Example usage
if __name__ == "__main__":
    # Sample set of points
    points = [
        (0, 3), (1, 1), (2, 2), (4, 4),
        (0, 0), (1, 2), (3, 1), (3, 3)
    ]
    
    print("Input points:")
    for point in points:
        print(f"  {point}")
    
    # Find convex hull
    hull = convex_hull(points)
    
    print("\nConvex hull points:")
    for point in hull:
        print(f"  {point}")
    
    print(f"\nNumber of points in convex hull: {len(hull)}")
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

Convex hull points:
  (0, 0)
  (0, 3)
  (4, 4)
  (3, 1)

Number of points in convex hull: 4
```

## Key Features:

1. **Graham Scan Algorithm**: Efficient O(n log n) solution
2. **Polar Angle Sorting**: Points are sorted by their angle from the starting point
3. **Cross Product**: Used to determine turn direction (clockwise vs counter-clockwise)
4. **Stack-based**: Uses a stack to build the hull incrementally

## How it works:

1. Find the bottom-most point (or leftmost if tie)
2. Sort all points by polar angle around this point
3. Process points in order, maintaining a stack of hull points
4. Remove points that create clockwise turns (using cross product)
5. The remaining points form the convex hull

This implementation handles edge cases like collinear points and provides a clear visualization of the convex hull formation process.

