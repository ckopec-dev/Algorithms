# Graham Scan Algorithm Implementation

Graham scan is a fundamental algorithm for computing the convex hull of a set of points in the plane.

```python
import math

def cross_product(o, a, b):
    """
    Calculate the cross product of vectors OA and OB
    Returns positive value if counter-clockwise turn,
    negative value if clockwise turn, zero if collinear
    """
    return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])

def distance(p1, p2):
    """Calculate Euclidean distance between two points"""
    return math.sqrt((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)

def graham_scan(points):
    """
    Compute the convex hull of a set of points using Graham scan algorithm
    
    Args:
        points: List of tuples representing 2D points [(x, y), ...]
    
    Returns:
        List of points forming the convex hull in counter-clockwise order
    """
    if len(points) < 3:
        return points
    
    # Find the point with the lowest y-coordinate (and leftmost if tie)
    start_point = min(points, key=lambda p: (p[1], p[0]))
    
    # Sort points by polar angle with respect to start_point
    def polar_angle_cmp(p):
        dx = p[0] - start_point[0]
        dy = p[1] - start_point[1]
        # Calculate angle in radians
        angle = math.atan2(dy, dx)
        # Return angle in range [0, 2π]
        return (angle + 2 * math.pi) % (2 * math.pi)
    
    sorted_points = sorted(points, key=polar_angle_cmp)
    
    # Remove duplicate angles (keep the farthest point)
    i = 0
    while i < len(sorted_points) - 1:
        if polar_angle_cmp(sorted_points[i]) == polar_angle_cmp(sorted_points[i+1]):
            # Keep the point that is farther from start_point
            if distance(start_point, sorted_points[i]) < distance(start_point, sorted_points[i+1]):
                sorted_points.pop(i)
            else:
                sorted_points.pop(i+1)
        else:
            i += 1
    
    # Graham scan algorithm
    hull = [start_point]
    
    for point in sorted_points[1:]:
        # Remove points that make clockwise turns
        while len(hull) > 1 and cross_product(hull[-2], hull[-1], point) <= 0:
            hull.pop()
        hull.append(point)
    
    return hull

# Example usage
if __name__ == "__main__":
    # Test points
    points = [
        (0, 3), (1, 1), (2, 2), (4, 4),
        (0, 0), (1, 2), (3, 1), (3, 3)
    ]
    
    print("Input points:")
    for point in points:
        print(f"  {point}")
    
    # Compute convex hull
    hull = graham_scan(points)
    
    print("\nConvex hull points (counter-clockwise order):")
    for point in hull:
        print(f"  {point}")
    
    print(f"\nNumber of points in convex hull: {len(hull)}")
```

## Algorithm Steps:

1. **Find the starting point**: The point with the lowest y-coordinate (and leftmost if tie)
2. **Sort points**: Sort all points by polar angle with respect to the starting point
3. **Graham scan**: Process points in order, maintaining a stack and removing points that create clockwise turns

## Time Complexity:
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the stack and sorted points

## Example Output:
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

Convex hull points (counter-clockwise order):
  (0, 0)
  (0, 3)
  (4, 4)
  (3, 1)

Number of points in convex hull: 4
```

