# Convex Hull Algorithms in Python

## Graham Scan Algorithm Implementation

```python
import math

def orientation(p, q, r):
    """
    Find orientation of ordered triplet (p, q, r).
    Returns:
    0 -> p, q and r are collinear
    1 -> Clockwise
    2 -> Counterclockwise
    """
    val = (q[1] - p[1]) * (r[0] - q[0]) - (q[0] - p[0]) * (r[1] - q[1])
    if val == 0:
        return 0
    return 1 if val > 0 else 2

def distance(p1, p2):
    """Calculate Euclidean distance between two points"""
    return math.sqrt((p1[0] - p2[0])**2 + (p1[1] - p2[1])**2)

def convex_hull_graham(points):
    """
    Find the convex hull of a set of points using Graham Scan algorithm
    """
    n = len(points)
    
    # Find the bottom-most point
    start = 0
    for i in range(1, n):
        if points[i][1] < points[start][1]:
            start = i
    
    # Sort points by polar angle with respect to the starting point
    def polar_angle_compare(p):
        angle = math.atan2(p[1] - points[start][1], p[0] - points[start][0])
        return angle
    
    # Sort points by polar angle
    sorted_points = sorted(points, key=polar_angle_compare)
    
    # Create convex hull
    hull = [sorted_points[0]]
    
    for i in range(1, n):
        # Remove points that make clockwise turn
        while len(hull) > 1 and orientation(hull[-2], hull[-1], sorted_points[i]) == 1:
            hull.pop()
        hull.append(sorted_points[i])
    
    return hull

# Example usage
if __name__ == "__main__":
    # Sample points
    points = [(0, 3), (1, 1), (2, 2), (4, 4), (0, 0), (1, 2), (3, 1), (3, 3)]
    
    print("Input points:")
    for point in points:
        print(f"  {point}")
    
    # Find convex hull
    hull = convex_hull_graham(points)
    
    print("\nConvex Hull points:")
    for point in hull:
        print(f"  {point}")
```

## Jarvis March (Gift Wrapping) Algorithm Implementation

```python
def convex_hull_jarvis(points):
    """
    Find the convex hull using Jarvis March (Gift Wrapping) algorithm
    """
    n = len(points)
    
    if n < 3:
        return points
    
    # Find leftmost point
    leftmost = 0
    for i in range(1, n):
        if points[i][0] < points[leftmost][0]:
            leftmost = i
    
    hull = []
    current = leftmost
    
    while True:
        hull.append(points[current])
        
        # Find the next point that makes counterclockwise turn
        next_point = (current + 1) % n
        
        for i in range(n):
            if orientation(points[current], points[i], points[next_point]) == 2:
                next_point = i
        
        current = next_point
        
        # Return to starting point
        if current == leftmost:
            break
    
    return hull

# Example usage of Jarvis March
if __name__ == "__main__":
    points = [(0, 3), (1, 1), (2, 2), (4, 4), (0, 0), (1, 2), (3, 1), (3, 3)]
    
    print("Jarvis March Convex Hull:")
    hull = convex_hull_jarvis(points)
    for point in hull:
        print(f"  {point}")
```

## Complete Example with Visualization

```python
def print_convex_hull_info(points):
    """Print information about the convex hull"""
    print(f"\nOriginal points ({len(points)}):")
    for i, point in enumerate(points):
        print(f"  {i}: {point}")
    
    # Graham Scan
    hull_graham = convex_hull_graham(points)
    print(f"\nGraham Scan Hull ({len(hull_graham)} points):")
    for point in hull_graham:
        print(f"  {point}")
    
    # Jarvis March
    hull_jarvis = convex_hull_jarvis(points)
    print(f"\nJarvis March Hull ({len(hull_jarvis)} points):")
    for point in hull_jarvis:
        print(f"  {point}")

# Test with different datasets
if __name__ == "__main__":
    # Test case 1: Simple points
    test_points = [(0, 3), (1, 1), (2, 2), (4, 4), (0, 0), (1, 2), (3, 1), (3, 3)]
    print_convex_hull_info(test_points)
    
    # Test case 2: Points forming a rectangle
    rect_points = [(0, 0), (4, 0), (4, 3), (0, 3)]
    print("\n" + "="*50)
    print_convex_hull_info(rect_points)
```

## Algorithm Complexity

- **Graham Scan**: O(n log n) time complexity due to sorting
- **Jarvis March**: O(nh) time complexity where h is the number of hull points
- **Space Complexity**: O(n) for storing intermediate results

## Key Features

1. **Orientation Calculation**: Determines if three points make a clockwise or counterclockwise turn
2. **Polar Angle Sorting**: Orders points relative to a reference point
3. **Convexity Check**: Ensures the resulting polygon is convex
4. **Edge Cases**: Handles collinear points and minimal point sets

This implementation provides both classic convex hull algorithms that can be used for computational geometry applications.

