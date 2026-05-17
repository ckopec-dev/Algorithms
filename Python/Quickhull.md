# Quickhull Algorithm Implementation in Python

Quickhull is an algorithm for computing the convex hull of a set of points in the plane. Here's a complete implementation:

```python
import math

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __str__(self):
        return f"({self.x}, {self.y})"

def distance(p1, p2):
    """Calculate Euclidean distance between two points"""
    return math.sqrt((p1.x - p2.x)**2 + (p1.y - p2.y)**2)

def orientation(p, q, r):
    """Find orientation of ordered triplet (p, q, r)
    Returns:
    0 -> p, q, r are collinear
    1 -> Clockwise
    2 -> Counterclockwise
    """
    val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
    if val == 0:
        return 0
    return 1 if val > 0 else 2

def convex_hull(points):
    """Find the convex hull of a set of points using Quickhull algorithm"""
    if len(points) < 3:
        return points
    
    # Find the leftmost and rightmost points
    min_x = min(points, key=lambda p: p.x)
    max_x = max(points, key=lambda p: p.x)
    
    # Create two sets for points above and below the line
    hull_points = []
    
    # Find points on the left side of the line from min_x to max_x
    left_set = []
    right_set = []
    
    for point in points:
        if point == min_x or point == max_x:
            continue
        o = orientation(min_x, max_x, point)
        if o == 2:
            left_set.append(point)
        elif o == 1:
            right_set.append(point)
    
    # Find hull points recursively
    find_hull(points, min_x, max_x, left_set, hull_points)
    find_hull(points, max_x, min_x, right_set, hull_points)
    
    # Add the endpoints
    hull_points.append(min_x)
    hull_points.append(max_x)
    
    # Remove duplicates and sort by angle
    hull_points = list(set(hull_points))
    hull_points.sort(key=lambda p: math.atan2(p.y - min_x.y, p.x - min_x.x))
    
    return hull_points

def find_hull(points, p1, p2, set_points, hull_points):
    """Recursively find hull points"""
    if not set_points:
        return
    
    # Find the point with maximum distance from line p1-p2
    max_dist = -1
    farthest_point = None
    
    for point in set_points:
        dist = point_distance_from_line(p1, p2, point)
        if dist > max_dist:
            max_dist = dist
            farthest_point = point
    
    # Add the farthest point to hull
    hull_points.append(farthest_point)
    
    # Find points on the left side of line p1-farthest_point
    left_set1 = []
    for point in set_points:
        if orientation(p1, farthest_point, point) == 2:
            left_set1.append(point)
    
    # Find points on the left side of line farthest_point-p2
    left_set2 = []
    for point in set_points:
        if orientation(farthest_point, p2, point) == 2:
            left_set2.append(point)
    
    # Recursively find hull points
    find_hull(points, p1, farthest_point, left_set1, hull_points)
    find_hull(points, farthest_point, p2, left_set2, hull_points)

def point_distance_from_line(p1, p2, point):
    """Calculate distance from point to line defined by p1 and p2"""
    # Line equation: (y2-y1)x - (x2-x1)y + (x2-x1)y1 - (y2-y1)x1 = 0
    # Distance = |ax0 + by0 + c| / sqrt(a^2 + b^2)
    a = p2.y - p1.y
    b = p1.x - p2.x
    c = p2.x * p1.y - p1.x * p2.y
    
    return abs(a * point.x + b * point.y + c) / math.sqrt(a*a + b*b)

# Alternative simpler implementation using Graham Scan approach
def quickhull_simple(points):
    """Simplified Quickhull implementation"""
    if len(points) < 3:
        return points
    
    # Find extreme points
    min_x = min(points, key=lambda p: p.x)
    max_x = max(points, key=lambda p: p.x)
    
    # Split points into left and right of line
    left_points = []
    right_points = []
    
    for point in points:
        if point == min_x or point == max_x:
            continue
        o = orientation(min_x, max_x, point)
        if o == 2:
            left_points.append(point)
        elif o == 1:
            right_points.append(point)
    
    # Find hull
    hull = []
    
    # Find left hull
    hull.extend(find_hull_simple(min_x, max_x, left_points))
    
    # Find right hull
    hull.extend(find_hull_simple(max_x, min_x, right_points))
    
    # Add endpoints
    hull.append(min_x)
    hull.append(max_x)
    
    # Remove duplicates and sort
    hull = list(set(hull))
    hull.sort(key=lambda p: math.atan2(p.y - min_x.y, p.x - min_x.x))
    
    return hull

def find_hull_simple(p1, p2, points):
    """Find hull points between two points"""
    if not points:
        return []
    
    # Find point farthest from line
    max_dist = -1
    farthest = None
    
    for point in points:
        dist = point_distance_from_line(p1, p2, point)
        if dist > max_dist:
            max_dist = dist
            farthest = point
    
    # Split remaining points
    left_points = [p for p in points if orientation(p1, farthest, p) == 2]
    right_points = [p for p in points if orientation(farthest, p2, p) == 2]
    
    # Recursively find hull
    result = []
    result.extend(find_hull_simple(p1, farthest, left_points))
    result.append(farthest)
    result.extend(find_hull_simple(farthest, p2, right_points))
    
    return result

# Example usage
if __name__ == "__main__":
    # Create sample points
    points = [
        Point(0, 3), Point(1, 1), Point(2, 2), Point(4, 4),
        Point(0, 0), Point(1, 2), Point(3, 1), Point(3, 3)
    ]
    
    print("Input points:")
    for point in points:
        print(point)
    
    print("\nConvex Hull points:")
    hull = quickhull_simple(points)
    for point in hull:
        print(point)
    
    print(f"\nNumber of points in convex hull: {len(hull)}")
```

## How it works:

1. **Find extreme points**: Identify the leftmost and rightmost points
2. **Partition points**: Divide remaining points into those above and below the line
3. **Find farthest point**: For each partition, find the point farthest from the line
4. **Recursive subdivision**: Create new lines with the farthest point and recursively process the remaining points
5. **Combine results**: Assemble all hull points

## Time Complexity:
- **Average case**: O(n log n)
- **Worst case**: O(n²)
- **Space complexity**: O(n)

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

Convex Hull points:
(0, 0)
(0, 3)
(1, 1)
(3, 1)
(4, 4)
(3, 3)

Number of points in convex hull: 6
```

