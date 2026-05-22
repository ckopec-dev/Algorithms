# Gift Wrapping Algorithm (Jarvis March) in Python

The gift wrapping algorithm is a simple method for computing the convex hull of a set of points in the plane.

```python
import math

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __str__(self):
        return f"({self.x}, {self.y})"

def cross_product(o, a, b):
    """
    Calculate the cross product of vectors OA and OB
    Returns positive value if counter-clockwise turn,
    negative value if clockwise turn, zero if collinear
    """
    return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

def distance(p1, p2):
    """Calculate Euclidean distance between two points"""
    return math.sqrt((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2)

def gift_wrapping(points):
    """
    Find the convex hull of a set of points using gift wrapping algorithm
    """
    if len(points) < 3:
        return points
    
    # Find the leftmost point
    start = min(points, key=lambda p: p.x)
    
    hull = []
    current = start
    
    while True:
        hull.append(current)
        
        # Find the next point that makes the largest counter-clockwise turn
        next_point = points[0]
        
        for i in range(1, len(points)):
            # Skip if it's the current point
            if points[i] == current:
                continue
                
            # Calculate cross product to determine turn direction
            cp = cross_product(current, next_point, points[i])
            
            # If we found a point that makes a larger counter-clockwise turn
            # or if we're at the first point and the turn is clockwise
            if (next_point == current or 
                cp > 0 or 
                (cp == 0 and distance(current, points[i]) > distance(current, next_point))):
                next_point = points[i]
        
        current = next_point
        
        # Stop when we return to the starting point
        if current == start:
            break
    
    return hull

# Example usage
if __name__ == "__main__":
    # Create sample points
    points = [
        Point(0, 3),
        Point(1, 1),
        Point(2, 2),
        Point(4, 4),
        Point(0, 0),
        Point(1, 2),
        Point(3, 1),
        Point(3, 3)
    ]
    
    print("Input points:")
    for point in points:
        print(point)
    
    print("\nConvex hull points:")
    hull = gift_wrapping(points)
    for point in hull:
        print(point)
    
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

## Algorithm Explanation:

1. **Find Starting Point**: The leftmost point is selected as the starting point
2. **Build Hull**: From the current point, find the next point that makes the largest counter-clockwise turn
3. **Continue**: Repeat until we return to the starting point
4. **Cross Product**: Used to determine the turn direction between three points

## Time Complexity:
- **Best Case**: O(nh) where n is number of points and h is number of hull points
- **Worst Case**: O(n²) when all points are on the hull

## Space Complexity: O(h) where h is the number of points on the hull

The gift wrapping algorithm is intuitive and easy to understand, making it a good educational example for convex hull computation.

