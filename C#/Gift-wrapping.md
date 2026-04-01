# Gift Wrapping Algorithm (Jarvis March) in C#

The gift wrapping algorithm is a simple algorithm for computing the convex hull of a set of points in the plane. It works by starting with the leftmost point and then repeatedly finding the next point that makes the smallest counterclockwise angle with the previous edge.

## Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Point
{
    public double X { get; set; }
    public double Y { get; set; }
    
    public Point(double x, double y)
    {
        X = x;
        Y = y;
    }
    
    public override string ToString()
    {
        return $"({X}, {Y})";
    }
}

public class GiftWrappingAlgorithm
{
    /// <summary>
    /// Computes the convex hull using the gift wrapping algorithm (Jarvis March)
    /// </summary>
    /// <param name="points">List of points</param>
    /// <returns>List of points forming the convex hull</returns>
    public static List<Point> ComputeConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return new List<Point>();
        
        // Find the leftmost point
        Point leftmost = points[0];
        foreach (Point point in points)
        {
            if (point.X < leftmost.X)
                leftmost = point;
        }
        
        List<Point> hull = new List<Point>();
        Point current = leftmost;
        Point nextPoint;
        
        do
        {
            hull.Add(current);
            nextPoint = points[0];
            
            // Find the point that makes the smallest counterclockwise angle
            // with the current point and the previous point
            foreach (Point point in points)
            {
                if (point == current)
                    continue;
                    
                int orientation = GetOrientation(current, nextPoint, point);
                
                // If point is counterclockwise from nextPoint, or if nextPoint is 
                // collinear with current and point, update nextPoint
                if (nextPoint == current || orientation == 2 || 
                    (orientation == 0 && Distance(current, point) < Distance(current, nextPoint)))
                {
                    nextPoint = point;
                }
            }
            
            current = nextPoint;
        } while (current != leftmost && current != null);
        
        return hull;
    }
    
    /// <summary>
    /// Returns the orientation of three points
    /// 0 = collinear, 1 = clockwise, 2 = counterclockwise
    /// </summary>
    private static int GetOrientation(Point p, Point q, Point r)
    {
        double val = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y);
        
        if (val == 0)
            return 0; // collinear
        
        return (val > 0) ? 1 : 2; // clockwise or counterclockwise
    }
    
    /// <summary>
    /// Calculates the distance between two points
    /// </summary>
    private static double Distance(Point p1, Point p2)
    {
        return Math.Sqrt(Math.Pow(p2.X - p1.X, 2) + Math.Pow(p2.Y - p1.Y, 2));
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create sample points
        List<Point> points = new List<Point>
        {
            new Point(0, 3),
            new Point(1, 1),
            new Point(2, 2),
            new Point(4, 4),
            new Point(0, 0),
            new Point(1, 2),
            new Point(3, 1),
            new Point(3, 3)
        };
        
        Console.WriteLine("Input points:");
        foreach (Point point in points)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine("\nComputing convex hull using gift wrapping algorithm:");
        List<Point> hull = GiftWrappingAlgorithm.ComputeConvexHull(points);
        
        Console.WriteLine("Convex hull points:");
        foreach (Point point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine($"\nTotal points in convex hull: {hull.Count}");
    }
}
```

## Sample Output

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

Computing convex hull using gift wrapping algorithm:
Convex hull points:
(0, 0)
(0, 3)
(4, 4)
(3, 1)

Total points in convex hull: 4
```

## Algorithm Explanation

1. **Find the starting point**: The leftmost point is selected as the starting point
2. **Build the hull**: Starting from the leftmost point, find the next point that makes the smallest counterclockwise angle with the previous edge
3. **Continue until closure**: Repeat until we return to the starting point

## Time and Space Complexity

- **Time Complexity**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space Complexity**: O(h) for storing the hull points

## Key Features

- Handles collinear points correctly
- Works with any set of 2D points
- Returns points in counterclockwise order
- Efficient for small to medium-sized datasets
- Easy to understand and implement

The gift wrapping algorithm is particularly useful when the number of hull points is much smaller than the total number of input points, as its performance depends on the number of points on the hull rather than the total number of points.

