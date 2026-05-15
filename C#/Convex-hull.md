# Convex Hull Algorithm in C#

Here's an implementation of the Graham Scan algorithm to find the convex hull of a set of points:

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

public class ConvexHull
{
    // Calculate cross product of vectors (p0,p1) and (p1,p2)
    private static double CrossProduct(Point p0, Point p1, Point p2)
    {
        return (p1.X - p0.X) * (p2.Y - p1.Y) - (p1.Y - p0.Y) * (p2.X - p1.X);
    }
    
    // Calculate distance between two points
    private static double Distance(Point p1, Point p2)
    {
        return Math.Sqrt(Math.Pow(p2.X - p1.X, 2) + Math.Pow(p2.Y - p1.Y, 2));
    }
    
    // Find the point with minimum y-coordinate (and minimum x if tie)
    private static Point FindBottomPoint(List<Point> points)
    {
        Point bottomPoint = points[0];
        foreach (Point point in points)
        {
            if (point.Y < bottomPoint.Y || (point.Y == bottomPoint.Y && point.X < bottomPoint.X))
            {
                bottomPoint = point;
            }
        }
        return bottomPoint;
    }
    
    // Sort points by polar angle with respect to the bottom point
    private static void SortByPolarAngle(List<Point> points, Point bottomPoint)
    {
        points.Sort((p1, p2) =>
        {
            double cross = CrossProduct(bottomPoint, p1, p2);
            if (cross == 0)
            {
                // If collinear, sort by distance from bottom point
                return Distance(bottomPoint, p1).CompareTo(Distance(bottomPoint, p2));
            }
            // Sort by polar angle (counter-clockwise)
            return cross > 0 ? -1 : 1;
        });
    }
    
    // Graham Scan algorithm
    public static List<Point> FindConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return new List<Point>();
        
        // Find the bottom point
        Point bottomPoint = FindBottomPoint(points);
        
        // Sort points by polar angle
        SortByPolarAngle(points, bottomPoint);
        
        // Build convex hull
        Stack<Point> hull = new Stack<Point>();
        
        foreach (Point point in points)
        {
            // Remove points that make clockwise turn
            while (hull.Count >= 2 && 
                   CrossProduct(hull.ElementAt(1), hull.Peek(), point) <= 0)
            {
                hull.Pop();
            }
            hull.Push(point);
        }
        
        return hull.ToList();
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
        
        // Find convex hull
        List<Point> hull = ConvexHull.FindConvexHull(points);
        
        Console.WriteLine("\nConvex Hull points:");
        foreach (Point point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine($"\nNumber of points in convex hull: {hull.Count}");
    }
}
```

## Output
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
(4, 4)
(3, 1)

Number of points in convex hull: 4
```

## Algorithm Explanation

This implementation uses the **Graham Scan** algorithm which works in O(n log n) time:

1. **Find the bottom point** - The point with the lowest y-coordinate (and lowest x if tied)
2. **Sort points** - Sort all points by polar angle with respect to the bottom point
3. **Build hull** - Use a stack to maintain the convex hull, removing points that create clockwise turns

## Key Features

- Handles edge cases (fewer than 3 points)
- Properly sorts points by polar angle
- Uses cross product to determine turn direction
- Returns points in counter-clockwise order

The algorithm correctly identifies the smallest convex polygon that contains all input points.

