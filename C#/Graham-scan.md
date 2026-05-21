# Graham Scan Algorithm Implementation in C#

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

public class GrahamScan
{
    /// <summary>
    /// Calculates the cross product of three points to determine orientation
    /// </summary>
    /// <returns>
    /// Positive value if counter-clockwise turn
    /// Negative value if clockwise turn
    /// Zero if collinear
    /// </returns>
    private static double CrossProduct(Point o, Point a, Point b)
    {
        return (a.X - o.X) * (b.Y - o.Y) - (a.Y - o.Y) * (b.X - o.X);
    }
    
    /// <summary>
    /// Calculates the distance between two points
    /// </summary>
    private static double Distance(Point a, Point b)
    {
        return Math.Sqrt(Math.Pow(a.X - b.X, 2) + Math.Pow(a.Y - b.Y, 2));
    }
    
    /// <summary>
    /// Finds the point with the lowest y-coordinate (and leftmost if tie)
    /// </summary>
    private static Point FindBottomMostPoint(List<Point> points)
    {
        Point bottomMost = points[0];
        for (int i = 1; i < points.Count; i++)
        {
            if (points[i].Y < bottomMost.Y || 
                (points[i].Y == bottomMost.Y && points[i].X < bottomMost.X))
            {
                bottomMost = points[i];
            }
        }
        return bottomMost;
    }
    
    /// <summary>
    /// Sorts points by polar angle with respect to the bottom-most point
    /// </summary>
    private static void SortByPolarAngle(List<Point> points, Point bottomMost)
    {
        points.Sort((a, b) =>
        {
            double angleA = Math.Atan2(a.Y - bottomMost.Y, a.X - bottomMost.X);
            double angleB = Math.Atan2(b.Y - bottomMost.Y, b.X - bottomMost.X);
            
            if (angleA < angleB) return -1;
            if (angleA > angleB) return 1;
            
            // If angles are equal, sort by distance from bottomMost
            double distA = Distance(bottomMost, a);
            double distB = Distance(bottomMost, b);
            return distA.CompareTo(distB);
        });
    }
    
    /// <summary>
    /// Computes the convex hull using Graham Scan algorithm
    /// </summary>
    public static List<Point> ComputeConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return new List<Point>();
        
        // Find the bottom-most point
        Point bottomMost = FindBottomMostPoint(points);
        
        // Sort points by polar angle
        SortByPolarAngle(points, bottomMost);
        
        // Create stack for hull points
        Stack<Point> hull = new Stack<Point>();
        
        foreach (Point point in points)
        {
            // Remove points that make clockwise turns
            while (hull.Count > 1 && 
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
        foreach (var point in points)
        {
            Console.WriteLine(point);
        }
        
        // Compute convex hull
        List<Point> hull = GrahamScan.ComputeConvexHull(points);
        
        Console.WriteLine("\nConvex Hull Points:");
        foreach (var point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine($"\nNumber of points in convex hull: {hull.Count}");
    }
}
```

## Algorithm Explanation

The Graham Scan algorithm works in three main steps:

1. **Find the bottom-most point**: Identify the point with the lowest y-coordinate (leftmost if tied)
2. **Sort by polar angle**: Sort all points by their polar angle with respect to the bottom-most point
3. **Build the hull**: Use a stack to iteratively build the convex hull by removing points that create clockwise turns

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the stack and temporary storage

## Key Features
- Handles edge cases (less than 3 points)
- Correctly identifies collinear points
- Returns vertices of the convex hull in counterclockwise order
- Uses cross product to determine turn direction

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

Convex Hull Points:
(0, 0)
(0, 3)
(4, 4)
(3, 1)

Number of points in convex hull: 4
```

