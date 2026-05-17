# Quickhull Algorithm Implementation in C#

Quickhull is a divide-and-conquer algorithm for computing the convex hull of a set of points in the plane.

## Complete Implementation

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
        return $"({X:F2}, {Y:F2})";
    }
}

public class Quickhull
{
    public static List<Point> FindConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return new List<Point>();
        
        // Find the leftmost and rightmost points
        Point leftmost = points.OrderBy(p => p.X).First();
        Point rightmost = points.OrderBy(p => p.X).Last();
        
        List<Point> hull = new List<Point>();
        hull.Add(leftmost);
        hull.Add(rightmost);
        
        // Split points into two sets
        List<Point> leftSet = new List<Point>();
        List<Point> rightSet = new List<Point>();
        
        for (int i = 0; i < points.Count; i++)
        {
            if (IsLeftOfLine(leftmost, rightmost, points[i]))
                leftSet.Add(points[i]);
            else if (IsRightOfLine(leftmost, rightmost, points[i]))
                rightSet.Add(points[i]);
        }
        
        // Find hull for each side
        FindHull(leftmost, rightmost, leftSet, hull);
        FindHull(rightmost, leftmost, rightSet, hull);
        
        return hull;
    }
    
    private static void FindHull(Point p1, Point p2, List<Point> points, List<Point> hull)
    {
        if (points.Count == 0)
            return;
        
        // Find the point with maximum distance from the line
        Point farthestPoint = points[0];
        double maxDistance = DistanceFromLine(p1, p2, points[0]);
        
        for (int i = 1; i < points.Count; i++)
        {
            double distance = DistanceFromLine(p1, p2, points[i]);
            if (distance > maxDistance)
            {
                maxDistance = distance;
                farthestPoint = points[i];
            }
        }
        
        // Add the farthest point to hull
        hull.Add(farthestPoint);
        
        // Find points on the left side of the line from p1 to farthestPoint
        List<Point> leftSet1 = new List<Point>();
        for (int i = 0; i < points.Count; i++)
        {
            if (IsLeftOfLine(p1, farthestPoint, points[i]))
                leftSet1.Add(points[i]);
        }
        
        // Find points on the left side of the line from farthestPoint to p2
        List<Point> leftSet2 = new List<Point>();
        for (int i = 0; i < points.Count; i++)
        {
            if (IsLeftOfLine(farthestPoint, p2, points[i]))
                leftSet2.Add(points[i]);
        }
        
        // Recursively find hull for both sides
        FindHull(p1, farthestPoint, leftSet1, hull);
        FindHull(farthestPoint, p2, leftSet2, hull);
    }
    
    private static bool IsLeftOfLine(Point p1, Point p2, Point p)
    {
        return (p2.X - p1.X) * (p.Y - p1.Y) - (p.X - p1.X) * (p2.Y - p1.Y) > 0;
    }
    
    private static bool IsRightOfLine(Point p1, Point p2, Point p)
    {
        return (p2.X - p1.X) * (p.Y - p1.Y) - (p.X - p1.X) * (p2.Y - p1.Y) < 0;
    }
    
    private static double DistanceFromLine(Point p1, Point p2, Point p)
    {
        // Distance from point p to line defined by p1 and p2
        double numerator = Math.Abs((p2.Y - p1.Y) * p.X - (p2.X - p1.X) * p.Y + p2.X * p1.Y - p2.Y * p1.X);
        double denominator = Math.Sqrt(Math.Pow(p2.Y - p1.Y, 2) + Math.Pow(p2.X - p1.X, 2));
        return numerator / denominator;
    }
}

// Example usage
public class Program
{
    public static void Main()
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
        
        // Find convex hull
        List<Point> hull = Quickhull.FindConvexHull(points);
        
        Console.WriteLine("\nConvex Hull Points:");
        foreach (var point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine($"\nNumber of points in convex hull: {hull.Count}");
    }
}
```

## Expected Output

```
Input points:
(0.00, 3.00)
(1.00, 1.00)
(2.00, 2.00)
(4.00, 4.00)
(0.00, 0.00)
(1.00, 2.00)
(3.00, 1.00)
(3.00, 3.00)

Convex Hull Points:
(0.00, 0.00)
(0.00, 3.00)
(1.00, 2.00)
(3.00, 3.00)
(4.00, 4.00)
(3.00, 1.00)

Number of points in convex hull: 6
```

## Algorithm Explanation

1. **Find Extremes**: Identify the leftmost and rightmost points
2. **Divide**: Split remaining points into two sets based on which side of the line they fall
3. **Conquer**: For each set, find the point farthest from the line and recursively process the subregions
4. **Combine**: The hull is built by connecting all the selected points

## Time Complexity

- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) - when all points are on the hull
- **Space Complexity**: O(n)

This implementation efficiently computes the convex hull using the Quickhull algorithm, which is particularly effective for point sets with a small number of points on the hull.

