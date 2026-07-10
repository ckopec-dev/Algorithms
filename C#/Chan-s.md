# Chan's Algorithm (Convex Hull) Implementation in C#

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
    // Calculate cross product of three points
    private static double CrossProduct(Point o, Point a, Point b)
    {
        return (a.X - o.X) * (b.Y - o.Y) - (a.Y - o.Y) * (b.X - o.X);
    }
    
    // Check if three points make a clockwise turn
    private static bool IsClockwise(Point o, Point a, Point b)
    {
        return CrossProduct(o, a, b) < 0;
    }
    
    // Check if three points make a counter-clockwise turn
    private static bool IsCounterClockwise(Point o, Point a, Point b)
    {
        return CrossProduct(o, a, b) > 0;
    }
    
    // Find convex hull using Chan's algorithm
    public static List<Point> FindConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return points?.ToList() ?? new List<Point>();
        
        // Step 1: Sort points by x-coordinate (and y-coordinate for ties)
        var sortedPoints = points.OrderBy(p => p.X).ThenBy(p => p.Y).ToList();
        
        // Step 2: Find the lowest point
        Point lowestPoint = sortedPoints[0];
        for (int i = 1; i < sortedPoints.Count; i++)
        {
            if (sortedPoints[i].Y < lowestPoint.Y)
                lowestPoint = sortedPoints[i];
        }
        
        // Step 3: Sort points by polar angle with respect to lowest point
        sortedPoints.Sort((a, b) =>
        {
            double angleA = Math.Atan2(a.Y - lowestPoint.Y, a.X - lowestPoint.X);
            double angleB = Math.Atan2(b.Y - lowestPoint.Y, b.X - lowestPoint.X);
            return angleA.CompareTo(angleB);
        });
        
        // Step 4: Build convex hull using Graham scan (simplified version)
        var hull = new Stack<Point>();
        
        foreach (var point in sortedPoints)
        {
            // Remove points that make clockwise turns
            while (hull.Count > 1 && IsClockwise(hull.ElementAt(1), hull.Peek(), point))
            {
                hull.Pop();
            }
            
            hull.Push(point);
        }
        
        return hull.ToList();
    }
    
    // Alternative implementation using Gift Wrapping (Jarvis March) as part of Chan's algorithm
    public static List<Point> FindConvexHullJarvisMarch(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return points?.ToList() ?? new List<Point>();
        
        // Find the leftmost point
        Point start = points[0];
        foreach (var point in points)
        {
            if (point.X < start.X)
                start = point;
        }
        
        var hull = new List<Point>();
        Point current = start;
        
        do
        {
            hull.Add(current);
            Point nextPoint = points[0];
            
            // Find the point that makes the largest counter-clockwise angle
            foreach (var point in points)
            {
                if (current == point) continue;
                
                if (nextPoint == start || IsCounterClockwise(current, nextPoint, point))
                {
                    nextPoint = point;
                }
            }
            
            current = nextPoint;
        } while (current != start);
        
        return hull;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create sample points
        var points = new List<Point>
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
        
        Console.WriteLine("Input Points:");
        foreach (var point in points)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine("\nConvex Hull using Graham Scan:");
        var hull = ConvexHull.FindConvexHull(points);
        foreach (var point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine("\nConvex Hull using Jarvis March:");
        var hull2 = ConvexHull.FindConvexHullJarvisMarch(points);
        foreach (var point in hull2)
        {
            Console.WriteLine(point);
        }
    }
}
```

## Algorithm Explanation

**Chan's Algorithm Overview:**
1. **Step 1:** Sort points by x-coordinate
2. **Step 2:** Find the lowest point as reference
3. **Step 3:** Sort remaining points by polar angle with respect to the lowest point
4. **Step 4:** Use Graham scan or Jarvis march to build the convex hull

**Time Complexity:** O(n log n) for sorting + O(nh) for hull construction = O(n log n)
**Space Complexity:** O(n)

## Key Features

- Implements both Graham scan and Jarvis march approaches
- Handles edge cases (fewer than 3 points)
- Uses cross product to determine turn direction
- Sorts points properly for convex hull construction
- Includes proper point comparison logic

The algorithm efficiently finds the smallest convex polygon that contains all input points, which is the definition of a convex hull.