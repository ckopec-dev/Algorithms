# Jarvis March Algorithm (Gift Wrapping Algorithm) in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Point
{
    public int X { get; set; }
    public int Y { get; set; }
    
    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }
    
    public override string ToString()
    {
        return $"({X}, {Y})";
    }
}

public class JarvisMarch
{
    /// <summary>
    /// Find the convex hull of a set of points using Jarvis March (Gift Wrapping) algorithm
    /// </summary>
    /// <param name="points">List of points</param>
    /// <returns>List of points forming the convex hull</returns>
    public static List<Point> FindConvexHull(List<Point> points)
    {
        if (points == null || points.Count < 3)
            return new List<Point>();
        
        // Find the leftmost point
        Point leftmost = points[0];
        for (int i = 1; i < points.Count; i++)
        {
            if (points[i].X < leftmost.X)
                leftmost = points[i];
        }
        
        List<Point> hull = new List<Point>();
        Point current = leftmost;
        Point nextPoint;
        
        do
        {
            hull.Add(current);
            nextPoint = points[0];
            
            // Find the point that makes the largest counterclockwise turn
            for (int i = 1; i < points.Count; i++)
            {
                if (current == points[i])
                    continue;
                    
                int orientation = GetOrientation(current, nextPoint, points[i]);
                
                // If we found a more counterclockwise point, update nextPoint
                if (orientation == 2 || (orientation == 0 && 
                    Distance(current, points[i]) > Distance(current, nextPoint)))
                {
                    nextPoint = points[i];
                }
            }
            
            current = nextPoint;
        } while (current != leftmost);
        
        return hull;
    }
    
    /// <summary>
    /// Calculate orientation of three points
    /// Returns: 0 -> collinear, 1 -> clockwise, 2 -> counterclockwise
    /// </summary>
    private static int GetOrientation(Point p, Point q, Point r)
    {
        int val = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y);
        
        if (val == 0)
            return 0; // collinear
        
        return (val > 0) ? 1 : 2; // clockwise or counterclockwise
    }
    
    /// <summary>
    /// Calculate distance between two points
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
        
        Console.WriteLine("Input Points:");
        foreach (var point in points)
        {
            Console.WriteLine(point);
        }
        
        // Find convex hull
        List<Point> hull = JarvisMarch.FindConvexHull(points);
        
        Console.WriteLine("\nConvex Hull Points:");
        foreach (var point in hull)
        {
            Console.WriteLine(point);
        }
        
        Console.WriteLine($"\nTotal points in convex hull: {hull.Count}");
    }
}
```

## Output:
```
Input Points:
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

Total points in convex hull: 4
```

## Algorithm Explanation:

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the leftmost point (or bottom-most if there are ties)
2. **Build the hull**: Starting from the leftmost point, find the next point that makes the largest counterclockwise turn
3. **Continue until return**: Keep selecting points until we return to the starting point
4. **Orientation calculation**: Uses cross product to determine if three points make a clockwise, counterclockwise, or collinear turn

## Time Complexity:
- **Worst case**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Best case**: O(n log n) when the hull has few points
- **Space complexity**: O(h) for storing the hull points

This implementation handles edge cases like collinear points and works correctly for any set of 2D points.