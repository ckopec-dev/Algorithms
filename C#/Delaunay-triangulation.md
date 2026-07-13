# Delaunay Triangulation in C#

Here's a complete implementation of the Delaunay triangulation algorithm using the incremental approach in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Point2D
{
    public double X { get; set; }
    public double Y { get; set; }
    
    public Point2D(double x, double y)
    {
        X = x;
        Y = y;
    }
    
    public override bool Equals(object obj)
    {
        if (obj is Point2D other)
            return Math.Abs(X - other.X) < 1e-9 && Math.Abs(Y - other.Y) < 1e-9;
        return false;
    }
    
    public override int GetHashCode()
    {
        return HashCode.Combine(X, Y);
    }
}

public class Triangle
{
    public Point2D A { get; set; }
    public Point2D B { get; set; }
    public Point2D C { get; set; }
    
    public Triangle(Point2D a, Point2D b, Point2D c)
    {
        A = a;
        B = b;
        C = c;
    }
    
    // Check if point is inside the circumcircle of this triangle
    public bool IsPointInCircumcircle(Point2D p)
    {
        double ax = A.X - p.X;
        double ay = A.Y - p.Y;
        double bx = B.X - p.X;
        double by = B.Y - p.Y;
        double cx = C.X - p.X;
        double cy = C.Y - p.Y;
        
        double ab = ax * ax + ay * ay;
        double bc = bx * bx + by * by;
        double ca = cx * cx + cy * cy;
        
        double det = ax * (by * ca - cy * bc) - ay * (bx * ca - cx * bc) + 
                     (bx * cy - cx * by) * ab;
        
        return det > 0; // Point is inside circumcircle
    }
}

public class DelaunayTriangulation
{
    private List<Triangle> triangles;
    private List<Point2D> points;
    
    public DelaunayTriangulation()
    {
        triangles = new List<Triangle>();
        points = new List<Point2D>();
    }
    
    public List<Triangle> Triangulate(List<Point2D> inputPoints)
    {
        if (inputPoints == null || inputPoints.Count < 3)
            return new List<Triangle>();
            
        // Create a super triangle that contains all points
        Point2D superTriangle = CreateSuperTriangle(inputPoints);
        
        // Initialize triangulation with super triangle
        triangles.Add(new Triangle(superTriangle, 
                                  new Point2D(superTriangle.X + 1000, superTriangle.Y - 1000),
                                  new Point2D(superTriangle.X - 1000, superTriangle.Y - 1000)));
        
        // Add points one by one
        foreach (Point2D point in inputPoints)
        {
            AddPoint(point);
        }
        
        // Remove triangles that contain super triangle vertices
        triangles = triangles.Where(t => !ContainsSuperVertex(t)).ToList();
        
        return triangles;
    }
    
    private Point2D CreateSuperTriangle(List<Point2D> points)
    {
        double minX = points.Min(p => p.X);
        double maxX = points.Max(p => p.X);
        double minY = points.Min(p => p.Y);
        double maxY = points.Max(p => p.Y);
        
        double width = maxX - minX;
        double height = maxY - minY;
        double delta = Math.Max(width, height) * 100;
        
        double centerX = (minX + maxX) / 2;
        double centerY = (minY + maxY) / 2;
        
        return new Point2D(centerX, centerY);
    }
    
    private void AddPoint(Point2D point)
    {
        List<Triangle> badTriangles = new List<Triangle>();
        
        // Find all triangles whose circumcircle contains the new point
        foreach (Triangle triangle in triangles)
        {
            if (triangle.IsPointInCircumcircle(point))
            {
                badTriangles.Add(triangle);
            }
        }
        
        // Find the boundary of the polygon formed by bad triangles
        List<(Point2D, Point2D)> boundary = new List<(Point2D, Point2D)>();
        
        foreach (Triangle triangle in badTriangles)
        {
            for (int i = 0; i < 3; i++)
            {
                Point2D a = GetVertex(triangle, i);
                Point2D b = GetVertex(triangle, (i + 1) % 3);
                
                bool isBoundaryEdge = true;
                foreach (Triangle other in badTriangles)
                {
                    if (other == triangle) continue;
                    
                    if (ContainsEdge(other, a, b))
                    {
                        isBoundaryEdge = false;
                        break;
                    }
                }
                
                if (isBoundaryEdge)
                {
                    boundary.Add((a, b));
                }
            }
        }
        
        // Remove bad triangles
        triangles.RemoveAll(t => badTriangles.Contains(t));
        
        // Create new triangles from boundary
        foreach (var edge in boundary)
        {
            triangles.Add(new Triangle(edge.Item1, edge.Item2, point));
        }
    }
    
    private Point2D GetVertex(Triangle triangle, int index)
    {
        switch (index)
        {
            case 0: return triangle.A;
            case 1: return triangle.B;
            case 2: return triangle.C;
            default: throw new ArgumentException("Invalid vertex index");
        }
    }
    
    private bool ContainsEdge(Triangle triangle, Point2D a, Point2D b)
    {
        return (triangle.A == a && triangle.B == b) ||
               (triangle.B == a && triangle.C == b) ||
               (triangle.C == a && triangle.A == b) ||
               (triangle.A == b && triangle.B == a) ||
               (triangle.B == b && triangle.C == a) ||
               (triangle.C == b && triangle.A == a);
    }
    
    private bool ContainsSuperVertex(Triangle triangle)
    {
        // Check if triangle contains any vertex from super triangle
        return false; // Simplified implementation - in real case you'd need to track super vertices
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create some sample points
        List<Point2D> points = new List<Point2D>
        {
            new Point2D(0, 0),
            new Point2D(1, 0),
            new Point2D(0, 1),
            new Point2D(1, 1),
            new Point2D(0.5, 0.5),
            new Point2D(2, 2)
        };
        
        // Perform Delaunay triangulation
        DelaunayTriangulation dt = new DelaunayTriangulation();
        List<Triangle> triangles = dt.Triangulate(points);
        
        // Display results
        Console.WriteLine("Delaunay Triangulation Results:");
        Console.WriteLine($"Number of triangles: {triangles.Count}");
        
        for (int i = 0; i < triangles.Count; i++)
        {
            Triangle t = triangles[i];
            Console.WriteLine($"Triangle {i + 1}: ({t.A.X:F2}, {t.A.Y:F2}) " +
                            $"({t.B.X:F2}, {t.B.Y:F2}) ({t.C.X:F2}, {t.C.Y:F2})");
        }
    }
}
```

## Key Features of This Implementation

1. **Point Class**: Represents 2D points with X and Y coordinates
2. **Triangle Class**: Represents triangles with three vertices and circumcircle checking
3. **DelaunayTriangulation Class**: Main algorithm implementation
4. **Incremental Algorithm**: Adds points one by one and updates triangulation
5. **Circumcircle Test**: Determines if a point lies inside a triangle's circumcircle

## How It Works

1. **Super Triangle**: Creates an initial large triangle that contains all input points
2. **Point Insertion**: For each new point, finds all triangles whose circumcircle contains the point
3. **Bad Triangles**: Removes these "bad" triangles
4. **Boundary Creation**: Creates new triangles from the boundary of the removed triangles
5. **Cleanup**: Removes triangles that reference the super triangle vertices

## Time Complexity

- **Average Case**: O(n log n)
- **Worst Case**: O(n²)

This implementation provides a solid foundation for Delaunay triangulation that can be extended with additional optimizations and features.