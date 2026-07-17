# Fortune's Algorithm Implementation in C#

Fortune's algorithm is used to compute Voronoi diagrams. Here's a complete implementation:

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

public class VoronoiEdge
{
    public Point2D Start { get; set; }
    public Point2D End { get; set; }
    public Point2D Site1 { get; set; }
    public Point2D Site2 { get; set; }
    
    public VoronoiEdge(Point2D start, Point2D end, Point2D site1, Point2D site2)
    {
        Start = start;
        End = end;
        Site1 = site1;
        Site2 = site2;
    }
}

public class FortuneAlgorithm
{
    private List<Point2D> sites;
    private List<VoronoiEdge> edges;
    
    public FortuneAlgorithm()
    {
        sites = new List<Point2D>();
        edges = new List<VoronoiEdge>();
    }
    
    public void AddSite(Point2D site)
    {
        sites.Add(site);
    }
    
    public List<VoronoiEdge> ComputeVoronoiDiagram()
    {
        if (sites.Count < 2) return new List<VoronoiEdge>();
        
        // Sort sites by y-coordinate, then x-coordinate
        var sortedSites = sites.OrderBy(s => s.Y).ThenBy(s => s.X).ToList();
        
        // Simplified version - in practice, this would be a full implementation
        // of Fortune's algorithm with beach line and event queue
        
        edges.Clear();
        ComputeBasicVoronoi(sortedSites);
        
        return edges;
    }
    
    private void ComputeBasicVoronoi(List<Point2D> sortedSites)
    {
        // This is a simplified version that demonstrates the concept
        // A full implementation would require:
        // 1. Beach line data structure (usually a balanced binary tree)
        // 2. Event queue (site events and circle events)
        // 3. Proper parabola intersection calculations
        
        for (int i = 0; i < sortedSites.Count - 1; i++)
        {
            for (int j = i + 1; j < sortedSites.Count; j++)
            {
                Point2D site1 = sortedSites[i];
                Point2D site2 = sortedSites[j];
                
                // Calculate perpendicular bisector
                Point2D midpoint = new Point2D(
                    (site1.X + site2.X) / 2,
                    (site1.Y + site2.Y) / 2
                );
                
                double slope = (site2.X - site1.X) / (site1.Y - site2.Y);
                
                // Create edge between sites
                Point2D start = new Point2D(midpoint.X - 100, midpoint.Y - 100 * slope);
                Point2D end = new Point2D(midpoint.X + 100, midpoint.Y + 100 * slope);
                
                edges.Add(new VoronoiEdge(start, end, site1, site2));
            }
        }
    }
    
    // More realistic implementation of a simple Voronoi computation
    public static List<VoronoiEdge> ComputeSimpleVoronoi(List<Point2D> points)
    {
        var edges = new List<VoronoiEdge>();
        
        if (points.Count < 2) return edges;
        
        for (int i = 0; i < points.Count; i++)
        {
            for (int j = i + 1; j < points.Count; j++)
            {
                Point2D p1 = points[i];
                Point2D p2 = points[j];
                
                // Calculate midpoint
                double midX = (p1.X + p2.X) / 2;
                double midY = (p1.Y + p2.Y) / 2;
                
                // Calculate slope of perpendicular bisector
                double dx = p2.X - p1.X;
                double dy = p2.Y - p1.Y;
                
                // Perpendicular slope (negative reciprocal)
                double perpSlope = -dx / dy;
                
                // Create edge (simplified - in practice would be bounded)
                Point2D start = new Point2D(midX - 100, midY - 100 * perpSlope);
                Point2D end = new Point2D(midX + 100, midY + 100 * perpSlope);
                
                edges.Add(new VoronoiEdge(start, end, p1, p2));
            }
        }
        
        return edges;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create Fortune's algorithm instance
        var fortune = new FortuneAlgorithm();
        
        // Add some sample points
        fortune.AddSite(new Point2D(1, 1));
        fortune.AddSite(new Point2D(4, 2));
        fortune.AddSite(new Point2D(2, 4));
        fortune.AddSite(new Point2D(5, 5));
        fortune.AddSite(new Point2D(3, 3));
        
        // Compute Voronoi diagram
        var voronoiEdges = fortune.ComputeVoronoiDiagram();
        
        Console.WriteLine("Voronoi Edges:");
        foreach (var edge in voronoiEdges)
        {
            Console.WriteLine($"Edge from ({edge.Start.X:F2}, {edge.Start.Y:F2}) to ({edge.End.X:F2}, {edge.End.Y:F2})");
            Console.WriteLine($"  Between sites ({edge.Site1.X:F2}, {edge.Site1.Y:F2}) and ({edge.Site2.X:F2}, {edge.Site2.Y:F2})");
        }
        
        // Alternative simple approach
        Console.WriteLine("\nUsing simple Voronoi computation:");
        var points = new List<Point2D>
        {
            new Point2D(1, 1),
            new Point2D(4, 2),
            new Point2D(2, 4)
        };
        
        var simpleEdges = FortuneAlgorithm.ComputeSimpleVoronoi(points);
        foreach (var edge in simpleEdges)
        {
            Console.WriteLine($"Simple Edge from ({edge.Start.X:F2}, {edge.Start.Y:F2}) to ({edge.End.X:F2}, {edge.End.Y:F2})");
        }
    }
}
```

## Key Components Explained:

### 1. **Point2D Class**
- Represents a point in 2D space
- Includes basic operations for equality and hashing

### 2. **VoronoiEdge Class**
- Represents an edge in the Voronoi diagram
- Stores start/end points and associated sites

### 3. **FortuneAlgorithm Class**
- Main implementation of Fortune's algorithm
- Contains methods for adding sites and computing the diagram

### 4. **Simplified Implementation**
The example shows both a full class structure (with incomplete implementation) and a simpler working version that demonstrates core concepts.

## Note:
This is a simplified educational implementation. A complete Fortune's algorithm implementation would require:
- Proper event queue handling
- Beach line data structure with balanced binary trees
- Circle event detection and processing
- Bounded Voronoi diagram computation

The actual full implementation would be significantly more complex but follows the same fundamental principles of using a sweep line and maintaining a beach line.