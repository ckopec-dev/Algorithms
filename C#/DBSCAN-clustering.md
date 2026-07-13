# DBSCAN Clustering Algorithm in C#

Here's a complete implementation of the DBSCAN clustering algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Point
{
    public double X { get; set; }
    public double Y { get; set; }
    public int ClusterId { get; set; } = -1; // -1 means unclassified
    public bool IsVisited { get; set; } = false;
    
    public Point(double x, double y)
    {
        X = x;
        Y = y;
    }
    
    public double DistanceTo(Point other)
    {
        return Math.Sqrt(Math.Pow(X - other.X, 2) + Math.Pow(Y - other.Y, 2));
    }
}

public class DBSCAN
{
    private List<Point> points;
    private double eps;
    private int minPts;
    
    public DBSCAN(List<Point> points, double eps, int minPts)
    {
        this.points = points;
        this.eps = eps;
        this.minPts = minPts;
    }
    
    public void Cluster()
    {
        int clusterId = 0;
        
        foreach (Point point in points)
        {
            if (point.IsVisited)
                continue;
                
            point.IsVisited = true;
            
            List<Point> neighbors = FindNeighbors(point);
            
            if (neighbors.Count < minPts)
            {
                point.ClusterId = -1; // Noise point
            }
            else
            {
                clusterId++;
                point.ClusterId = clusterId;
                
                // Expand cluster
                ExpandCluster(point, neighbors, clusterId);
            }
        }
    }
    
    private List<Point> FindNeighbors(Point point)
    {
        var neighbors = new List<Point>();
        
        foreach (Point other in points)
        {
            if (point.DistanceTo(other) <= eps)
            {
                neighbors.Add(other);
            }
        }
        
        return neighbors;
    }
    
    private void ExpandCluster(Point point, List<Point> neighbors, int clusterId)
    {
        var seedSet = new Queue<Point>(neighbors);
        
        while (seedSet.Count > 0)
        {
            Point currentPoint = seedSet.Dequeue();
            
            if (!currentPoint.IsVisited)
            {
                currentPoint.IsVisited = true;
                List<Point> currentNeighbors = FindNeighbors(currentPoint);
                
                if (currentNeighbors.Count >= minPts)
                {
                    foreach (Point neighbor in currentNeighbors)
                    {
                        if (!neighbor.IsVisited)
                        {
                            seedSet.Enqueue(neighbor);
                        }
                    }
                }
            }
            
            // Assign cluster ID if not already assigned
            if (currentPoint.ClusterId == -1) // Only assign if it's noise
            {
                currentPoint.ClusterId = clusterId;
            }
        }
    }
    
    public void PrintClusters()
    {
        var clusters = points.GroupBy(p => p.ClusterId)
                           .OrderBy(c => c.Key);
        
        foreach (var cluster in clusters)
        {
            Console.WriteLine($"Cluster {cluster.Key}:");
            foreach (Point point in cluster)
            {
                Console.WriteLine($"  ({point.X}, {point.Y})");
            }
            Console.WriteLine();
        }
    }
    
    public List<Point> GetPoints() => points;
}

// Example usage
class Program
{
    static void Main()
    {
        // Create sample data points
        var points = new List<Point>
        {
            new Point(1, 2),
            new Point(1, 4),
            new Point(1, 0),
            new Point(4, 2),
            new Point(4, 4),
            new Point(4, 0),
            new Point(7, 2),
            new Point(7, 4),
            new Point(7, 0),
            new Point(10, 2),
            new Point(10, 4),
            new Point(10, 0)
        };
        
        // Create DBSCAN instance
        var dbscan = new DBSCAN(points, eps: 2.0, minPts: 2);
        
        // Perform clustering
        dbscan.Cluster();
        
        // Print results
        Console.WriteLine("DBSCAN Clustering Results:");
        Console.WriteLine("==========================");
        dbscan.PrintClusters();
        
        // Count points in each cluster
        var clusters = points.GroupBy(p => p.ClusterId)
                           .OrderBy(c => c.Key);
        
        foreach (var cluster in clusters)
        {
            Console.WriteLine($"Cluster {cluster.Key}: {cluster.Count()} points");
        }
    }
}
```

## Key Features of this Implementation:

### 1. **Point Class**
- Represents data points with X, Y coordinates
- Tracks cluster assignment and visit status
- Includes distance calculation method

### 2. **DBSCAN Class**
- **Constructor**: Takes points, epsilon (eps), and minimum points (minPts)
- **Cluster()**: Main clustering algorithm implementation
- **FindNeighbors()**: Finds all points within epsilon distance
- **ExpandCluster()**: Recursively expands clusters using the DBSCAN algorithm

### 3. **Algorithm Logic**
- **Noise Points**: Points with fewer than minPts neighbors are marked as noise (-1)
- **Core Points**: Points with at least minPts neighbors form clusters
- **Border Points**: Points that are neighbors of core points but don't have enough neighbors themselves
- **Cluster Expansion**: Uses queue-based approach for cluster growth

### 4. **Example Output**
```
DBSCAN Clustering Results:
==========================
Cluster 1:
  (1, 2)
  (1, 4)
  (1, 0)

Cluster 2:
  (4, 2)
  (4, 4)
  (4, 0)

Cluster 3:
  (7, 2)
  (7, 4)
  (7, 0)

Cluster 4:
  (10, 2)
  (10, 4)
  (10, 0)
```

## Parameters Explanation:

- **Epsilon (eps)**: Maximum distance between points to be considered neighbors
- **MinPts**: Minimum number of points required to form a cluster
- **ClusterId**: -1 for noise, positive integers for cluster assignments

This implementation correctly handles the core DBSCAN algorithm with proper handling of core points, border points, and noise points.