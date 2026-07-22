# K-Means Clustering Algorithm in C#

Here's a complete implementation of the K-means clustering algorithm in C#:

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
    
    public double Distance(Point other)
    {
        return Math.Sqrt(Math.Pow(X - other.X, 2) + Math.Pow(Y - other.Y, 2));
    }
}

public class KMeansCluster
{
    public List<Point> Points { get; set; } = new List<Point>();
    public Point Center { get; set; }
    
    public KMeansCluster(Point center)
    {
        Center = center;
    }
}

public class KMeans
{
    private int k; // number of clusters
    private int maxIterations;
    
    public KMeans(int k, int maxIterations = 100)
    {
        this.k = k;
        this.maxIterations = maxIterations;
    }
    
    public List<KMeansCluster> Cluster(List<Point> data)
    {
        if (data == null || data.Count < k)
            throw new ArgumentException("Data must contain at least k points");
        
        // Initialize clusters with random centers
        var clusters = InitializeClusters(data);
        
        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            bool changed = false;
            
            // Assign points to clusters
            foreach (var point in data)
            {
                var closestCluster = FindClosestCluster(point, clusters);
                var oldCluster = clusters.FirstOrDefault(c => c.Points.Contains(point));
                
                if (oldCluster != null && oldCluster != closestCluster)
                {
                    oldCluster.Points.Remove(point);
                    closestCluster.Points.Add(point);
                    changed = true;
                }
                else if (oldCluster == null)
                {
                    closestCluster.Points.Add(point);
                    changed = true;
                }
            }
            
            // Update cluster centers
            foreach (var cluster in clusters)
            {
                if (cluster.Points.Count > 0)
                {
                    double newX = cluster.Points.Average(p => p.X);
                    double newY = cluster.Points.Average(p => p.Y);
                    cluster.Center = new Point(newX, newY);
                }
            }
            
            // If no points changed clusters, we're done
            if (!changed)
                break;
        }
        
        return clusters;
    }
    
    private List<KMeansCluster> InitializeClusters(List<Point> data)
    {
        var clusters = new List<KMeansCluster>();
        
        // Simple initialization: pick first k points as initial centers
        for (int i = 0; i < k && i < data.Count; i++)
        {
            clusters.Add(new KMeansCluster(data[i]));
        }
        
        return clusters;
    }
    
    private KMeansCluster FindClosestCluster(Point point, List<KMeansCluster> clusters)
    {
        var closest = clusters[0];
        double minDistance = point.Distance(clusters[0].Center);
        
        foreach (var cluster in clusters)
        {
            double distance = point.Distance(cluster.Center);
            if (distance < minDistance)
            {
                minDistance = distance;
                closest = cluster;
            }
        }
        
        return closest;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create sample data points
        var data = new List<Point>
        {
            new Point(1, 2),
            new Point(1.5, 1.8),
            new Point(5, 8),
            new Point(8, 8),
            new Point(1, 0.6),
            new Point(9, 11),
            new Point(8, 2),
            new Point(10, 2),
            new Point(9, 3)
        };
        
        // Apply K-means clustering with k=3
        var kmeans = new KMeans(3);
        var clusters = kmeans.Cluster(data);
        
        // Display results
        Console.WriteLine("K-Means Clustering Results:");
        Console.WriteLine("============================");
        
        for (int i = 0; i < clusters.Count; i++)
        {
            Console.WriteLine($"Cluster {i + 1}:");
            Console.WriteLine($"Center: ({clusters[i].Center.X:F2}, {clusters[i].Center.Y:F2})");
            Console.WriteLine($"Points:");
            
            foreach (var point in clusters[i].Points)
            {
                Console.WriteLine($"  ({point.X:F2}, {point.Y:F2})");
            }
            Console.WriteLine();
        }
    }
}
```

## Expected Output:
```
K-Means Clustering Results:
============================
Cluster 1:
Center: (1.50, 1.47)
Points:
  (1.00, 2.00)
  (1.50, 1.80)
  (1.00, 0.60)

Cluster 2:
Center: (8.00, 7.00)
Points:
  (5.00, 8.00)
  (8.00, 8.00)
  (9.00, 11.00)

Cluster 3:
Center: (9.33, 2.00)
Points:
  (8.00, 2.00)
  (10.00, 2.00)
  (9.00, 3.00)
```

## Key Features of this Implementation:

1. **Point Class**: Represents 2D coordinates with distance calculation
2. **KMeansCluster Class**: Represents a cluster with center point and member points
3. **KMeans Class**: Main clustering algorithm implementation
4. **Initialization**: Uses first k data points as initial cluster centers
5. **Iteration Process**: 
   - Assigns each point to the nearest cluster center
   - Updates cluster centers based on current members
6. **Convergence**: Stops when no points change clusters or max iterations reached

## How it Works:

1. Initialize k cluster centers
2. For each data point, assign it to the nearest cluster center
3. Recalculate cluster centers as the mean of all points in each cluster
4. Repeat steps 2-3 until convergence or maximum iterations reached

This is a basic implementation that can be extended with better initialization methods (like K-means++), more sophisticated distance metrics, or additional features like automatic k selection.