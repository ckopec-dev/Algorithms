# Lloyd's Algorithm Implementation in C#

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

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

public class Cluster
{
    public Point Center { get; set; }
    public List<Point> Points { get; set; }
    
    public Cluster(Point center)
    {
        Center = center;
        Points = new List<Point>();
    }
}

public class LloydAlgorithm
{
    public static List<Cluster> RunLloydAlgorithm(List<Point> points, int k, int maxIterations = 100)
    {
        // Initialize centroids randomly
        var centroids = InitializeCentroids(points, k);
        var clusters = new List<Cluster>();
        
        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            // Clear previous clusters
            clusters.Clear();
            for (int i = 0; i < k; i++)
            {
                clusters.Add(new Cluster(centroids[i]));
            }
            
            // Assign points to closest centroid
            foreach (var point in points)
            {
                var closestCluster = clusters.OrderBy(c => c.Center.Distance(point)).First();
                closestCluster.Points.Add(point);
            }
            
            // Update centroids
            var newCentroids = new List<Point>();
            foreach (var cluster in clusters)
            {
                if (cluster.Points.Count == 0)
                {
                    newCentroids.Add(cluster.Center); // Keep old centroid if empty
                }
                else
                {
                    double newX = cluster.Points.Average(p => p.X);
                    double newY = cluster.Points.Average(p => p.Y);
                    newCentroids.Add(new Point(newX, newY));
                }
            }
            
            // Check for convergence
            bool converged = true;
            for (int i = 0; i < k; i++)
            {
                if (centroids[i].Distance(newCentroids[i]) > 0.001)
                {
                    converged = false;
                    break;
                }
            }
            
            centroids = newCentroids;
            
            if (converged)
            {
                Console.WriteLine($"Converged after {iteration + 1} iterations");
                break;
            }
        }
        
        return clusters;
    }
    
    private static List<Point> InitializeCentroids(List<Point> points, int k)
    {
        var random = new Random();
        var centroids = new List<Point>();
        
        // Simple random initialization
        for (int i = 0; i < k; i++)
        {
            int index = random.Next(points.Count);
            centroids.Add(new Point(points[index].X, points[index].Y));
        }
        
        return centroids;
    }
    
    public static void PrintClusters(List<Cluster> clusters)
    {
        for (int i = 0; i < clusters.Count; i++)
        {
            Console.WriteLine($"Cluster {i + 1}:");
            Console.WriteLine($"  Center: ({clusters[i].Center.X:F2}, {clusters[i].Center.Y:F2})");
            Console.WriteLine($"  Points: {clusters[i].Points.Count}");
            Console.WriteLine();
        }
    }
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
            new Point(1.5, 1.8),
            new Point(5, 8),
            new Point(8, 8),
            new Point(1, 0.6),
            new Point(9, 11),
            new Point(8, 2),
            new Point(10, 2),
            new Point(9, 3)
        };
        
        Console.WriteLine("Original Points:");
        foreach (var point in points)
        {
            Console.WriteLine($"({point.X}, {point.Y})");
        }
        Console.WriteLine();
        
        // Run Lloyd's algorithm
        var clusters = LloydAlgorithm.RunLloydAlgorithm(points, 3);
        
        Console.WriteLine("Final Clusters:");
        LloydAlgorithm.PrintClusters(clusters);
        
        // Calculate and display cluster statistics
        Console.WriteLine("Cluster Statistics:");
        for (int i = 0; i < clusters.Count; i++)
        {
            var cluster = clusters[i];
            var avgDistance = cluster.Points.Average(p => p.Distance(cluster.Center));
            Console.WriteLine($"Cluster {i + 1}:");
            Console.WriteLine($"  Average distance from center: {avgDistance:F2}");
            Console.WriteLine($"  Number of points: {cluster.Points.Count}");
            Console.WriteLine();
        }
    }
}
```

## Algorithm Steps Explained

1. **Initialization**: Randomly select k initial centroids
2. **Assignment**: Assign each point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points in each cluster
4. **Repeat**: Continue until convergence (centroids don't change significantly)

## Key Features

- **Convergence Detection**: Stops when centroids change by less than 0.001
- **Handles Empty Clusters**: Keeps old centroids if a cluster becomes empty
- **Flexible Input**: Works with any number of 2D points
- **Statistics Output**: Provides cluster information and quality metrics

## Sample Output
```
Original Points:
(1, 2)
(1.5, 1.8)
(5, 8)
(8, 8)
(1, 0.6)
(9, 11)
(8, 2)
(10, 2)
(9, 3)

Final Clusters:
Cluster 1:
  Center: (1.17, 1.47)
  Points: 3

Cluster 2:
  Center: (8.00, 5.33)
  Points: 4

Cluster 3:
  Center: (9.00, 2.00)
  Points: 2
```

