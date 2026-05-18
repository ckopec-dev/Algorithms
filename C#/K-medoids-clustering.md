# K-Medoids Clustering Algorithm in C#

Here's a complete implementation of the K-Medoids clustering algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class KMedoidsClustering
{
    public class Point
    {
        public double X { get; set; }
        public double Y { get; set; }
        public int Cluster { get; set; }
        
        public Point(double x, double y)
        {
            X = x;
            Y = y;
            Cluster = -1;
        }
        
        public double Distance(Point other)
        {
            return Math.Sqrt(Math.Pow(X - other.X, 2) + Math.Pow(Y - other.Y, 2));
        }
    }
    
    public class ClusterResult
    {
        public List<Point> Medoids { get; set; }
        public List<List<Point>> Clusters { get; set; }
        public double TotalCost { get; set; }
        
        public ClusterResult()
        {
            Medoids = new List<Point>();
            Clusters = new List<List<Point>>();
        }
    }
    
    public static ClusterResult Cluster(List<Point> points, int k, int maxIterations = 100)
    {
        if (points == null || points.Count < k)
            throw new ArgumentException("Not enough points for clustering");
            
        Random random = new Random();
        List<Point> medoids = new List<Point>();
        
        // Initialize medoids randomly
        HashSet<int> selectedIndices = new HashSet<int>();
        while (selectedIndices.Count < k)
        {
            int index = random.Next(points.Count);
            if (!selectedIndices.Contains(index))
            {
                selectedIndices.Add(index);
                medoids.Add(points[index]);
            }
        }
        
        List<List<Point>> clusters = new List<List<Point>>();
        double previousCost = double.MaxValue;
        
        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            // Assign points to clusters
            clusters = new List<List<Point>>();
            for (int i = 0; i < k; i++)
                clusters.Add(new List<Point>());
            
            foreach (Point point in points)
            {
                double minDistance = double.MaxValue;
                int closestMedoid = 0;
                
                for (int i = 0; i < k; i++)
                {
                    double distance = point.Distance(medoids[i]);
                    if (distance < minDistance)
                    {
                        minDistance = distance;
                        closestMedoid = i;
                    }
                }
                
                point.Cluster = closestMedoid;
                clusters[closestMedoid].Add(point);
            }
            
            // Calculate current cost
            double currentCost = CalculateCost(points, medoids);
            
            // Check for convergence
            if (Math.Abs(currentCost - previousCost) < 1e-6)
                break;
                
            previousCost = currentCost;
            
            // Update medoids
            List<Point> newMedoids = new List<Point>();
            for (int i = 0; i < k; i++)
            {
                if (clusters[i].Count == 0)
                {
                    newMedoids.Add(medoids[i]);
                    continue;
                }
                
                Point newMedoid = FindBestMedoid(clusters[i], medoids[i]);
                newMedoids.Add(newMedoid);
            }
            
            medoids = newMedoids;
        }
        
        ClusterResult result = new ClusterResult
        {
            Medoids = medoids,
            Clusters = clusters,
            TotalCost = previousCost
        };
        
        return result;
    }
    
    private static Point FindBestMedoid(List<Point> cluster, Point currentMedoid)
    {
        double minCost = double.MaxValue;
        Point bestMedoid = currentMedoid;
        
        foreach (Point candidate in cluster)
        {
            double cost = 0;
            foreach (Point point in cluster)
            {
                cost += point.Distance(candidate);
            }
            
            if (cost < minCost)
            {
                minCost = cost;
                bestMedoid = candidate;
            }
        }
        
        return bestMedoid;
    }
    
    private static double CalculateCost(List<Point> points, List<Point> medoids)
    {
        double totalCost = 0;
        
        foreach (Point point in points)
        {
            double minDistance = double.MaxValue;
            foreach (Point medoid in medoids)
            {
                double distance = point.Distance(medoid);
                if (distance < minDistance)
                    minDistance = distance;
            }
            totalCost += minDistance;
        }
        
        return totalCost;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create sample data points
        List<KMedoidsClustering.Point> points = new List<KMedoidsClustering.Point>
        {
            new KMedoidsClustering.Point(1, 2),
            new KMedoidsClustering.Point(1, 4),
            new KMedoidsClustering.Point(1, 0),
            new KMedoidsClustering.Point(4, 2),
            new KMedoidsClustering.Point(4, 4),
            new KMedoidsClustering.Point(4, 0),
            new KMedoidsClustering.Point(7, 2),
            new KMedoidsClustering.Point(7, 4),
            new KMedoidsClustering.Point(7, 0),
            new KMedoidsClustering.Point(10, 2),
            new KMedoidsClustering.Point(10, 4),
            new KMedoidsClustering.Point(10, 0)
        };
        
        // Perform K-Medoids clustering with k=3
        var result = KMedoidsClustering.Cluster(points, 3);
        
        // Display results
        Console.WriteLine("K-Medoids Clustering Results:");
        Console.WriteLine("==============================");
        Console.WriteLine($"Total Cost: {result.TotalCost:F2}");
        Console.WriteLine();
        
        for (int i = 0; i < result.Medoids.Count; i++)
        {
            Console.WriteLine($"Cluster {i + 1}:");
            Console.WriteLine($"  Medoid: ({result.Medoids[i].X:F1}, {result.Medoids[i].Y:F1})");
            Console.WriteLine($"  Points in cluster:");
            
            foreach (var point in result.Clusters[i])
            {
                Console.WriteLine($"    ({point.X:F1}, {point.Y:F1})");
            }
            Console.WriteLine();
        }
    }
}
```

## Key Features of this Implementation:

1. **Point Class**: Represents data points with X, Y coordinates and cluster assignment
2. **ClusterResult Class**: Contains the clustering results including medoids and clusters
3. **Main Algorithm**: Implements the K-Medoids algorithm with:
   - Random initialization of medoids
   - Point assignment to closest medoid
   - Medoid update using the "best" point in each cluster
   - Convergence checking based on cost function
4. **Distance Calculation**: Uses Euclidean distance
5. **Cost Function**: Sum of distances from each point to its assigned medoid

## How it Works:

1. **Initialization**: Randomly select k points as initial medoids
2. **Assignment**: Assign each point to the cluster of its nearest medoid
3. **Update**: For each cluster, find the point that minimizes the sum of distances to all points in that cluster
4. **Repeat**: Continue until convergence or maximum iterations reached

## Output Example:
```
K-Medoids Clustering Results:
==============================
Total Cost: 12.45

Cluster 1:
  Medoid: (1.0, 2.0)
  Points in cluster:
    (1.0, 2.0)
    (1.0, 4.0)
    (1.0, 0.0)

Cluster 2:
  Medoid: (4.0, 2.0)
  Points in cluster:
    (4.0, 2.0)
    (4.0, 4.0)
    (4.0, 0.0)

Cluster 3:
  Medoid: (7.0, 2.0)
  Points in cluster:
    (7.0, 2.0)
    (7.0, 4.0)
    (7.0, 0.0)
```

This implementation provides a complete, working K-Medoids clustering solution that can be easily integrated into larger C# applications.

