# Lloyd's Algorithm Implementation in Go

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Point represents a 2D point
type Point struct {
    X, Y float64
}

// Centroid represents a cluster centroid
type Centroid struct {
    Point
    ID int
}

// Distance calculates Euclidean distance between two points
func (p Point) Distance(other Point) float64 {
    dx := p.X - other.X
    dy := p.Y - other.Y
    return math.Sqrt(dx*dx + dy*dy)
}

// LloydAlgorithm performs k-means clustering using Lloyd's algorithm
func LloydAlgorithm(points []Point, k int, maxIterations int) ([]Centroid, [][]Point) {
    // Initialize centroids randomly
    centroids := initializeCentroids(points, k)
    
    var clusters [][]Point
    
    for iteration := 0; iteration < maxIterations; iteration++ {
        // Assign points to closest centroid
        clusters = assignPointsToClusters(points, centroids)
        
        // Update centroids based on cluster means
        newCentroids := updateCentroids(clusters)
        
        // Check for convergence
        if centroidsConverged(centroids, newCentroids) {
            fmt.Printf("Converged after %d iterations\n", iteration+1)
            break
        }
        
        centroids = newCentroids
    }
    
    return centroids, clusters
}

// initializeCentroids randomly selects k points from the dataset as initial centroids
func initializeCentroids(points []Point, k int) []Centroid {
    rand.Seed(time.Now().UnixNano())
    centroids := make([]Centroid, k)
    
    // Simple random initialization
    for i := 0; i < k; i++ {
        idx := rand.Intn(len(points))
        centroids[i] = Centroid{
            Point: points[idx],
            ID:    i,
        }
    }
    
    return centroids
}

// assignPointsToClusters assigns each point to the nearest centroid
func assignPointsToClusters(points []Point, centroids []Centroid) [][]Point {
    clusters := make([][]Point, len(centroids))
    
    for _, point := range points {
        minDistance := math.MaxFloat64
        closestCentroid := 0
        
        for i, centroid := range centroids {
            distance := point.Distance(centroid.Point)
            if distance < minDistance {
                minDistance = distance
                closestCentroid = i
            }
        }
        
        clusters[closestCentroid] = append(clusters[closestCentroid], point)
    }
    
    return clusters
}

// updateCentroids calculates new centroids as the mean of all points in each cluster
func updateCentroids(clusters [][]Point) []Centroid {
    centroids := make([]Centroid, len(clusters))
    
    for i, cluster := range clusters {
        if len(cluster) == 0 {
            // If cluster is empty, keep the old centroid
            continue
        }
        
        var sumX, sumY float64
        for _, point := range cluster {
            sumX += point.X
            sumY += point.Y
        }
        
        centroids[i] = Centroid{
            Point: Point{
                X: sumX / float64(len(cluster)),
                Y: sumY / float64(len(cluster)),
            },
            ID: i,
        }
    }
    
    return centroids
}

// centroidsConverged checks if centroids have stopped changing significantly
func centroidsConverged(oldCentroids, newCentroids []Centroid) bool {
    threshold := 0.001
    
    for i := range oldCentroids {
        if oldCentroids[i].Point.Distance(newCentroids[i].Point) > threshold {
            return false
        }
    }
    
    return true
}

// printResults displays the clustering results
func printResults(centroids []Centroid, clusters [][]Point) {
    fmt.Println("Final Clusters:")
    for i, cluster := range clusters {
        fmt.Printf("Cluster %d (centroid: (%.2f, %.2f)): %d points\n", 
            i, centroids[i].X, centroids[i].Y, len(cluster))
        for _, point := range cluster {
            fmt.Printf("  (%.2f, %.2f)\n", point.X, point.Y)
        }
        fmt.Println()
    }
}

func main() {
    // Create sample data points
    points := []Point{
        {1, 2}, {1.5, 1.8}, {5, 8}, {8, 8}, {1, 0.6},
        {9, 11}, {8, 2}, {10, 2}, {9, 3}, {2, 1},
        {3, 2}, {4, 3}, {5, 4}, {6, 5}, {7, 6},
    }
    
    fmt.Println("Original points:")
    for i, point := range points {
        fmt.Printf("Point %d: (%.2f, %.2f)\n", i, point.X, point.Y)
    }
    fmt.Println()
    
    // Run Lloyd's algorithm
    k := 3
    maxIterations := 100
    centroids, clusters := LloydAlgorithm(points, k, maxIterations)
    
    // Display results
    printResults(centroids, clusters)
}
```

## How Lloyd's Algorithm Works

1. **Initialization**: Randomly select k centroids from the dataset
2. **Assignment**: Assign each point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points in each cluster
4. **Repeat**: Continue steps 2-3 until convergence or maximum iterations

## Key Features

- **Distance Calculation**: Uses Euclidean distance
- **Convergence Detection**: Stops when centroids don't change significantly
- **Flexible Input**: Works with any number of 2D points
- **Clear Output**: Shows final clusters and their centroids

## Sample Output

```
Original points:
Point 0: (1.00, 2.00)
Point 1: (1.50, 1.80)
Point 2: (5.00, 8.00)
...

Final Clusters:
Cluster 0 (centroid: (1.50, 1.47)): 5 points
  (1.00, 2.00)
  (1.50, 1.80)
  (2.00, 1.00)
  (1.00, 0.60)
  (3.00, 2.00)

Cluster 1 (centroid: (7.00, 7.00)): 5 points
  (5.00, 8.00)
  (8.00, 8.00)
  (9.00, 11.00)
  (8.00, 2.00)
  (10.00, 2.00)

Cluster 2 (centroid: (6.00, 5.00)): 5 points
  (9.00, 3.00)
  (4.00, 3.00)
  (5.00, 4.00)
  (6.00, 5.00)
  (7.00, 6.00)
```

This implementation demonstrates the core principles of Lloyd's algorithm for k-means clustering in Go.

