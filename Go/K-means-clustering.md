# K-Means Clustering in Go

Here's a complete implementation of the K-means clustering algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Point represents a data point in n-dimensional space
type Point struct {
    X []float64
}

// Cluster represents a cluster with its centroid and member points
type Cluster struct {
    Centroid Point
    Points   []Point
}

// KMeans represents the K-means clustering algorithm
type KMeans struct {
    K        int
    Clusters []Cluster
    MaxIter  int
}

// NewKMeans creates a new K-means instance
func NewKMeans(k, maxIter int) *KMeans {
    return &KMeans{
        K:       k,
        MaxIter: maxIter,
    }
}

// EuclideanDistance calculates the Euclidean distance between two points
func EuclideanDistance(p1, p2 Point) float64 {
    sum := 0.0
    for i := 0; i < len(p1.X); i++ {
        diff := p1.X[i] - p2.X[i]
        sum += diff * diff
    }
    return math.Sqrt(sum)
}

// InitializeClusters initializes clusters with random centroids
func (km *KMeans) initializeClusters(points []Point) {
    km.Clusters = make([]Cluster, km.K)
    
    // Simple random initialization
    for i := 0; i < km.K; i++ {
        km.Clusters[i] = Cluster{
            Centroid: points[rand.Intn(len(points))],
            Points:   []Point{},
        }
    }
}

// AssignPoints assigns each point to the nearest cluster
func (km *KMeans) assignPoints(points []Point) {
    for i := range km.Clusters {
        km.Clusters[i].Points = []Point{}
    }
    
    for _, point := range points {
        minDist := math.MaxFloat64
        clusterIndex := 0
        
        for i, cluster := range km.Clusters {
            dist := EuclideanDistance(point, cluster.Centroid)
            if dist < minDist {
                minDist = dist
                clusterIndex = i
            }
        }
        
        km.Clusters[clusterIndex].Points = append(km.Clusters[clusterIndex].Points, point)
    }
}

// UpdateCentroids updates cluster centroids based on member points
func (km *KMeans) updateCentroids(points []Point) {
    for i, cluster := range km.Clusters {
        if len(cluster.Points) == 0 {
            continue
        }
        
        // Calculate new centroid as the mean of all points in the cluster
        newCentroid := make([]float64, len(cluster.Points[0].X))
        
        for _, point := range cluster.Points {
            for j, coord := range point.X {
                newCentroid[j] += coord
            }
        }
        
        for j := range newCentroid {
            newCentroid[j] /= float64(len(cluster.Points))
        }
        
        km.Clusters[i].Centroid = Point{X: newCentroid}
    }
}

// Fit performs K-means clustering on the given points
func (km *KMeans) Fit(points []Point) {
    // Initialize clusters
    km.initializeClusters(points)
    
    // Iterate until convergence or max iterations
    for iter := 0; iter < km.MaxIter; iter++ {
        // Assign points to clusters
        km.assignPoints(points)
        
        // Store old centroids for convergence check
        oldCentroids := make([]Point, len(km.Clusters))
        for i, cluster := range km.Clusters {
            oldCentroids[i] = cluster.Centroid
        }
        
        // Update centroids
        km.updateCentroids(points)
        
        // Check for convergence
        converged := true
        for i, cluster := range km.Clusters {
            if EuclideanDistance(cluster.Centroid, oldCentroids[i]) > 1e-4 {
                converged = false
                break
            }
        }
        
        if converged {
            fmt.Printf("Converged after %d iterations\n", iter+1)
            break
        }
    }
}

// PrintClusters prints the clustering results
func (km *KMeans) PrintClusters() {
    for i, cluster := range km.Clusters {
        fmt.Printf("Cluster %d (centroid: %v):\n", i, cluster.Centroid.X)
        for _, point := range cluster.Points {
            fmt.Printf("  %v\n", point.X)
        }
        fmt.Println()
    }
}

func main() {
    // Set random seed for reproducible results
    rand.Seed(time.Now().UnixNano())
    
    // Create sample data points (2D points)
    points := []Point{
        {X: []float64{1.0, 2.0}},
        {X: []float64{1.5, 1.8}},
        {X: []float64{5.0, 8.0}},
        {X: []float64{8.0, 8.0}},
        {X: []float64{1.0, 0.6}},
        {X: []float64{9.0, 11.0}},
        {X: []float64{8.0, 2.0}},
        {X: []float64{10.0, 2.0}},
        {X: []float64{9.0, 3.0}},
    }
    
    fmt.Println("Original data points:")
    for _, point := range points {
        fmt.Printf("%v\n", point.X)
    }
    fmt.Println()
    
    // Create and run K-means clustering
    kmeans := NewKMeans(3, 100)
    kmeans.Fit(points)
    
    fmt.Println("Clustering Results:")
    kmeans.PrintClusters()
    
    // Example with different data
    fmt.Println("=== Example with 4 clusters ===")
    points2 := []Point{
        {X: []float64{2.0, 2.0}},
        {X: []float64{2.5, 3.0}},
        {X: []float64{3.0, 1.0}},
        {X: []float64{8.0, 7.0}},
        {X: []float64{8.5, 8.0}},
        {X: []float64{7.5, 7.5}},
        {X: []float64{1.0, 1.0}},
        {X: []float64{9.0, 9.0}},
    }
    
    kmeans2 := NewKMeans(4, 100)
    kmeans2.Fit(points2)
    
    fmt.Println("Clustering Results (4 clusters):")
    kmeans2.PrintClusters()
}
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `Point`: Represents a data point in n-dimensional space
- `Cluster`: Contains a centroid and member points
- `KMeans`: Main clustering algorithm struct

### 2. **Core Methods**
- `EuclideanDistance`: Calculates distance between points
- `initializeClusters`: Sets up initial centroids
- `assignPoints`: Assigns each point to the nearest cluster
- `updateCentroids`: Recalculates centroids based on cluster members
- `Fit`: Main clustering function that runs the algorithm

### 3. **Algorithm Steps**
1. Initialize K centroids randomly
2. Assign each point to the nearest centroid
3. Update centroids to the mean of assigned points
4. Repeat until convergence or max iterations

### 4. **Usage Example**
The code demonstrates clustering with:
- 2D data points
- 3 clusters
- 4 clusters
- Convergence detection

### 5. **Output Format**
The program shows:
- Original data points
- Cluster centroids
- Points assigned to each cluster
- Convergence information

This implementation is flexible and can handle data points of any dimensionality, making it suitable for various clustering tasks in Go applications.

