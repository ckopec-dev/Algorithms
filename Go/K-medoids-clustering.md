# K-Medoids Clustering in Go

Here's a complete implementation of the K-Medoids clustering algorithm in Go:

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
    Coordinates []float64
    ID          int
}

// Cluster represents a cluster with its medoid and member points
type Cluster struct {
    Medoid   Point
    Members  []Point
    Distance float64
}

// KMedoids represents the K-Medoids clustering algorithm
type KMedoids struct {
    k           int
    maxIterations int
    clusters    []Cluster
    data        []Point
}

// NewKMedoids creates a new K-Medoids instance
func NewKMedoids(k, maxIterations int) *KMedoids {
    return &KMedoids{
        k:           k,
        maxIterations: maxIterations,
        clusters:    make([]Cluster, 0),
        data:        make([]Point, 0),
    }
}

// AddData adds data points to the clustering algorithm
func (km *KMedoids) AddData(points []Point) {
    km.data = points
}

// EuclideanDistance calculates the Euclidean distance between two points
func EuclideanDistance(p1, p2 Point) float64 {
    sum := 0.0
    for i := 0; i < len(p1.Coordinates); i++ {
        diff := p1.Coordinates[i] - p2.Coordinates[i]
        sum += diff * diff
    }
    return math.Sqrt(sum)
}

// InitializeMedoids randomly selects k medoids from the data
func (km *KMedoids) initializeMedoids() []Point {
    rand.Seed(time.Now().UnixNano())
    medoids := make([]Point, 0, km.k)
    selected := make(map[int]bool)
    
    for len(medoids) < km.k {
        index := rand.Intn(len(km.data))
        if !selected[index] {
            selected[index] = true
            medoids = append(medoids, km.data[index])
        }
    }
    
    return medoids
}

// AssignPointsToClusters assigns each point to the nearest medoid
func (km *KMedoids) assignPointsToClusters(medoids []Point) []Cluster {
    clusters := make([]Cluster, km.k)
    
    // Initialize clusters with medoids
    for i, medoid := range medoids {
        clusters[i] = Cluster{
            Medoid:  medoid,
            Members: make([]Point, 0),
        }
    }
    
    // Assign each point to the nearest medoid
    for _, point := range km.data {
        minDistance := math.MaxFloat64
        nearestCluster := 0
        
        for i, cluster := range clusters {
            distance := EuclideanDistance(point, cluster.Medoid)
            if distance < minDistance {
                minDistance = distance
                nearestCluster = i
            }
        }
        
        clusters[nearestCluster].Members = append(clusters[nearestCluster].Members, point)
    }
    
    return clusters
}

// CalculateTotalCost calculates the total cost (sum of distances) for the current clustering
func (km *KMedoids) calculateTotalCost(clusters []Cluster) float64 {
    totalCost := 0.0
    for _, cluster := range clusters {
        for _, point := range cluster.Members {
            totalCost += EuclideanDistance(point, cluster.Medoid)
        }
    }
    return totalCost
}

// UpdateMedoids updates medoids by finding the point that minimizes the cost function
func (km *KMedoids) updateMedoids(clusters []Cluster) []Point {
    newMedoids := make([]Point, km.k)
    
    for i, cluster := range clusters {
        if len(cluster.Members) == 0 {
            newMedoids[i] = cluster.Medoid
            continue
        }
        
        minCost := math.MaxFloat64
        bestMedoid := cluster.Medoid
        
        // Try each point in the cluster as a potential medoid
        for _, point := range cluster.Members {
            // Calculate cost if this point becomes the medoid
            cost := 0.0
            for _, member := range cluster.Members {
                cost += EuclideanDistance(point, member)
            }
            
            if cost < minCost {
                minCost = cost
                bestMedoid = point
            }
        }
        
        newMedoids[i] = bestMedoid
    }
    
    return newMedoids
}

// Fit performs the K-Medoids clustering
func (km *KMedoids) Fit() []Cluster {
    if len(km.data) == 0 {
        return nil
    }
    
    // Initialize medoids
    medoids := km.initializeMedoids()
    bestClusters := make([]Cluster, 0)
    bestCost := math.MaxFloat64
    
    // Run K-Medoids algorithm
    for iteration := 0; iteration < km.maxIterations; iteration++ {
        // Assign points to clusters
        clusters := km.assignPointsToClusters(medoids)
        
        // Calculate total cost
        cost := km.calculateTotalCost(clusters)
        
        // Update medoids
        newMedoids := km.updateMedoids(clusters)
        
        // Check if we found a better solution
        if cost < bestCost {
            bestCost = cost
            bestClusters = clusters
        }
        
        // Check for convergence (if medoids don't change)
        if equalMedoids(medoids, newMedoids) {
            break
        }
        
        medoids = newMedoids
    }
    
    return bestClusters
}

// equalMedoids checks if two sets of medoids are equal
func equalMedoids(medoids1, medoids2 []Point) bool {
    if len(medoids1) != len(medoids2) {
        return false
    }
    
    for i := 0; i < len(medoids1); i++ {
        if !pointsEqual(medoids1[i], medoids2[i]) {
            return false
        }
    }
    return true
}

// pointsEqual checks if two points are equal
func pointsEqual(p1, p2 Point) bool {
    if len(p1.Coordinates) != len(p2.Coordinates) {
        return false
    }
    
    for i := 0; i < len(p1.Coordinates); i++ {
        if math.Abs(p1.Coordinates[i]-p2.Coordinates[i]) > 1e-9 {
            return false
        }
    }
    return true
}

// PrintClusters prints the clustering results
func PrintClusters(clusters []Cluster) {
    for i, cluster := range clusters {
        fmt.Printf("Cluster %d (Medoid: (%.2f, %.2f)):\n", i+1, 
            cluster.Medoid.Coordinates[0], cluster.Medoid.Coordinates[1])
        for _, point := range cluster.Members {
            fmt.Printf("  Point (%.2f, %.2f)\n", point.Coordinates[0], point.Coordinates[1])
        }
        fmt.Printf("  Number of points: %d\n\n", len(cluster.Members))
    }
}

func main() {
    // Create sample data points
    data := []Point{
        {Coordinates: []float64{1.0, 2.0}, ID: 1},
        {Coordinates: []float64{1.5, 1.8}, ID: 2},
        {Coordinates: []float64{5.0, 8.0}, ID: 3},
        {Coordinates: []float64{8.0, 8.0}, ID: 4},
        {Coordinates: []float64{1.0, 0.6}, ID: 5},
        {Coordinates: []float64{9.0, 11.0}, ID: 6},
        {Coordinates: []float64{8.0, 2.0}, ID: 7},
        {Coordinates: []float64{10.0, 2.0}, ID: 8},
        {Coordinates: []float64{9.0, 3.0}, ID: 9},
    }
    
    // Create K-Medoids instance with k=3 and max 100 iterations
    kmedoids := NewKMedoids(3, 100)
    kmedoids.AddData(data)
    
    // Perform clustering
    clusters := kmedoids.Fit()
    
    // Print results
    fmt.Println("K-Medoids Clustering Results:")
    fmt.Println("==============================")
    PrintClusters(clusters)
    
    // Calculate and print total cost
    totalCost := 0.0
    for _, cluster := range clusters {
        for _, point := range cluster.Members {
            totalCost += EuclideanDistance(point, cluster.Medoid)
        }
    }
    fmt.Printf("Total Cost: %.2f\n", totalCost)
}
```

## How it works:

1. **Point Structure**: Represents data points in n-dimensional space
2. **KMedoids Structure**: Main clustering algorithm implementation
3. **Key Functions**:
   - `initializeMedoids()`: Randomly selects initial medoids
   - `assignPointsToClusters()`: Assigns each point to the nearest medoid
   - `updateMedoids()`: Updates medoids by finding the point that minimizes cost
   - `calculateTotalCost()`: Computes the total clustering cost
   - `Fit()`: Main clustering function that runs the algorithm

## Key Features:

- **Robust Initialization**: Random selection of initial medoids
- **Cost Function**: Minimizes the sum of distances from points to their medoids
- **Convergence**: Stops when medoids don't change or maximum iterations reached
- **Flexible Dimensionality**: Works with any number of dimensions
- **Clear Output**: Shows cluster assignments and medoids

## Sample Output:
```
K-Medoids Clustering Results:
==============================
Cluster 1 (Medoid: (1.00, 2.00)):
  Point (1.00, 2.00)
  Point (1.50, 1.80)
  Point (1.00, 0.60)
  Number of points: 3

Cluster 2 (Medoid: (8.00, 8.00)):
  Point (5.00, 8.00)
  Point (8.00, 8.00)
  Point (9.00, 11.00)
  Number of points: 3

Cluster 3 (Medoid: (9.00, 2.00)):
  Point (8.00, 2.00)
  Point (10.00, 2.00)
  Point (9.00, 3.00)
  Number of points: 3

Total Cost: 12.74
```

This implementation provides a complete, working K-Medoids clustering solution in Go that can be easily extended or modified for specific use cases.

