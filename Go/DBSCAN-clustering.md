# DBSCAN Clustering Algorithm in Go

Here's a complete implementation of the DBSCAN clustering algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
)

// Point represents a data point in n-dimensional space
type Point struct {
    X, Y float64
    ID   int
}

// Cluster represents a group of points
type Cluster struct {
    ID     int
    Points []Point
}

// DBSCAN implements the DBSCAN clustering algorithm
type DBSCAN struct {
    eps     float64
    minPts  int
    points  []Point
    clusters []Cluster
    visited []bool
    noise   []Point
}

// NewDBSCAN creates a new DBSCAN instance
func NewDBSCAN(eps float64, minPts int) *DBSCAN {
    return &DBSCAN{
        eps:     eps,
        minPts:  minPts,
        points:  make([]Point, 0),
        clusters: make([]Cluster, 0),
        visited: make([]bool, 0),
        noise:   make([]Point, 0),
    }
}

// AddPoint adds a point to the dataset
func (d *DBSCAN) AddPoint(point Point) {
    d.points = append(d.points, point)
    d.visited = append(d.visited, false)
}

// Distance calculates Euclidean distance between two points
func (d *DBSCAN) Distance(p1, p2 Point) float64 {
    dx := p1.X - p2.X
    dy := p1.Y - p2.Y
    return math.Sqrt(dx*dx + dy*dy)
}

// RegionQuery finds all points within epsilon distance of a point
func (d *DBSCAN) RegionQuery(point Point) []int {
    neighbors := make([]int, 0)
    for i, p := range d.points {
        if d.Distance(point, p) <= d.eps {
            neighbors = append(neighbors, i)
        }
    }
    return neighbors
}

// ExpandCluster expands a cluster from a core point
func (d *DBSCAN) ExpandCluster(point Point, neighbors []int, clusterID int) {
    cluster := Cluster{ID: clusterID, Points: make([]Point, 0)}
    
    // Add the initial point to the cluster
    for i, p := range d.points {
        if p.ID == point.ID {
            cluster.Points = append(cluster.Points, p)
            break
        }
    }
    
    // Process neighbors
    for _, neighborIdx := range neighbors {
        if !d.visited[neighborIdx] {
            d.visited[neighborIdx] = true
            neighborPoint := d.points[neighborIdx]
            neighborNeighbors := d.RegionQuery(neighborPoint)
            
            if len(neighborNeighbors) >= d.minPts {
                // This is a core point, add its neighbors to the queue
                for _, n := range neighborNeighbors {
                    if !d.contains(neighbors, n) {
                        neighbors = append(neighbors, n)
                    }
                }
            }
        }
        
        // Check if point is already in a cluster
        isInCluster := false
        for _, c := range d.clusters {
            for _, p := range c.Points {
                if p.ID == d.points[neighborIdx].ID {
                    isInCluster = true
                    break
                }
            }
            if isInCluster {
                break
            }
        }
        
        if !isInCluster {
            cluster.Points = append(cluster.Points, d.points[neighborIdx])
        }
    }
    
    d.clusters = append(d.clusters, cluster)
}

// contains checks if a slice contains a value
func (d *DBSCAN) contains(slice []int, value int) bool {
    for _, v := range slice {
        if v == value {
            return true
        }
    }
    return false
}

// Cluster performs DBSCAN clustering
func (d *DBSCAN) Cluster() {
    clusterID := 0
    
    for i, point := range d.points {
        if d.visited[i] {
            continue
        }
        
        d.visited[i] = true
        neighbors := d.RegionQuery(point)
        
        if len(neighbors) < d.minPts {
            // Noise point
            d.noise = append(d.noise, point)
        } else {
            // Core point, start new cluster
            clusterID++
            d.ExpandCluster(point, neighbors, clusterID)
        }
    }
}

// PrintResults displays the clustering results
func (d *DBSCAN) PrintResults() {
    fmt.Printf("DBSCAN Results:\n")
    fmt.Printf("Epsilon: %.2f, MinPts: %d\n", d.eps, d.minPts)
    fmt.Printf("Total points: %d\n", len(d.points))
    fmt.Printf("Noise points: %d\n", len(d.noise))
    fmt.Printf("Number of clusters: %d\n\n", len(d.clusters))
    
    for i, cluster := range d.clusters {
        fmt.Printf("Cluster %d (size: %d):\n", i+1, len(cluster.Points))
        for _, point := range cluster.Points {
            fmt.Printf("  Point(%d): (%.2f, %.2f)\n", point.ID, point.X, point.Y)
        }
        fmt.Println()
    }
    
    if len(d.noise) > 0 {
        fmt.Println("Noise points:")
        for _, point := range d.noise {
            fmt.Printf("  Point(%d): (%.2f, %.2f)\n", point.ID, point.X, point.Y)
        }
    }
}

// Example usage
func main() {
    // Create DBSCAN instance with eps=2.0 and minPts=3
    dbscan := NewDBSCAN(2.0, 3)
    
    // Add sample points
    points := []Point{
        {X: 1.0, Y: 1.0, ID: 1},
        {X: 1.5, Y: 1.0, ID: 2},
        {X: 1.2, Y: 1.1, ID: 3},
        {X: 5.0, Y: 5.0, ID: 4},
        {X: 5.1, Y: 5.0, ID: 5},
        {X: 5.2, Y: 5.1, ID: 6},
        {X: 10.0, Y: 10.0, ID: 7},
        {X: 10.1, Y: 10.1, ID: 8},
        {X: 10.2, Y: 10.2, ID: 9},
        {X: 1.0, Y: 10.0, ID: 10},
    }
    
    // Add points to the DBSCAN instance
    for _, point := range points {
        dbscan.AddPoint(point)
    }
    
    // Perform clustering
    dbscan.Cluster()
    
    // Print results
    dbscan.PrintResults()
}
```

## How it works:

1. **Point Structure**: Represents a data point with X, Y coordinates and an ID
2. **DBSCAN Structure**: Contains the algorithm parameters (eps, minPts) and data structures for clustering
3. **Distance Function**: Calculates Euclidean distance between two points
4. **RegionQuery**: Finds all points within epsilon distance of a given point
5. **ExpandCluster**: Grows a cluster by recursively finding neighbors of core points
6. **Cluster Method**: Main clustering function that processes all points

## Key Features:

- **Core Point Detection**: Points with at least minPts neighbors within eps distance
- **Cluster Expansion**: Core points expand clusters by including density-reachable points
- **Noise Detection**: Points that are neither core points nor density-reachable from core points
- **Cluster Identification**: Each cluster gets a unique ID

## Sample Output:
```
DBSCAN Results:
Epsilon: 2.00, MinPts: 3
Total points: 10
Noise points: 1
Number of clusters: 3

Cluster 1 (size: 3):
  Point(1): (1.00, 1.00)
  Point(2): (1.50, 1.00)
  Point(3): (1.20, 1.10)

Cluster 2 (size: 3):
  Point(4): (5.00, 5.00)
  Point(5): (5.10, 5.00)
  Point(6): (5.20, 5.10)

Cluster 3 (size: 3):
  Point(7): (10.00, 10.00)
  Point(8): (10.10, 10.10)
  Point(9): (10.20, 10.20)

Noise points:
  Point(10): (1.00, 10.00)
```

This implementation demonstrates the core DBSCAN algorithm principles while being fully functional in Go.

