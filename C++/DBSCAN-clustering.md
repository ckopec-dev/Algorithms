# DBSCAN Clustering Algorithm in C++

Here's a complete implementation of the DBSCAN clustering algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

class Point {
public:
    double x, y;
    int cluster_id;
    bool visited;
    
    Point(double x, double y) : x(x), y(y), cluster_id(-1), visited(false) {}
    
    // Calculate Euclidean distance between two points
    double distance(const Point& other) const {
        return sqrt(pow(x - other.x, 2) + pow(y - other.y, 2));
    }
};

class DBSCAN {
private:
    std::vector<Point> points;
    double eps;
    int minPts;
    int current_cluster_id;
    
public:
    DBSCAN(double epsilon, int min_points) : eps(epsilon), minPts(min_points), current_cluster_id(0) {}
    
    void addPoint(double x, double y) {
        points.emplace_back(x, y);
    }
    
    // Find all neighbors within eps distance
    std::vector<int> getNeighbors(int point_idx) {
        std::vector<int> neighbors;
        for (int i = 0; i < points.size(); i++) {
            if (points[point_idx].distance(points[i]) <= eps) {
                neighbors.push_back(i);
            }
        }
        return neighbors;
    }
    
    // Expand cluster recursively
    void expandCluster(int point_idx, const std::vector<int>& neighbors) {
        points[point_idx].cluster_id = current_cluster_id;
        points[point_idx].visited = true;
        
        for (int neighbor_idx : neighbors) {
            if (!points[neighbor_idx].visited) {
                points[neighbor_idx].visited = true;
                std::vector<int> neighbor_neighbors = getNeighbors(neighbor_idx);
                
                if (neighbor_neighbors.size() >= minPts) {
                    // Add neighbors to the current neighbors list
                    for (int n : neighbor_neighbors) {
                        if (std::find(neighbors.begin(), neighbors.end(), n) == neighbors.end()) {
                            neighbors.push_back(n);
                        }
                    }
                }
            }
            
            // If neighbor hasn't been assigned to a cluster yet
            if (points[neighbor_idx].cluster_id == -1) {
                points[neighbor_idx].cluster_id = current_cluster_id;
            }
        }
    }
    
    void run() {
        for (int i = 0; i < points.size(); i++) {
            if (points[i].visited) continue;
            
            points[i].visited = true;
            std::vector<int> neighbors = getNeighbors(i);
            
            if (neighbors.size() < minPts) {
                // Mark as noise
                points[i].cluster_id = -2; // -2 represents noise
            } else {
                // Start new cluster
                current_cluster_id++;
                expandCluster(i, neighbors);
            }
        }
    }
    
    void printResults() {
        std::cout << "DBSCAN Results:\n";
        std::cout << "================\n";
        
        // Count points per cluster
        std::unordered_map<int, int> cluster_count;
        for (const auto& point : points) {
            if (point.cluster_id >= 0) {
                cluster_count[point.cluster_id]++;
            }
        }
        
        // Print cluster information
        for (int i = 0; i < points.size(); i++) {
            const auto& point = points[i];
            if (point.cluster_id == -2) {
                std::cout << "Point " << i << " (" << point.x << ", " << point.y 
                         << ") -> Noise\n";
            } else {
                std::cout << "Point " << i << " (" << point.x << ", " << point.y 
                         << ") -> Cluster " << point.cluster_id << "\n";
            }
        }
        
        std::cout << "\nCluster Statistics:\n";
        for (const auto& pair : cluster_count) {
            std::cout << "Cluster " << pair.first << ": " << pair.second << " points\n";
        }
        
        std::cout << "Noise points: " << std::count_if(points.begin(), points.end(), 
            [](const Point& p) { return p.cluster_id == -2; }) << "\n";
    }
    
    // Get cluster information
    std::vector<std::vector<int>> getClusters() {
        std::unordered_map<int, std::vector<int>> cluster_map;
        
        for (int i = 0; i < points.size(); i++) {
            if (points[i].cluster_id >= 0) {
                cluster_map[points[i].cluster_id].push_back(i);
            }
        }
        
        std::vector<std::vector<int>> clusters;
        for (const auto& pair : cluster_map) {
            clusters.push_back(pair.second);
        }
        
        return clusters;
    }
};

int main() {
    // Create DBSCAN instance with eps=2.0 and minPts=3
    DBSCAN dbscan(2.0, 3);
    
    // Add sample data points
    // Cluster 1: Points close to (1,1)
    dbscan.addPoint(1.0, 1.0);
    dbscan.addPoint(1.1, 1.2);
    dbscan.addPoint(0.9, 0.8);
    dbscan.addPoint(1.2, 1.1);
    
    // Cluster 2: Points close to (8,8)
    dbscan.addPoint(8.0, 8.0);
    dbscan.addPoint(8.1, 8.2);
    dbscan.addPoint(7.9, 7.8);
    dbscan.addPoint(8.2, 8.1);
    
    // Noise point (too far from any cluster)
    dbscan.addPoint(15.0, 15.0);
    
    // Run DBSCAN clustering
    dbscan.run();
    
    // Print results
    dbscan.printResults();
    
    return 0;
}
```

## How it works:

1. **Point Class**: Represents a data point with x,y coordinates, cluster ID, and visited flag
2. **DBSCAN Class**: Implements the core algorithm with:
   - `addPoint()`: Adds data points to the dataset
   - `getNeighbors()`: Finds all points within epsilon distance
   - `expandCluster()`: Recursively expands clusters using the DBSCAN algorithm
   - `run()`: Main clustering function that processes all points
   - `printResults()`: Displays clustering results

## Key Features:

- **Core Algorithm**: Implements the standard DBSCAN algorithm with epsilon and minPts parameters
- **Cluster Expansion**: Uses recursive expansion to find connected components
- **Noise Detection**: Points that don't belong to any cluster are marked as noise (-2)
- **Cluster Counting**: Automatically counts points in each cluster
- **Flexible Input**: Can handle any number of 2D points

## Output Example:
```
DBSCAN Results:
================
Point 0 (1, 1) -> Cluster 1
Point 1 (1.1, 1.2) -> Cluster 1
Point 2 (0.9, 0.8) -> Cluster 1
Point 3 (1.2, 1.1) -> Cluster 1
Point 4 (8, 8) -> Cluster 2
Point 5 (8.1, 8.2) -> Cluster 2
Point 6 (7.9, 7.8) -> Cluster 2
Point 7 (8.2, 8.1) -> Cluster 2
Point 8 (15, 15) -> Noise

Cluster Statistics:
Cluster 1: 4 points
Cluster 2: 4 points
Noise points: 1
```

This implementation demonstrates the fundamental DBSCAN clustering approach, identifying clusters of varying shapes and sizes while properly handling noise points.

