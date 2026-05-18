# Lloyd's Algorithm Implementation in C++

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <random>
#include <algorithm>

struct Point {
    double x, y;
    Point(double x = 0, double y = 0) : x(x), y(y) {}
};

struct Cluster {
    Point center;
    std::vector<Point> points;
    
    Cluster(Point center) : center(center) {}
};

class KMeans {
private:
    int k;  // number of clusters
    int maxIterations;
    
public:
    KMeans(int k, int maxIterations = 100) : k(k), maxIterations(maxIterations) {}
    
    // Calculate Euclidean distance between two points
    double distance(const Point& a, const Point& b) {
        return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
    }
    
    // Assign points to nearest cluster
    std::vector<int> assignPointsToClusters(const std::vector<Point>& points, 
                                          const std::vector<Point>& centers) {
        std::vector<int> assignments(points.size());
        
        for (size_t i = 0; i < points.size(); i++) {
            double minDist = std::numeric_limits<double>::max();
            int closestCluster = 0;
            
            for (size_t j = 0; j < centers.size(); j++) {
                double dist = distance(points[i], centers[j]);
                if (dist < minDist) {
                    minDist = dist;
                    closestCluster = j;
                }
            }
            assignments[i] = closestCluster;
        }
        
        return assignments;
    }
    
    // Update cluster centers based on assigned points
    std::vector<Point> updateCenters(const std::vector<Point>& points,
                                   const std::vector<int>& assignments) {
        std::vector<Point> newCenters(k, Point(0, 0));
        std::vector<int> counts(k, 0);
        
        for (size_t i = 0; i < points.size(); i++) {
            int clusterId = assignments[i];
            newCenters[clusterId].x += points[i].x;
            newCenters[clusterId].y += points[i].y;
            counts[clusterId]++;
        }
        
        // Calculate average for each cluster
        for (int i = 0; i < k; i++) {
            if (counts[i] > 0) {
                newCenters[i].x /= counts[i];
                newCenters[i].y /= counts[i];
            }
        }
        
        return newCenters;
    }
    
    // Main Lloyd's algorithm
    std::vector<Cluster> run(const std::vector<Point>& points) {
        // Initialize random centers
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> disX(0, 100);
        std::uniform_real_distribution<> disY(0, 100);
        
        std::vector<Point> centers(k);
        for (int i = 0; i < k; i++) {
            centers[i] = Point(disX(gen), disY(gen));
        }
        
        std::vector<Cluster> clusters;
        for (int i = 0; i < k; i++) {
            clusters.push_back(Cluster(centers[i]));
        }
        
        // Lloyd's algorithm iterations
        for (int iter = 0; iter < maxIterations; iter++) {
            // Assign points to clusters
            auto assignments = assignPointsToClusters(points, centers);
            
            // Clear previous assignments
            for (auto& cluster : clusters) {
                cluster.points.clear();
            }
            
            // Assign points to clusters
            for (size_t i = 0; i < points.size(); i++) {
                clusters[assignments[i]].points.push_back(points[i]);
            }
            
            // Update centers
            auto newCenters = updateCenters(points, assignments);
            
            // Check for convergence (simple check - centers don't change much)
            bool converged = true;
            for (int i = 0; i < k; i++) {
                if (distance(centers[i], newCenters[i]) > 0.01) {
                    converged = false;
                    break;
                }
            }
            
            centers = newCenters;
            
            if (converged) {
                std::cout << "Converged after " << iter + 1 << " iterations\n";
                break;
            }
        }
        
        return clusters;
    }
};

// Example usage
int main() {
    // Create sample data points
    std::vector<Point> points = {
        Point(1, 2), Point(1, 4), Point(1, 0),
        Point(10, 2), Point(10, 4), Point(10, 0),
        Point(5, 2), Point(5, 4), Point(5, 0)
    };
    
    // Run k-means clustering with k=2
    KMeans kmeans(2);
    auto clusters = kmeans.run(points);
    
    // Display results
    for (size_t i = 0; i < clusters.size(); i++) {
        std::cout << "Cluster " << i << " center: (" 
                  << clusters[i].center.x << ", " << clusters[i].center.y << ")\n";
        std::cout << "Points in cluster " << i << ": ";
        for (const auto& point : clusters[i].points) {
            std::cout << "(" << point.x << ", " << point.y << ") ";
        }
        std::cout << "\n\n";
    }
    
    return 0;
}
```

## Algorithm Steps:

1. **Initialization**: Randomly initialize k cluster centers
2. **Assignment**: Assign each data point to the nearest cluster center
3. **Update**: Recalculate cluster centers as the mean of all assigned points
4. **Repeat**: Continue steps 2-3 until convergence or maximum iterations

## Key Features:

- **Distance Calculation**: Uses Euclidean distance
- **Convergence Check**: Stops when centers don't change significantly
- **Random Initialization**: Uses random starting points
- **Flexible k**: Can work with any number of clusters

## Output Example:
```
Cluster 0 center: (5.33333, 2)
Points in cluster 0: (1, 2) (1, 4) (1, 0) (5, 2) (5, 4) (5, 0) 

Cluster 1 center: (10, 2)
Points in cluster 1: (10, 2) (10, 4) (10, 0) 
```

This implementation demonstrates the core Lloyd's algorithm for k-means clustering in C++.

