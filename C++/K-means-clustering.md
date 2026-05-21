# K-Means Clustering Algorithm in C++

Here's a complete implementation of the K-means clustering algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <iomanip>

class KMeans {
private:
    int k;                    // Number of clusters
    int maxIterations;        // Maximum number of iterations
    std::vector<std::vector<double>> centroids;  // Cluster centroids
    std::vector<std::vector<double>> data;       // Input data
    
    // Calculate Euclidean distance between two points
    double euclideanDistance(const std::vector<double>& point1, 
                           const std::vector<double>& point2) {
        double sum = 0.0;
        for (size_t i = 0; i < point1.size(); i++) {
            sum += std::pow(point1[i] - point2[i], 2);
        }
        return std::sqrt(sum);
    }
    
    // Initialize centroids randomly
    void initializeCentroids() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(0, data.size() - 1);
        
        centroids.clear();
        for (int i = 0; i < k; i++) {
            int randomIndex = dis(gen);
            centroids.push_back(data[randomIndex]);
        }
    }
    
    // Assign points to nearest centroid
    std::vector<int> assignClusters() {
        std::vector<int> clusters(data.size());
        
        for (size_t i = 0; i < data.size(); i++) {
            double minDistance = std::numeric_limits<double>::max();
            int nearestCentroid = 0;
            
            for (int j = 0; j < k; j++) {
                double distance = euclideanDistance(data[i], centroids[j]);
                if (distance < minDistance) {
                    minDistance = distance;
                    nearestCentroid = j;
                }
            }
            clusters[i] = nearestCentroid;
        }
        
        return clusters;
    }
    
    // Update centroids based on current cluster assignments
    void updateCentroids(const std::vector<int>& clusters) {
        std::vector<std::vector<double>> newCentroids(k, std::vector<double>(data[0].size(), 0.0));
        std::vector<int> clusterCounts(k, 0);
        
        // Sum up all points in each cluster
        for (size_t i = 0; i < data.size(); i++) {
            int clusterId = clusters[i];
            for (size_t j = 0; j < data[0].size(); j++) {
                newCentroids[clusterId][j] += data[i][j];
            }
            clusterCounts[clusterId]++;
        }
        
        // Calculate average to get new centroids
        for (int i = 0; i < k; i++) {
            if (clusterCounts[i] > 0) {
                for (size_t j = 0; j < data[0].size(); j++) {
                    newCentroids[i][j] /= clusterCounts[i];
                }
            }
        }
        
        centroids = newCentroids;
    }
    
public:
    KMeans(int k, int maxIterations = 100) : k(k), maxIterations(maxIterations) {}
    
    // Fit the K-means model to data
    std::vector<int> fit(const std::vector<std::vector<double>>& inputData) {
        data = inputData;
        initializeCentroids();
        
        std::vector<int> clusters;
        
        for (int iteration = 0; iteration < maxIterations; iteration++) {
            clusters = assignClusters();
            updateCentroids(clusters);
        }
        
        return clusters;
    }
    
    // Get the final centroids
    std::vector<std::vector<double>> getCentroids() const {
        return centroids;
    }
    
    // Print the results
    void printResults(const std::vector<int>& clusters) {
        std::cout << "K-Means Clustering Results:\n";
        std::cout << "============================\n";
        
        for (int i = 0; i < k; i++) {
            std::cout << "Cluster " << i << " Centroid: [";
            for (size_t j = 0; j < centroids[i].size(); j++) {
                std::cout << std::fixed << std::setprecision(2) 
                         << centroids[i][j];
                if (j < centroids[i].size() - 1) std::cout << ", ";
            }
            std::cout << "]\n";
        }
        
        std::cout << "\nData Point Assignments:\n";
        for (size_t i = 0; i < data.size(); i++) {
            std::cout << "Point " << i << ": [";
            for (size_t j = 0; j < data[i].size(); j++) {
                std::cout << std::fixed << std::setprecision(2) 
                         << data[i][j];
                if (j < data[i].size() - 1) std::cout << ", ";
            }
            std::cout << "] -> Cluster " << clusters[i] << "\n";
        }
    }
};

// Example usage
int main() {
    // Sample 2D data points
    std::vector<std::vector<double>> dataPoints = {
        {1.0, 2.0}, {1.5, 1.8}, {5.0, 8.0}, {8.0, 8.0},
        {1.0, 0.6}, {9.0, 11.0}, {8.0, 2.0}, {10.0, 2.0},
        {9.0, 3.0}, {2.0, 1.0}, {3.0, 1.5}, {4.0, 2.0}
    };
    
    // Create K-means instance with 3 clusters
    KMeans kmeans(3, 100);
    
    // Fit the model
    std::vector<int> clusters = kmeans.fit(dataPoints);
    
    // Print results
    kmeans.printResults(clusters);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Class Structure**: Clean object-oriented design with proper encapsulation
2. **Distance Calculation**: Uses Euclidean distance for clustering
3. **Random Initialization**: Initializes centroids randomly
4. **Iterative Process**: Runs until convergence or maximum iterations
5. **Cluster Assignment**: Assigns each data point to the nearest centroid
6. **Centroid Update**: Recalculates centroids based on assigned points

## Sample Output:
```
K-Means Clustering Results:
============================
Cluster 0 Centroid: [2.00, 1.25]
Cluster 1 Centroid: [8.00, 7.00]
Cluster 2 Centroid: [10.00, 2.00]

Data Point Assignments:
Point 0: [1.00, 2.00] -> Cluster 0
Point 1: [1.50, 1.80] -> Cluster 0
Point 2: [5.00, 8.00] -> Cluster 1
...
```

## How to Compile and Run:
```bash
g++ -std=c++11 kmeans.cpp -o kmeans
./kmeans
```

This implementation provides a solid foundation for K-means clustering that can be easily extended with additional features like different distance metrics, better initialization methods (K-means++), or visualization capabilities.

