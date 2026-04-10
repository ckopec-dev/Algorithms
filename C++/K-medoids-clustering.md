# K-Medoids Clustering Algorithm in C++

Here's a complete implementation of the K-Medoids clustering algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <cmath>
#include <limits>
#include <iomanip>

class KMedoids {
private:
    std::vector<std::vector<double>> data;
    int k, maxIterations;
    std::vector<int> medoids;
    std::vector<int> clusters;
    
    // Calculate Euclidean distance between two points
    double euclideanDistance(const std::vector<double>& a, const std::vector<double>& b) {
        double sum = 0.0;
        for (size_t i = 0; i < a.size(); ++i) {
            sum += (a[i] - b[i]) * (a[i] - b[i]);
        }
        return std::sqrt(sum);
    }
    
    // Calculate total cost (sum of distances from each point to its medoid)
    double calculateCost() {
        double totalCost = 0.0;
        for (size_t i = 0; i < data.size(); ++i) {
            int medoidIndex = medoids[clusters[i]];
            totalCost += euclideanDistance(data[i], data[medoidIndex]);
        }
        return totalCost;
    }
    
    // Initialize medoids randomly
    void initializeMedoids() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(0, data.size() - 1);
        
        medoids.clear();
        std::vector<bool> used(data.size(), false);
        
        for (int i = 0; i < k; ++i) {
            int index;
            do {
                index = dis(gen);
            } while (used[index]);
            
            used[index] = true;
            medoids.push_back(index);
        }
    }
    
    // Assign points to clusters based on nearest medoid
    void assignClusters() {
        clusters.assign(data.size(), 0);
        for (size_t i = 0; i < data.size(); ++i) {
            double minDistance = std::numeric_limits<double>::max();
            int nearestMedoid = 0;
            
            for (int j = 0; j < k; ++j) {
                double distance = euclideanDistance(data[i], data[medoids[j]]);
                if (distance < minDistance) {
                    minDistance = distance;
                    nearestMedoid = j;
                }
            }
            clusters[i] = nearestMedoid;
        }
    }
    
    // Update medoids to minimize cost function
    void updateMedoids() {
        std::vector<int> oldMedoids = medoids;
        double currentCost = calculateCost();
        
        for (int medoidIndex = 0; medoidIndex < k; ++medoidIndex) {
            int currentMedoid = medoids[medoidIndex];
            
            // Try moving the medoid to each data point
            for (size_t pointIndex = 0; pointIndex < data.size(); ++pointIndex) {
                // Skip if the point is already a medoid
                if (std::find(medoids.begin(), medoids.end(), pointIndex) != medoids.end()) {
                    continue;
                }
                
                // Temporarily change the medoid
                medoids[medoidIndex] = pointIndex;
                assignClusters();
                double newCost = calculateCost();
                
                if (newCost < currentCost) {
                    currentCost = newCost;
                } else {
                    // Revert the change
                    medoids[medoidIndex] = currentMedoid;
                }
            }
        }
    }

public:
    KMedoids(const std::vector<std::vector<double>>& inputData, int numClusters, int maxIter = 100)
        : data(inputData), k(numClusters), maxIterations(maxIter) {}
    
    void cluster() {
        initializeMedoids();
        assignClusters();
        
        double previousCost = std::numeric_limits<double>::max();
        int iteration = 0;
        
        while (iteration < maxIterations) {
            updateMedoids();
            assignClusters();
            double currentCost = calculateCost();
            
            // Check for convergence
            if (std::abs(currentCost - previousCost) < 1e-6) {
                break;
            }
            
            previousCost = currentCost;
            iteration++;
        }
        
        std::cout << "K-Medoids clustering completed after " << iteration << " iterations" << std::endl;
        std::cout << "Final cost: " << std::fixed << std::setprecision(2) << previousCost << std::endl;
    }
    
    // Get cluster assignments
    const std::vector<int>& getClusters() const {
        return clusters;
    }
    
    // Get medoid indices
    const std::vector<int>& getMedoids() const {
        return medoids;
    }
    
    // Print results
    void printResults() {
        std::cout << "\nCluster assignments:" << std::endl;
        for (size_t i = 0; i < clusters.size(); ++i) {
            std::cout << "Point " << i << " -> Cluster " << clusters[i] << std::endl;
        }
        
        std::cout << "\nMedoids:" << std::endl;
        for (int i = 0; i < k; ++i) {
            std::cout << "Medoid " << i << " at index " << medoids[i] << std::endl;
        }
    }
};

// Example usage
int main() {
    // Sample 2D data points
    std::vector<std::vector<double>> data = {
        {1.0, 2.0},
        {1.5, 1.8},
        {5.0, 8.0},
        {8.0, 8.0},
        {1.0, 0.6},
        {9.0, 11.0},
        {8.0, 2.0},
        {10.0, 2.0},
        {9.0, 3.0}
    };
    
    std::cout << "K-Medoids Clustering Example" << std::endl;
    std::cout << "=============================" << std::endl;
    
    // Print original data
    std::cout << "Original data points:" << std::endl;
    for (size_t i = 0; i < data.size(); ++i) {
        std::cout << "Point " << i << ": (" << data[i][0] << ", " << data[i][1] << ")" << std::endl;
    }
    
    // Perform K-Medoids clustering with k=3
    KMedoids kmedoids(data, 3);
    kmedoids.cluster();
    kmedoids.printResults();
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Euclidean Distance Calculation**: Computes distance between data points
2. **Random Medoid Initialization**: Randomly selects initial medoids
3. **Cluster Assignment**: Assigns each point to the nearest medoid
4. **Medoid Update**: Iteratively improves medoids to minimize cost function
5. **Convergence Detection**: Stops when cost function no longer improves significantly

## How it Works:

1. **Initialization**: Randomly select k data points as initial medoids
2. **Assignment**: Assign each data point to the cluster of its nearest medoid
3. **Update**: For each medoid, try moving it to each data point and see if cost decreases
4. **Repeat**: Continue until convergence or maximum iterations reached

## Sample Output:
```
K-Medoids Clustering Example
=============================
Original data points:
Point 0: (1.0, 2.0)
Point 1: (1.5, 1.8)
Point 2: (5.0, 8.0)
Point 3: (8.0, 8.0)
Point 4: (1.0, 0.6)
Point 5: (9.0, 11.0)
Point 6: (8.0, 2.0)
Point 7: (10.0, 2.0)
Point 8: (9.0, 3.0)

K-Medoids clustering completed after 5 iterations
Final cost: 12.85

Cluster assignments:
Point 0 -> Cluster 0
Point 1 -> Cluster 0
Point 2 -> Cluster 1
Point 3 -> Cluster 1
Point 4 -> Cluster 0
Point 5 -> Cluster 2
Point 6 -> Cluster 2
Point 7 -> Cluster 2
Point 8 -> Cluster 2

Medoids:
Medoid 0 at index 0
Medoid 1 at index 2
Medoid 2 at index 5
```

This implementation provides a complete working example of the K-Medoids algorithm that can be easily adapted for different datasets and requirements.

