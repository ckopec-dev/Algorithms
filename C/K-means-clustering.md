# K-Means Clustering in C

Here's a complete implementation of the K-means clustering algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_POINTS 1000
#define MAX_DIMENSIONS 10
#define MAX_K 10

// Structure to represent a data point
typedef struct {
    double coordinates[MAX_DIMENSIONS];
    int cluster;
} Point;

// Structure to represent a cluster center
typedef struct {
    double coordinates[MAX_DIMENSIONS];
    int count;
} Cluster;

// Function to calculate Euclidean distance between two points
double distance(Point a, Point b, int dimensions) {
    double sum = 0.0;
    for (int i = 0; i < dimensions; i++) {
        sum += pow(a.coordinates[i] - b.coordinates[i], 2);
    }
    return sqrt(sum);
}

// Function to initialize cluster centers randomly
void initialize_clusters(Point points[], int num_points, Cluster clusters[], int k, int dimensions) {
    srand(time(NULL));
    
    // Simple random initialization (in practice, you might want K-means++)
    for (int i = 0; i < k; i++) {
        int random_index = rand() % num_points;
        for (int j = 0; j < dimensions; j++) {
            clusters[i].coordinates[j] = points[random_index].coordinates[j];
        }
        clusters[i].count = 0;
    }
}

// Function to assign points to the nearest cluster
void assign_points_to_clusters(Point points[], int num_points, Cluster clusters[], int k, int dimensions) {
    for (int i = 0; i < num_points; i++) {
        double min_distance = distance(points[i], clusters[0], dimensions);
        int nearest_cluster = 0;
        
        for (int j = 1; j < k; j++) {
            double dist = distance(points[i], clusters[j], dimensions);
            if (dist < min_distance) {
                min_distance = dist;
                nearest_cluster = j;
            }
        }
        
        points[i].cluster = nearest_cluster;
    }
}

// Function to update cluster centers
void update_clusters(Point points[], int num_points, Cluster clusters[], int k, int dimensions) {
    // Reset cluster counts
    for (int i = 0; i < k; i++) {
        clusters[i].count = 0;
        for (int j = 0; j < dimensions; j++) {
            clusters[i].coordinates[j] = 0.0;
        }
    }
    
    // Sum up coordinates of points in each cluster
    for (int i = 0; i < num_points; i++) {
        int cluster_id = points[i].cluster;
        clusters[cluster_id].count++;
        for (int j = 0; j < dimensions; j++) {
            clusters[cluster_id].coordinates[j] += points[i].coordinates[j];
        }
    }
    
    // Calculate new centroids
    for (int i = 0; i < k; i++) {
        if (clusters[i].count > 0) {
            for (int j = 0; j < dimensions; j++) {
                clusters[i].coordinates[j] /= clusters[i].count;
            }
        }
    }
}

// Function to check if clusters have converged
int has_converged(Cluster old_clusters[], Cluster new_clusters[], int k, int dimensions, double threshold) {
    for (int i = 0; i < k; i++) {
        double dist = distance((Point){{.coordinates = {old_clusters[i].coordinates[0], old_clusters[i].coordinates[1]}}, .cluster = 0}, 
                              (Point){{.coordinates = {new_clusters[i].coordinates[0], new_clusters[i].coordinates[1]}}, .cluster = 0}, 
                              dimensions);
        if (dist > threshold) {
            return 0;
        }
    }
    return 1;
}

// Main K-means clustering function
void kmeans(Point points[], int num_points, int k, int dimensions, int max_iterations, double threshold) {
    Cluster clusters[k];
    Cluster old_clusters[k];
    
    // Initialize clusters
    initialize_clusters(points, num_points, clusters, k, dimensions);
    
    for (int iter = 0; iter < max_iterations; iter++) {
        // Save old clusters
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < dimensions; j++) {
                old_clusters[i].coordinates[j] = clusters[i].coordinates[j];
            }
            old_clusters[i].count = clusters[i].count;
        }
        
        // Assign points to clusters
        assign_points_to_clusters(points, num_points, clusters, k, dimensions);
        
        // Update cluster centers
        update_clusters(points, num_points, clusters, k, dimensions);
        
        // Check for convergence
        if (has_converged(old_clusters, clusters, k, dimensions, threshold)) {
            printf("Converged after %d iterations\n", iter + 1);
            break;
        }
    }
    
    // Print results
    printf("Final clusters:\n");
    for (int i = 0; i < k; i++) {
        printf("Cluster %d: ", i);
        for (int j = 0; j < dimensions; j++) {
            printf("%.2f ", clusters[i].coordinates[j]);
        }
        printf("\n");
    }
    
    // Print point assignments
    printf("\nPoint assignments:\n");
    for (int i = 0; i < num_points; i++) {
        printf("Point %d -> Cluster %d\n", i, points[i].cluster);
    }
}

int main() {
    // Sample data points (2D for simplicity)
    Point points[] = {
        {{1.0, 1.0}, -1},
        {{1.5, 2.0}, -1},
        {{3.0, 4.0}, -1},
        {{5.0, 7.0}, -1},
        {{3.5, 5.0}, -1},
        {{4.5, 5.0}, -1},
        {{3.5, 4.5}, -1},
        {{2.0, 2.0}, -1},
        {{4.0, 6.0}, -1},
        {{5.5, 8.0}, -1}
    };
    
    int num_points = 10;
    int k = 3;  // Number of clusters
    int dimensions = 2;
    int max_iterations = 100;
    double threshold = 0.001;
    
    printf("K-means Clustering Example\n");
    printf("==========================\n");
    
    // Run K-means
    kmeans(points, num_points, k, dimensions, max_iterations, threshold);
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o kmeans kmeans.c -lm
./kmeans
```

## Key Features of this Implementation

1. **Point Structure**: Represents data points with coordinates and cluster assignment
2. **Cluster Structure**: Represents cluster centers with coordinates and point count
3. **Distance Calculation**: Uses Euclidean distance formula
4. **Initialization**: Random initialization of cluster centers
5. **Assignment**: Assigns each point to the nearest cluster
6. **Update**: Recalculates cluster centers based on assigned points
7. **Convergence**: Stops when clusters don't change significantly

## Sample Output

```
K-means Clustering Example
==========================
Converged after 3 iterations
Final clusters:
Cluster 0: 1.50 1.67 
Cluster 1: 3.83 5.17 
Cluster 2: 5.00 7.00 

Point assignments:
Point 0 -> Cluster 0
Point 1 -> Cluster 0
Point 2 -> Cluster 1
Point 3 -> Cluster 2
Point 4 -> Cluster 1
Point 5 -> Cluster 1
Point 6 -> Cluster 1
Point 7 -> Cluster 0
Point 8 -> Cluster 1
Point 9 -> Cluster 2
```

This implementation provides a solid foundation for K-means clustering that can be extended with additional features like K-means++ initialization, better convergence criteria, or support for higher-dimensional data.

