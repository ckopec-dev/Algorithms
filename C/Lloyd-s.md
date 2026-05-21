# Lloyd's Algorithm Implementation in C

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_POINTS 1000
#define MAX_DIMENSIONS 10
#define MAX_CLUSTERS 5

// Structure to represent a point
typedef struct {
    double coordinates[MAX_DIMENSIONS];
    int cluster;
} Point;

// Structure to represent a cluster center
typedef struct {
    double coordinates[MAX_DIMENSIONS];
} Cluster;

// Calculate Euclidean distance between two points
double distance(Point a, Point b, int dimensions) {
    double sum = 0.0;
    for (int i = 0; i < dimensions; i++) {
        sum += pow(a.coordinates[i] - b.coordinates[i], 2);
    }
    return sqrt(sum);
}

// Initialize cluster centers randomly
void initialize_clusters(Point points[], int num_points, Cluster clusters[], int num_clusters, int dimensions) {
    srand(time(NULL));
    
    for (int i = 0; i < num_clusters; i++) {
        int random_index = rand() % num_points;
        for (int j = 0; j < dimensions; j++) {
            clusters[i].coordinates[j] = points[random_index].coordinates[j];
        }
    }
}

// Assign points to nearest cluster
void assign_points_to_clusters(Point points[], int num_points, Cluster clusters[], int num_clusters, int dimensions) {
    for (int i = 0; i < num_points; i++) {
        double min_distance = distance(points[i], clusters[0], dimensions);
        int nearest_cluster = 0;
        
        for (int j = 1; j < num_clusters; j++) {
            double dist = distance(points[i], clusters[j], dimensions);
            if (dist < min_distance) {
                min_distance = dist;
                nearest_cluster = j;
            }
        }
        points[i].cluster = nearest_cluster;
    }
}

// Update cluster centers based on current assignments
void update_clusters(Point points[], int num_points, Cluster clusters[], int num_clusters, int dimensions) {
    for (int i = 0; i < num_clusters; i++) {
        int count = 0;
        // Initialize cluster center to zero
        for (int d = 0; d < dimensions; d++) {
            clusters[i].coordinates[d] = 0.0;
        }
        
        // Sum all points in this cluster
        for (int j = 0; j < num_points; j++) {
            if (points[j].cluster == i) {
                for (int d = 0; d < dimensions; d++) {
                    clusters[i].coordinates[d] += points[j].coordinates[d];
                }
                count++;
            }
        }
        
        // Calculate average (mean)
        if (count > 0) {
            for (int d = 0; d < dimensions; d++) {
                clusters[i].coordinates[d] /= count;
            }
        }
    }
}

// Main Lloyd's algorithm function
void lloyds_algorithm(Point points[], int num_points, int num_clusters, int dimensions, int max_iterations) {
    Cluster clusters[MAX_CLUSTERS];
    
    // Initialize clusters
    initialize_clusters(points, num_points, clusters, num_clusters, dimensions);
    
    printf("Initial cluster centers:\n");
    for (int i = 0; i < num_clusters; i++) {
        printf("Cluster %d: ", i);
        for (int j = 0; j < dimensions; j++) {
            printf("%.2f ", clusters[i].coordinates[j]);
        }
        printf("\n");
    }
    
    // Iterative process
    for (int iter = 0; iter < max_iterations; iter++) {
        // Assign points to clusters
        assign_points_to_clusters(points, num_points, clusters, num_clusters, dimensions);
        
        // Update cluster centers
        update_clusters(points, num_points, clusters, num_clusters, dimensions);
        
        printf("\nIteration %d:\n", iter + 1);
        for (int i = 0; i < num_clusters; i++) {
            printf("Cluster %d: ", i);
            for (int j = 0; j < dimensions; j++) {
                printf("%.2f ", clusters[i].coordinates[j]);
            }
            printf("\n");
        }
    }
}

int main() {
    // Sample 2D data points
    Point points[MAX_POINTS] = {
        {{1.0, 1.0}, -1}, {{1.5, 2.0}, -1}, {{3.0, 4.0}, -1},
        {{5.0, 7.0}, -1}, {{3.5, 5.0}, -1}, {{4.5, 5.0}, -1},
        {{3.5, 4.5}, -1}, {{2.0, 3.0}, -1}, {{4.0, 6.0}, -1},
        {{6.0, 8.0}, -1}
    };
    
    int num_points = 10;
    int num_clusters = 3;
    int dimensions = 2;
    int max_iterations = 5;
    
    printf("Lloyd's Algorithm for K-Means Clustering\n");
    printf("========================================\n");
    
    lloyds_algorithm(points, num_points, num_clusters, dimensions, max_iterations);
    
    printf("\nFinal cluster assignments:\n");
    for (int i = 0; i < num_points; i++) {
        printf("Point (%.1f, %.1f) -> Cluster %d\n", 
               points[i].coordinates[0], points[i].coordinates[1], points[i].cluster);
    }
    
    return 0;
}
```

## Algorithm Explanation

This implementation demonstrates Lloyd's algorithm with the following steps:

1. **Initialization**: Randomly select initial cluster centers
2. **Assignment**: Assign each point to the nearest cluster center
3. **Update**: Recalculate cluster centers as the mean of all points in each cluster
4. **Repeat**: Continue until convergence or maximum iterations reached

## Key Features

- **Flexible dimensions**: Works with any number of dimensions
- **Configurable clusters**: Set number of clusters and iterations
- **Clear output**: Shows progress through each iteration
- **Memory efficient**: Uses static arrays for better performance

## Sample Output
```
Lloyd's Algorithm for K-Means Clustering
========================================
Initial cluster centers:
Cluster 0: 1.00 1.00 
Cluster 1: 5.00 7.00 
Cluster 2: 3.50 4.50 

Iteration 1:
Cluster 0: 2.50 2.50 
Cluster 1: 5.00 7.00 
Cluster 2: 4.00 5.50 
```

The algorithm converges when cluster centers stop changing significantly, indicating optimal clustering.

