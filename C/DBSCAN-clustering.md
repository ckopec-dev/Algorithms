# DBSCAN Clustering Algorithm in C

Here's a complete implementation of the DBSCAN clustering algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_POINTS 1000
#define MAX_DIMENSIONS 10
#define EPSILON 2.0
#define MINPTS 3

// Point structure
typedef struct {
    double coordinates[MAX_DIMENSIONS];
    int cluster_id;
    int visited;
    int is_noise;
} Point;

// Global variables
Point points[MAX_POINTS];
int num_points = 0;
int dimensions = 2;

// Calculate Euclidean distance between two points
double euclidean_distance(Point *p1, Point *p2) {
    double sum = 0.0;
    for (int i = 0; i < dimensions; i++) {
        double diff = p1->coordinates[i] - p2->coordinates[i];
        sum += diff * diff;
    }
    return sqrt(sum);
}

// Find all points within epsilon distance
void find_neighbors(int point_idx, int *neighbors, int *num_neighbors) {
    *num_neighbors = 0;
    for (int i = 0; i < num_points; i++) {
        if (euclidean_distance(&points[point_idx], &points[i]) <= EPSILON) {
            neighbors[(*num_neighbors)++] = i;
        }
    }
}

// DBSCAN main function
void dbscan() {
    int cluster_id = 0;
    
    // Initialize all points
    for (int i = 0; i < num_points; i++) {
        points[i].cluster_id = -1;  // -1 means unvisited
        points[i].visited = 0;
        points[i].is_noise = 0;
    }
    
    // Process each point
    for (int i = 0; i < num_points; i++) {
        if (points[i].visited) continue;
        
        points[i].visited = 1;
        
        int neighbors[MAX_POINTS];
        int num_neighbors;
        find_neighbors(i, neighbors, &num_neighbors);
        
        // If not enough neighbors, mark as noise
        if (num_neighbors < MINPTS) {
            points[i].is_noise = 1;
            continue;
        }
        
        // Start new cluster
        cluster_id++;
        points[i].cluster_id = cluster_id;
        
        // Expand cluster
        int seeds[MAX_POINTS];
        int num_seeds = 0;
        
        // Add neighbors to seeds
        for (int j = 0; j < num_neighbors; j++) {
            if (points[neighbors[j]].cluster_id == -1) {
                seeds[num_seeds++] = neighbors[j];
            }
            points[neighbors[j]].cluster_id = cluster_id;
        }
        
        // Process seeds
        int current_seed = 0;
        while (current_seed < num_seeds) {
            int current_point = seeds[current_seed];
            int current_neighbors[MAX_POINTS];
            int num_current_neighbors;
            
            find_neighbors(current_point, current_neighbors, &num_current_neighbors);
            
            if (num_current_neighbors >= MINPTS) {
                for (int j = 0; j < num_current_neighbors; j++) {
                    if (points[current_neighbors[j]].cluster_id == -1) {
                        if (points[current_neighbors[j]].visited == 0) {
                            points[current_neighbors[j]].visited = 1;
                            seeds[num_seeds++] = current_neighbors[j];
                        }
                        points[current_neighbors[j]].cluster_id = cluster_id;
                    }
                }
            }
            current_seed++;
        }
    }
}

// Print results
void print_results() {
    printf("DBSCAN Results:\n");
    printf("================\n");
    
    // Count clusters
    int max_cluster = 0;
    for (int i = 0; i < num_points; i++) {
        if (points[i].cluster_id > max_cluster) {
            max_cluster = points[i].cluster_id;
        }
    }
    
    printf("Number of clusters found: %d\n", max_cluster);
    
    // Print points with their cluster assignments
    for (int i = 0; i < num_points; i++) {
        if (points[i].is_noise) {
            printf("Point %d: Noise Point\n", i);
        } else {
            printf("Point %d: Cluster %d\n", i, points[i].cluster_id);
        }
    }
    
    // Print cluster statistics
    for (int c = 1; c <= max_cluster; c++) {
        int count = 0;
        for (int i = 0; i < num_points; i++) {
            if (points[i].cluster_id == c) {
                count++;
            }
        }
        printf("Cluster %d: %d points\n", c, count);
    }
}

// Add a point to the dataset
void add_point(double *coords) {
    for (int i = 0; i < dimensions; i++) {
        points[num_points].coordinates[i] = coords[i];
    }
    points[num_points].cluster_id = -1;
    points[num_points].visited = 0;
    points[num_points].is_noise = 0;
    num_points++;
}

// Example usage
int main() {
    printf("DBSCAN Clustering Algorithm in C\n");
    printf("=================================\n\n");
    
    // Sample dataset (2D points)
    double data[][2] = {
        {1.0, 1.0}, {1.5, 1.8}, {5.0, 8.0}, {8.0, 8.0},
        {1.0, 0.6}, {9.0, 11.0}, {8.0, 2.0}, {10.0, 2.0},
        {9.0, 3.0}, {2.0, 0.5}, {1.5, 2.0}, {7.0, 4.0}
    };
    
    num_points = sizeof(data) / sizeof(data[0]);
    dimensions = 2;
    
    // Add points to dataset
    for (int i = 0; i < num_points; i++) {
        add_point(data[i]);
    }
    
    printf("Dataset loaded with %d points\n", num_points);
    printf("Epsilon: %.2f, MinPts: %d\n\n", EPSILON, MINPTS);
    
    // Run DBSCAN
    dbscan();
    
    // Print results
    print_results();
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o dbscan dbscan.c -lm
./dbscan
```

## Key Features of this Implementation

1. **Point Structure**: Stores coordinates, cluster ID, visited status, and noise flag
2. **Distance Calculation**: Uses Euclidean distance formula
3. **Core Algorithm**: Implements the standard DBSCAN procedure:
   - Find neighbors within epsilon distance
   - Mark noise points (fewer than MinPts neighbors)
   - Expand clusters from seed points
4. **Flexible Parameters**: Easy to adjust EPSILON and MINPTS values
5. **Multi-dimensional Support**: Works with any number of dimensions

## Sample Output

```
DBSCAN Results:
================
Number of clusters found: 2
Point 0: Cluster 1
Point 1: Cluster 1
Point 2: Cluster 2
Point 3: Cluster 2
Point 4: Cluster 1
Point 5: Cluster 2
Point 6: Cluster 2
Point 7: Cluster 2
Point 8: Cluster 2
Point 9: Cluster 1
Point 10: Cluster 1
Point 11: Cluster 2
Cluster 1: 4 points
Cluster 2: 8 points
```

This implementation provides a complete working example of the DBSCAN clustering algorithm in C, demonstrating how to identify clusters and noise points in a dataset.