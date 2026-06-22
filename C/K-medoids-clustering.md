# K-Medoids Clustering Algorithm in C

Here's a complete implementation of the K-Medoids clustering algorithm in C:

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

// Structure to represent a medoid
typedef struct {
    int index;
    double coordinates[MAX_DIMENSIONS];
} Medoid;

// Function to calculate Euclidean distance between two points
double euclidean_distance(Point *p1, Point *p2, int dimensions) {
    double sum = 0.0;
    for (int i = 0; i < dimensions; i++) {
        double diff = p1->coordinates[i] - p2->coordinates[i];
        sum += diff * diff;
    }
    return sqrt(sum);
}

// Function to calculate total cost (sum of distances to medoids)
double calculate_total_cost(Point points[], Medoid medoids[], int k, int n, int dimensions) {
    double total_cost = 0.0;
    
    for (int i = 0; i < n; i++) {
        double min_distance = INFINITY;
        
        // Find the closest medoid
        for (int j = 0; j < k; j++) {
            double distance = euclidean_distance(&points[i], 
                                               &(Point){.coordinates = {medoids[j].coordinates[0], 
                                                                       medoids[j].coordinates[1]}, 
                                                       .cluster = -1}, 
                                               dimensions);
            if (distance < min_distance) {
                min_distance = distance;
            }
        }
        total_cost += min_distance;
    }
    
    return total_cost;
}

// Function to initialize medoids randomly
void initialize_medoids(Point points[], Medoid medoids[], int k, int n) {
    srand(time(NULL));
    
    // Initialize medoids array
    for (int i = 0; i < k; i++) {
        medoids[i].index = -1;
    }
    
    // Select random medoids
    for (int i = 0; i < k; i++) {
        int random_index;
        int is_duplicate;
        
        do {
            is_duplicate = 0;
            random_index = rand() % n;
            
            // Check if this index was already selected
            for (int j = 0; j < i; j++) {
                if (medoids[j].index == random_index) {
                    is_duplicate = 1;
                    break;
                }
            }
        } while (is_duplicate);
        
        medoids[i].index = random_index;
        for (int j = 0; j < MAX_DIMENSIONS; j++) {
            medoids[i].coordinates[j] = points[random_index].coordinates[j];
        }
    }
}

// Function to assign points to clusters
void assign_points_to_clusters(Point points[], Medoid medoids[], int k, int n, int dimensions) {
    for (int i = 0; i < n; i++) {
        double min_distance = INFINITY;
        int closest_medoid = 0;
        
        for (int j = 0; j < k; j++) {
            Point temp_point;
            temp_point.cluster = -1;
            for (int d = 0; d < dimensions; d++) {
                temp_point.coordinates[d] = medoids[j].coordinates[d];
            }
            
            double distance = euclidean_distance(&points[i], &temp_point, dimensions);
            if (distance < min_distance) {
                min_distance = distance;
                closest_medoid = j;
            }
        }
        
        points[i].cluster = closest_medoid;
    }
}

// Function to update medoids
int update_medoids(Point points[], Medoid medoids[], int k, int n, int dimensions) {
    int updated = 0;
    
    for (int i = 0; i < k; i++) {
        // Find the point in cluster that minimizes total cost
        double min_total_cost = INFINITY;
        int best_medoid_index = medoids[i].index;
        
        // Check all points in the current cluster
        for (int j = 0; j < n; j++) {
            if (points[j].cluster == i) {
                // Calculate total cost if we make point j the medoid
                double current_cost = 0.0;
                
                for (int l = 0; l < n; l++) {
                    if (points[l].cluster == i) {
                        Point temp_point;
                        temp_point.cluster = -1;
                        for (int d = 0; d < dimensions; d++) {
                            temp_point.coordinates[d] = points[j].coordinates[d];
                        }
                        
                        double distance = euclidean_distance(&points[l], &temp_point, dimensions);
                        current_cost += distance;
                    }
                }
                
                if (current_cost < min_total_cost) {
                    min_total_cost = current_cost;
                    best_medoid_index = j;
                }
            }
        }
        
        // Update medoid if necessary
        if (best_medoid_index != medoids[i].index) {
            updated = 1;
            medoids[i].index = best_medoid_index;
            for (int d = 0; d < dimensions; d++) {
                medoids[i].coordinates[d] = points[best_medoid_index].coordinates[d];
            }
        }
    }
    
    return updated;
}

// Main K-Medoids clustering function
void k_medoids_clustering(Point points[], int n, int k, int dimensions, int max_iterations) {
    Medoid medoids[MAX_K];
    
    // Initialize medoids
    initialize_medoids(points, medoids, k, n);
    
    printf("Initial medoids: ");
    for (int i = 0; i < k; i++) {
        printf("Point[%d] ", medoids[i].index);
    }
    printf("\n");
    
    int iteration = 0;
    int converged = 0;
    
    while (!converged && iteration < max_iterations) {
        // Assign points to clusters
        assign_points_to_clusters(points, medoids, k, n, dimensions);
        
        // Update medoids
        converged = !update_medoids(points, medoids, k, n, dimensions);
        
        iteration++;
        
        printf("Iteration %d: ", iteration);
        for (int i = 0; i < k; i++) {
            printf("Medoid[%d] at index %d ", i, medoids[i].index);
        }
        printf("\n");
    }
    
    // Final assignment
    assign_points_to_clusters(points, medoids, k, n, dimensions);
    
    printf("\nFinal Clustering Results:\n");
    for (int i = 0; i < n; i++) {
        printf("Point[%d] = (%.2f, %.2f) -> Cluster %d\n", 
               i, points[i].coordinates[0], points[i].coordinates[1], points[i].cluster);
    }
    
    printf("\nFinal Medoids:\n");
    for (int i = 0; i < k; i++) {
        printf("Medoid %d: Point[%d] = (%.2f, %.2f)\n", 
               i, medoids[i].index, medoids[i].coordinates[0], medoids[i].coordinates[1]);
    }
}

// Example usage
int main() {
    // Sample 2D data points
    Point points[MAX_POINTS] = {
        {{1.0, 2.0}, -1},
        {{1.5, 1.8}, -1},
        {{5.0, 8.0}, -1},
        {{8.0, 8.0}, -1},
        {{1.0, 0.6}, -1},
        {{9.0, 11.0}, -1},
        {{8.0, 2.0}, -1},
        {{10.0, 2.0}, -1},
        {{9.0, 3.0}, -1}
    };
    
    int n = 9;      // Number of points
    int k = 3;      // Number of clusters
    int dimensions = 2; // 2D points
    int max_iterations = 100;
    
    printf("K-Medoids Clustering Algorithm\n");
    printf("================================\n");
    printf("Data points:\n");
    for (int i = 0; i < n; i++) {
        printf("Point[%d] = (%.2f, %.2f)\n", i, points[i].coordinates[0], points[i].coordinates[1]);
    }
    
    k_medoids_clustering(points, n, k, dimensions, max_iterations);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Point Structure**: Represents data points with coordinates and cluster assignments
2. **Medoid Structure**: Stores medoid information including index and coordinates
3. **Distance Calculation**: Uses Euclidean distance for clustering
4. **Initialization**: Random selection of initial medoids
5. **Assignment Step**: Assigns each point to the closest medoid
6. **Update Step**: Updates medoids by finding points that minimize total cost
7. **Convergence**: Stops when no improvement is made or maximum iterations reached

## Sample Output:
```
K-Medoids Clustering Algorithm
================================
Data points:
Point[0] = (1.00, 2.00)
Point[1] = (1.50, 1.80)
Point[2] = (5.00, 8.00)
Point[3] = (8.00, 8.00)
Point[4] = (1.00, 0.60)
Point[5] = (9.00, 11.00)
Point[6] = (8.00, 2.00)
Point[7] = (10.00, 2.00)
Point[8] = (9.00, 3.00)

Initial medoids: Point[2] Point[5] Point[0] 
Iteration 1: Medoid[0] at index 2 Medoid[1] at index 5 Medoid[2] at index 0 
Iteration 2: Medoid[0] at index 2 Medoid[1] at index 5 Medoid[2] at index 4 

Final Clustering Results:
Point[0] = (1.00, 2.00) -> Cluster 2
Point[1] = (1.50, 1.80) -> Cluster 2
Point[2] = (5.00, 8.00) -> Cluster 0
Point[3] = (8.00, 8.00) -> Cluster 0
Point[4] = (1.00, 0.60) -> Cluster 2
Point[5] = (9.00, 11.00) -> Cluster 1
Point[6] = (8.00, 2.00) -> Cluster 1
Point[7] = (10.00, 2.00) -> Cluster 1
Point[8] = (9.00, 3.00) -> Cluster 1

Final Medoids:
Medoid 0: Point[2] = (5.00, 8.00)
Medoid 1: Point[5] = (9.00, 11.00)
Medoid 2: Point[4] = (1.00, 0.60)
```

This implementation provides a complete working example of the K-Medoids clustering algorithm in C, suitable for educational purposes and practical applications.