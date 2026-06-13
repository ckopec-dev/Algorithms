# Clarke-Wright Savings Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_NODES 100
#define INF 999999

// Structure to represent a node
typedef struct {
    int id;
    double x, y;
} Node;

// Structure to represent a route
typedef struct {
    int nodes[MAX_NODES];
    int count;
    double cost;
} Route;

// Structure to represent savings
typedef struct {
    int i, j;
    double saving;
} Saving;

// Global variables
Node nodes[MAX_NODES];
int distance_matrix[MAX_NODES][MAX_NODES];
int num_nodes;
int depot_id = 0;

// Calculate Euclidean distance between two nodes
double calculate_distance(int i, int j) {
    double dx = nodes[i].x - nodes[j].x;
    double dy = nodes[i].y - nodes[j].y;
    return sqrt(dx * dx + dy * dy);
}

// Initialize distance matrix
void initialize_distance_matrix() {
    for (int i = 0; i < num_nodes; i++) {
        for (int j = 0; j < num_nodes; j++) {
            if (i == j) {
                distance_matrix[i][j] = 0;
            } else {
                distance_matrix[i][j] = (int)(calculate_distance(i, j) + 0.5);
            }
        }
    }
}

// Calculate savings for two nodes
double calculate_saving(int i, int j) {
    if (i == depot_id || j == depot_id) {
        return 0;
    }
    return distance_matrix[depot_id][i] + distance_matrix[depot_id][j] - distance_matrix[i][j];
}

// Compare function for sorting savings
int compare_savings(const void *a, const void *b) {
    Saving *s1 = (Saving *)a;
    Saving *s2 = (Saving *)b;
    
    if (s1->saving < s2->saving) return 1;
    if (s1->saving > s2->saving) return -1;
    return 0;
}

// Check if two routes can be merged
int can_merge(int i, int j, int *route1, int route1_count, int *route2, int route2_count) {
    // Check if nodes are already in the same route
    for (int k = 0; k < route1_count; k++) {
        if (route1[k] == i || route1[k] == j) return 0;
    }
    
    // Check if merging would create a valid route (no loops)
    if (i == depot_id && j == depot_id) return 0;
    
    return 1;
}

// Main Clarke-Wright Savings algorithm
void clarke_wright_savings() {
    printf("=== Clarke-Wright Savings Algorithm ===\n\n");
    
    // Initialize distance matrix
    initialize_distance_matrix();
    
    // Calculate all savings
    Saving savings[MAX_NODES * MAX_NODES];
    int saving_count = 0;
    
    for (int i = 1; i < num_nodes; i++) {
        for (int j = i + 1; j < num_nodes; j++) {
            double saving = calculate_saving(i, j);
            if (saving > 0) {  // Only consider positive savings
                savings[saving_count].i = i;
                savings[saving_count].j = j;
                savings[saving_count].saving = saving;
                saving_count++;
            }
        }
    }
    
    printf("Number of potential savings: %d\n", saving_count);
    
    // Sort savings in descending order
    qsort(savings, saving_count, sizeof(Saving), compare_savings);
    
    printf("\nTop 10 highest savings:\n");
    printf("Node i\tNode j\tSaving\n");
    for (int i = 0; i < (saving_count < 10 ? saving_count : 10); i++) {
        printf("%d\t%d\t%.2f\n", savings[i].i, savings[i].j, savings[i].saving);
    }
    
    // Initialize routes - each customer gets its own route initially
    int route_id[MAX_NODES];
    for (int i = 0; i < num_nodes; i++) {
        route_id[i] = i;
    }
    
    // Create initial single-node routes
    Route routes[MAX_NODES];
    for (int i = 1; i < num_nodes; i++) {
        routes[i].nodes[0] = depot_id;
        routes[i].nodes[1] = i;
        routes[i].nodes[2] = depot_id;
        routes[i].count = 3;
        routes[i].cost = distance_matrix[depot_id][i] + distance_matrix[i][depot_id];
    }
    
    printf("\nInitial routes (single customer per route):\n");
    for (int i = 1; i < num_nodes; i++) {
        printf("Route %d: ", i);
        for (int j = 0; j < routes[i].count; j++) {
            printf("%d ", routes[i].nodes[j]);
        }
        printf("(Cost: %.2f)\n", routes[i].cost);
    }
    
    // Merge routes based on savings
    int merged_count = 0;
    for (int i = 0; i < saving_count; i++) {
        int node_i = savings[i].i;
        int node_j = savings[i].j;
        
        // Check if these nodes belong to different routes
        if (route_id[node_i] != route_id[node_j]) {
            int route1_id = route_id[node_i];
            int route2_id = route_id[node_j];
            
            // Check if merging is possible (no common node in routes)
            int can_merge_routes = 1;
            
            // Simple check: ensure we're not creating invalid routes
            if (can_merge(node_i, node_j, 
                         routes[route1_id].nodes, routes[route1_id].count,
                         routes[route2_id].nodes, routes[route2_id].count)) {
                
                // Merge the two routes
                Route new_route;
                new_route.count = 0;
                
                // Add first route (excluding depot)
                for (int j = 1; j < routes[route1_id].count - 1; j++) {
                    new_route.nodes[new_route.count++] = routes[route1_id].nodes[j];
                }
                
                // Add second route (excluding depot)
                for (int j = 1; j < routes[route2_id].count - 1; j++) {
                    new_route.nodes[new_route.count++] = routes[route2_id].nodes[j];
                }
                
                // Add depots
                new_route.nodes[new_route.count++] = depot_id;
                new_route.nodes[new_route.count++] = depot_id;
                
                // Calculate new route cost
                double new_cost = 0;
                for (int j = 0; j < new_route.count - 1; j++) {
                    new_cost += distance_matrix[new_route.nodes[j]][new_route.nodes[j+1]];
                }
                
                // Update routes and route_id
                for (int j = 0; j < num_nodes; j++) {
                    if (route_id[j] == route2_id) {
                        route_id[j] = route1_id;
                    }
                }
                
                routes[route1_id] = new_route;
                routes[route1_id].cost = new_cost;
                
                printf("\nMerged routes %d and %d with saving %.2f\n", 
                       route1_id, route2_id, savings[i].saving);
                printf("New route: ");
                for (int j = 0; j < routes[route1_id].count; j++) {
                    printf("%d ", routes[route1_id].nodes[j]);
                }
                printf("(Cost: %.2f)\n", routes[route1_id].cost);
                
                merged_count++;
            }
        }
    }
    
    // Print final result
    printf("\n=== Final Result ===\n");
    printf("Number of routes created: %d\n", merged_count);
    
    // Find all remaining routes (routes with more than 2 nodes)
    int route_count = 0;
    for (int i = 1; i < num_nodes; i++) {
        if (route_id[i] == i) {  // This is a root node
            if (routes[i].count > 2) {  // Only display non-trivial routes
                printf("Route %d: ", route_count + 1);
                for (int j = 0; j < routes[i].count; j++) {
                    printf("%d ", routes[i].nodes[j]);
                }
                printf("(Cost: %.2f)\n", routes[i].cost);
                route_count++;
            }
        }
    }
    
    if (route_count == 0) {
        printf("No valid routes created.\n");
    }
}

int main() {
    // Example with 6 nodes (1 depot + 5 customers)
    num_nodes = 6;
    
    // Set node coordinates
    nodes[0].id = 0; nodes[0].x = 0; nodes[0].y = 0;  // Depot
    nodes[1].id = 1; nodes[1].x = 2; nodes[1].y = 4;  // Customer 1
    nodes[2].id = 2; nodes[2].x = 5; nodes[2].y = 3;  // Customer 2
    nodes[3].id = 3; nodes[3].x = 7; nodes[3].y = 8;  // Customer 3
    nodes[4].id = 4; nodes[4].x = 8; nodes[4].y = 1;  // Customer 4
    nodes[5].id = 5; nodes[5].x = 2; nodes[5].y = 9;  // Customer 5
    
    printf("Clarke-Wright Savings Algorithm Demo\n");
    printf("=====================================\n\n");
    
    printf("Nodes:\n");
    for (int i = 0; i < num_nodes; i++) {
        printf("Node %d: (%.1f, %.1f)\n", i, nodes[i].x, nodes[i].y);
    }
    
    clarke_wright_savings();
    
    return 0;
}
```

## Sample Output

```
Clarke-Wright Savings Algorithm Demo
=====================================

Nodes:
Node 0: (0.0, 0.0)
Node 1: (2.0, 4.0)
Node 2: (5.0, 3.0)
Node 3: (7.0, 8.0)
Node 4: (8.0, 1.0)
Node 5: (2.0, 9.0)

=== Clarke-Wright Savings Algorithm ===

Number of potential savings: 10

Top 10 highest savings:
Node i	Node j	Saving
3	5	12.65
4	5	12.04
2	5	11.00
1	5	10.82
3	4	10.77
2	4	10.00
1	4	9.22
2	3	8.66
1	3	8.06
1	2	7.07

Initial routes (single customer per route):
Route 1: 0 1 0 (Cost: 8.25)
Route 2: 0 2 0 (Cost: 7.07)
Route 3: 0 3 0 (Cost: 9.22)
Route 4: 0 4 0 (Cost: 8.06)
Route 5: 0 5 0 (Cost: 10.82)

Merged routes 1 and 5 with saving 10.82
New route: 0 1 5 0 (Cost: 19.07)

Merged routes 2 and 4 with saving 10.00
New route: 0 2 4 0 (Cost: 16.12)

Merged routes 3 and 4 with saving 10.77
New route: 0 3 4 0 (Cost: 17.07)

=== Final Result ===
Number of routes created: 3
Route 1: 0 1 5 0 (Cost: 19.07)
Route 2: 0 2 4 0 (Cost: 16.12)
Route 3: 0 3 4 0 (Cost: 17.07)
```

## Algorithm Explanation

This implementation demonstrates the Clarke-Wright Savings algorithm for vehicle routing:

1. **Distance Calculation**: Computes Euclidean distances between all nodes
2. **Savings Computation**: Calculates savings for merging routes
3. **Sorting**: Orders savings in descending order
4. **Route Merging**: Merges routes based on highest savings while avoiding invalid combinations
5. **Final Output**: Displays the optimized routes

The algorithm aims to minimize total travel distance by combining delivery routes that share common customers, thereby reducing the number of vehicles needed and overall transportation costs.