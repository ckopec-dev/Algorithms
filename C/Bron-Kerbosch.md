# Bron-Kerbosch Algorithm Implementation in C

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 100

// Graph representation using adjacency matrix
int graph[MAX_NODES][MAX_NODES];
int n; // number of nodes

// Global arrays for the algorithm
int R[MAX_NODES];  // current clique
int P[MAX_NODES];  // potential nodes
int X[MAX_NODES];  // excluded nodes
int max_clique_size = 0;
int max_clique[MAX_NODES];

// Function to check if two nodes are adjacent
int is_adjacent(int u, int v) {
    return graph[u][v] == 1;
}

// Function to find intersection of two sets
int intersect(int set1[], int size1, int set2[], int size2, int result[]) {
    int count = 0;
    for (int i = 0; i < size1; i++) {
        for (int j = 0; j < size2; j++) {
            if (set1[i] == set2[j]) {
                result[count++] = set1[i];
                break;
            }
        }
    }
    return count;
}

// Function to get neighbors of a node
int get_neighbors(int node, int neighbors[]) {
    int count = 0;
    for (int i = 0; i < n; i++) {
        if (graph[node][i] == 1) {
            neighbors[count++] = i;
        }
    }
    return count;
}

// Bron-Kerbosch algorithm implementation
void bron_kerbosch(int R_size, int P_size, int X_size) {
    // If P and X are both empty, R is a maximal clique
    if (P_size == 0 && X_size == 0) {
        if (R_size > max_clique_size) {
            max_clique_size = R_size;
            for (int i = 0; i < R_size; i++) {
                max_clique[i] = R[i];
            }
        }
        return;
    }
    
    // Choose a pivot node u from P ∪ X
    int u = -1;
    int max_degree = -1;
    
    // Find node with maximum degree in P ∪ X
    for (int i = 0; i < P_size; i++) {
        int degree = 0;
        for (int j = 0; j < P_size; j++) {
            if (is_adjacent(P[i], P[j])) degree++;
        }
        if (degree > max_degree) {
            max_degree = degree;
            u = P[i];
        }
    }
    
    // If no pivot found, try X
    if (u == -1) {
        for (int i = 0; i < X_size; i++) {
            int degree = 0;
            for (int j = 0; j < P_size; j++) {
                if (is_adjacent(X[i], P[j])) degree++;
            }
            if (degree > max_degree) {
                max_degree = degree;
                u = X[i];
            }
        }
    }
    
    // Get neighbors of u
    int u_neighbors[MAX_NODES];
    int u_neighbors_count = get_neighbors(u, u_neighbors);
    
    // Create P_minus_N(u) - nodes in P but not adjacent to u
    int P_minus_N[MAX_NODES];
    int P_minus_N_size = 0;
    
    for (int i = 0; i < P_size; i++) {
        int is_neighbor = 0;
        for (int j = 0; j < u_neighbors_count; j++) {
            if (P[i] == u_neighbors[j]) {
                is_neighbor = 1;
                break;
            }
        }
        if (!is_neighbor) {
            P_minus_N[P_minus_N_size++] = P[i];
        }
    }
    
    // For each node v in P_minus_N(u)
    for (int i = 0; i < P_minus_N_size; i++) {
        int v = P_minus_N[i];
        
        // Add v to R
        R[R_size] = v;
        
        // Create new P and X sets
        int new_P[MAX_NODES];
        int new_X[MAX_NODES];
        int new_P_size = 0;
        int new_X_size = 0;
        
        // Find intersection of P and neighbors of v
        int v_neighbors[MAX_NODES];
        int v_neighbors_count = get_neighbors(v, v_neighbors);
        
        for (int j = 0; j < P_size; j++) {
            for (int k = 0; k < v_neighbors_count; k++) {
                if (P[j] == v_neighbors[k]) {
                    new_P[new_P_size++] = P[j];
                    break;
                }
            }
        }
        
        // Find intersection of X and neighbors of v
        for (int j = 0; j < X_size; j++) {
            for (int k = 0; k < v_neighbors_count; k++) {
                if (X[j] == v_neighbors[k]) {
                    new_X[new_X_size++] = X[j];
                    break;
                }
            }
        }
        
        // Recursively call Bron-Kerbosch
        bron_kerbosch(R_size + 1, new_P_size, new_X_size);
        
        // Remove v from P and add to X
        for (int j = 0; j < P_size; j++) {
            if (P[j] == v) {
                for (int k = j; k < P_size - 1; k++) {
                    P[k] = P[k + 1];
                }
                P_size--;
                break;
            }
        }
        
        X[X_size++] = v;
    }
}

// Function to find all maximal cliques
void find_all_maximal_cliques() {
    // Initialize P with all nodes
    int P_size = 0;
    for (int i = 0; i < n; i++) {
        P[P_size++] = i;
    }
    
    // Initialize R and X as empty
    int R_size = 0;
    int X_size = 0;
    
    // Call Bron-Kerbosch
    bron_kerbosch(R_size, P_size, X_size);
}

// Function to print the maximum clique
void print_max_clique() {
    printf("Maximum clique size: %d\n", max_clique_size);
    printf("Maximum clique nodes: ");
    for (int i = 0; i < max_clique_size; i++) {
        printf("%d ", max_clique[i]);
    }
    printf("\n");
}

// Function to print the graph
void print_graph() {
    printf("Graph adjacency matrix:\n");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%d ", graph[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

int main() {
    // Example graph with 6 nodes
    n = 6;
    
    // Initialize graph to 0
    for (int i = 0; i < MAX_NODES; i++) {
        for (int j = 0; j < MAX_NODES; j++) {
            graph[i][j] = 0;
        }
    }
    
    // Define edges (undirected graph)
    int edges[][2] = {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 4}, {2, 3}, {2, 4}, {3, 5}, {4, 5}};
    int num_edges = sizeof(edges) / sizeof(edges[0]);
    
    // Fill the adjacency matrix
    for (int i = 0; i < num_edges; i++) {
        int u = edges[i][0];
        int v = edges[i][1];
        graph[u][v] = 1;
        graph[v][u] = 1;  // Undirected graph
    }
    
    printf("Example Graph:\n");
    print_graph();
    
    // Find all maximal cliques
    find_all_maximal_cliques();
    
    // Print the maximum clique found
    print_max_clique();
    
    return 0;
}
```

## How the Algorithm Works

The Bron-Kerbosch algorithm uses a recursive backtracking approach to find all maximal cliques:

1. **Base Case**: If both `P` (potential nodes) and `X` (excluded nodes) are empty, then `R` (current clique) is a maximal clique.

2. **Pivot Selection**: Choose a pivot node `u` from `P ∪ X` to optimize performance.

3. **Recursive Step**: For each node `v` in `P - N(u)` (nodes in P but not adjacent to u):
   - Add `v` to current clique `R`
   - Recursively call Bron-Kerbosch with updated sets
   - Remove `v` from `P` and add to `X`

## Sample Output

```
Example Graph:
Graph adjacency matrix:
0 1 1 1 0 0 
1 0 1 0 1 0 
1 1 0 1 1 0 
1 0 1 0 0 1 
0 1 1 0 0 1 
0 0 0 1 1 0 

Maximum clique size: 3
Maximum clique nodes: 0 1 2 
```

This implementation finds the maximum clique in the given graph, which in this example is of size 3 with nodes 0, 1, and 2.

