# Ford-Fulkerson Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_VERTICES 100

// Function to find the maximum flow using Ford-Fulkerson algorithm
int fordFulkerson(int graph[MAX_VERTICES][MAX_VERTICES], int source, int sink, int vertices) {
    int u, v;
    
    // Create a residual graph and fill it with the original capacities
    int residualGraph[MAX_VERTICES][MAX_VERTICES];
    
    // Initialize residual graph with original capacities
    for (u = 0; u < vertices; u++) {
        for (v = 0; v < vertices; v++) {
            residualGraph[u][v] = graph[u][v];
        }
    }
    
    // Store the path from source to sink
    int parent[MAX_VERTICES];
    
    int maxFlow = 0;
    
    // Keep finding augmenting paths until no path exists
    while (true) {
        // Use BFS to find a path from source to sink
        bool visited[MAX_VERTICES] = {false};
        int queue[MAX_VERTICES];
        int front = 0, rear = 0;
        
        queue[rear++] = source;
        visited[source] = true;
        parent[source] = -1;
        
        // BFS to find if there's a path from source to sink
        while (front < rear) {
            u = queue[front++];
            
            for (v = 0; v < vertices; v++) {
                if (!visited[v] && residualGraph[u][v] > 0) {
                    queue[rear++] = v;
                    visited[v] = true;
                    parent[v] = u;
                    
                    if (v == sink) {
                        break;
                    }
                }
            }
        }
        
        // If we couldn't reach sink, no more augmenting paths exist
        if (!visited[sink]) {
            break;
        }
        
        // Find the minimum capacity along the path
        int pathFlow = INT_MAX;
        v = sink;
        while (v != source) {
            u = parent[v];
            if (residualGraph[u][v] < pathFlow) {
                pathFlow = residualGraph[u][v];
            }
            v = u;
        }
        
        // Update residual capacities of the edges and reverse edges
        v = sink;
        while (v != source) {
            u = parent[v];
            residualGraph[u][v] -= pathFlow;
            residualGraph[v][u] += pathFlow;
            v = u;
        }
        
        // Add path flow to overall flow
        maxFlow += pathFlow;
    }
    
    return maxFlow;
}

// Function to print the adjacency matrix
void printGraph(int graph[MAX_VERTICES][MAX_VERTICES], int vertices) {
    printf("Adjacency Matrix:\n");
    for (int i = 0; i < vertices; i++) {
        for (int j = 0; j < vertices; j++) {
            printf("%d ", graph[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

int main() {
    // Example graph represented as adjacency matrix
    // Graph with 6 vertices (0 to 5)
    int vertices = 6;
    
    int graph[MAX_VERTICES][MAX_VERTICES] = {
        {0, 16, 13, 0, 0, 0},
        {0, 0, 10, 12, 0, 0},
        {0, 4, 0, 0, 14, 0},
        {0, 0, 9, 0, 0, 20},
        {0, 0, 0, 7, 0, 4},
        {0, 0, 0, 0, 0, 0}
    };
    
    printf("=== Ford-Fulkerson Algorithm Example ===\n\n");
    
    // Print the original graph
    printGraph(graph, vertices);
    
    int source = 0;  // Source vertex
    int sink = 5;    // Sink vertex
    
    printf("Source vertex: %d\n", source);
    printf("Sink vertex: %d\n", sink);
    printf("\n");
    
    // Find maximum flow
    int maxFlow = fordFulkerson(graph, source, sink, vertices);
    
    printf("Maximum flow from vertex %d to vertex %d is: %d\n", source, sink, maxFlow);
    
    return 0;
}
```

## Explanation of the Algorithm

### How Ford-Fulkerson Works:

1. **Initialize**: Create a residual graph with the same capacities as the original graph
2. **Find Path**: Use BFS/DFS to find an augmenting path from source to sink
3. **Calculate Flow**: Find minimum capacity along the path
4. **Update**: Reduce forward edges and increase backward edges in residual graph
5. **Repeat**: Continue until no more augmenting paths exist

### Key Components:

- **Residual Graph**: Tracks available capacity for flow
- **BFS**: Finds augmenting paths
- **Path Tracking**: Uses parent array to reconstruct paths
- **Capacity Updates**: Modifies residual capacities after each augmentation

### Example Output:
```
=== Ford-Fulkerson Algorithm Example ===

Adjacency Matrix:
0 16 13 0 0 0 
0 0 10 12 0 0 
0 4 0 0 14 0 
0 0 9 0 0 20 
0 0 0 7 0 4 
0 0 0 0 0 0 

Source vertex: 0
Sink vertex: 5

Maximum flow from vertex 0 to vertex 5 is: 23
```

This implementation finds the maximum flow from vertex 0 to vertex 5 in the given network, which is 23 units.

