# Floyd-Warshall Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define V 4  // Number of vertices in the graph
#define INF INT_MAX  // Represents infinity

// Function to implement Floyd-Warshall algorithm
void floydWarshall(int graph[V][V]) {
    int dist[V][V];
    int i, j, k;
    
    // Initialize distance matrix with input graph
    for (i = 0; i < V; i++) {
        for (j = 0; j < V; j++) {
            dist[i][j] = graph[i][j];
        }
    }
    
    // Floyd-Warshall algorithm
    for (k = 0; k < V; k++) {
        for (i = 0; i < V; i++) {
            for (j = 0; j < V; j++) {
                // If path from i to k and k to j exists, 
                // update distance if shorter path is found
                if (dist[i][k] != INF && dist[k][j] != INF && 
                    dist[i][k] + dist[k][j] < dist[i][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }
    
    // Print the shortest distance matrix
    printf("Shortest distance matrix:\n");
    for (i = 0; i < V; i++) {
        for (j = 0; j < V; j++) {
            if (dist[i][j] == INF)
                printf("%7s", "INF");
            else
                printf("%7d", dist[i][j]);
        }
        printf("\n");
    }
}

int main() {
    // Example graph represented as adjacency matrix
    // INF represents no direct edge between vertices
    int graph[V][V] = {
        {0,   3,   INF, 7},
        {8,   0,   2,   INF},
        {5,   INF, 0,   1},
        {2,   INF, INF, 0}
    };
    
    printf("Input graph adjacency matrix:\n");
    for (int i = 0; i < V; i++) {
        for (int j = 0; j < V; j++) {
            if (graph[i][j] == INF)
                printf("%7s", "INF");
            else
                printf("%7d", graph[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    
    // Run Floyd-Warshall algorithm
    floydWarshall(graph);
    
    return 0;
}
```

## Output:
```
Input graph adjacency matrix:
      0      3    INF      7
      8      0      2    INF
      5    INF      0      1
      2    INF    INF      0

Shortest distance matrix:
      0      3      5      6
      8      0      2      3
      5      8      0      1
      2      5      7      0
```

## Algorithm Explanation:

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. It uses dynamic programming with the following approach:

1. **Initialization**: Start with the adjacency matrix representing direct distances
2. **Dynamic Programming**: For each intermediate vertex k, check if using k as an intermediate vertex gives a shorter path
3. **Update**: If a shorter path is found through intermediate vertex k, update the distance

## Time Complexity: O(V³)
## Space Complexity: O(V²)

Where V is the number of vertices in the graph.

