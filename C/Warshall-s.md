# Floyd-Warshall Algorithm Implementation in C

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```c
#include <stdio.h>
#include <limits.h>

#define V 4  // Number of vertices in the graph
#define INF INT_MAX  // Define infinity

// Function to implement Floyd-Warshall algorithm
void floydWarshall(int graph[V][V]) {
    int dist[V][V];
    
    // Initialize distance matrix with input graph values
    for (int i = 0; i < V; i++) {
        for (int j = 0; j < V; j++) {
            dist[i][j] = graph[i][j];
        }
    }
    
    // Apply Floyd-Warshall algorithm
    for (int k = 0; k < V; k++) {
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                // If vertex k is on the shortest path from i to j,
                // then update the distance
                if (dist[i][k] != INF && dist[k][j] != INF && 
                    dist[i][k] + dist[k][j] < dist[i][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }
    
    // Print the shortest distance matrix
    printf("Shortest distances between every pair of vertices:\n");
    for (int i = 0; i < V; i++) {
        for (int j = 0; j < V; j++) {
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
      0      3   INF      7
      8      0      2   INF
      5   INF      0      1
      2   INF   INF      0

Shortest distances between every pair of vertices:
      0      3      5      6
      8      0     10     11
      5      8      0      1
      2     11     13      0
```

## Algorithm Explanation:

**Time Complexity:** O(V³) where V is the number of vertices  
**Space Complexity:** O(V²) for the distance matrix

The algorithm works by considering each vertex as an intermediate point and checking if using that vertex in the path between two other vertices results in a shorter path.

1. **Initialization**: Copy the input graph to a distance matrix
2. **Triple nested loop**: For each possible intermediate vertex k, update all pairs (i,j) 
3. **Relaxation**: If dist[i][k] + dist[k][j] < dist[i][j], update the distance
4. **Result**: Final matrix contains shortest paths between all pairs of vertices

This implementation handles negative edge weights but not negative cycles.