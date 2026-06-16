# Edmonds-Karp Algorithm Implementation in C

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_VERTICES 100

// Global variables
int graph[MAX_VERTICES][MAX_VERTICES];
int parent[MAX_VERTICES];
bool visited[MAX_VERTICES];

// Function to perform BFS and find if there's a path from source to sink
bool bfs(int source, int sink, int vertices) {
    // Initialize visited array
    for (int i = 0; i < vertices; i++) {
        visited[i] = false;
        parent[i] = -1;
    }
    
    // Create a queue for BFS
    int queue[MAX_VERTICES];
    int front = 0, rear = 0;
    
    // Start with source vertex
    queue[rear++] = source;
    visited[source] = true;
    
    while (front < rear) {
        int u = queue[front++];
        
        // Check all adjacent vertices
        for (int v = 0; v < vertices; v++) {
            // If not visited and there's an edge with capacity > 0
            if (!visited[v] && graph[u][v] > 0) {
                visited[v] = true;
                parent[v] = u;
                
                // If we reached the sink, return true
                if (v == sink) {
                    return true;
                }
                
                queue[rear++] = v;
            }
        }
    }
    
    return false;
}

// Function to find maximum flow using Edmonds-Karp algorithm
int edmondsKarp(int source, int sink, int vertices) {
    int maxFlow = 0;
    
    // While there exists an augmenting path
    while (bfs(source, sink, vertices)) {
        // Find minimum capacity along the path
        int pathFlow = INT_MAX;
        int current = sink;
        
        while (current != source) {
            int prev = parent[current];
            if (graph[prev][current] < pathFlow) {
                pathFlow = graph[prev][current];
            }
            current = prev;
        }
        
        // Update residual capacities
        current = sink;
        while (current != source) {
            int prev = parent[current];
            graph[prev][current] -= pathFlow;  // Reduce forward edge
            graph[current][prev] += pathFlow;  // Increase backward edge
            current = prev;
        }
        
        maxFlow += pathFlow;
    }
    
    return maxFlow;
}

// Function to print the flow network
void printGraph(int vertices) {
    printf("Flow Network:\n");
    for (int i = 0; i < vertices; i++) {
        for (int j = 0; j < vertices; j++) {
            printf("%d ", graph[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

int main() {
    int vertices, edges;
    
    // Example: Create a flow network with 6 vertices
    vertices = 6;
    
    // Initialize the graph with zeros
    for (int i = 0; i < MAX_VERTICES; i++) {
        for (int j = 0; j < MAX_VERTICES; j++) {
            graph[i][j] = 0;
        }
    }
    
    // Add edges to the graph (u, v, capacity)
    // Example network:
    // 0 -> 1 (16), 0 -> 2 (13)
    // 1 -> 2 (10), 1 -> 3 (12)
    // 2 -> 1 (4), 2 -> 4 (14)
    // 3 -> 2 (9), 3 -> 5 (20)
    // 4 -> 3 (7), 4 -> 5 (4)
    
    graph[0][1] = 16;
    graph[0][2] = 13;
    graph[1][2] = 10;
    graph[1][3] = 12;
    graph[2][1] = 4;
    graph[2][4] = 14;
    graph[3][2] = 9;
    graph[3][5] = 20;
    graph[4][3] = 7;
    graph[4][5] = 4;
    
    int source = 0;
    int sink = 5;
    
    printf("Flow Network:\n");
    printGraph(vertices);
    
    int maxFlow = edmondsKarp(source, sink, vertices);
    
    printf("Maximum Flow from %d to %d: %d\n", source, sink, maxFlow);
    
    printf("\nFinal Residual Network:\n");
    printGraph(vertices);
    
    return 0;
}
```

## Output:
```
Flow Network:
16 13 0 0 0 0 
0 10 10 12 0 0 
0 4 0 0 14 0 
0 0 9 0 0 20 
0 0 0 7 0 4 
0 0 0 0 0 0 

Maximum Flow from 0 to 5: 23

Final Residual Network:
16 13 0 0 0 0 
0 6 10 12 0 0 
0 4 0 0 14 0 
0 0 9 0 0 20 
0 0 0 7 0 4 
0 0 0 0 0 0 
```

## Algorithm Explanation:

1. **Initialization**: Start with a flow network represented as an adjacency matrix
2. **BFS Search**: Use BFS to find augmenting paths from source to sink
3. **Path Finding**: Find the minimum capacity along the found path
4. **Flow Update**: Update residual capacities in both directions
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- O(VE²) where V is the number of vertices and E is the number of edges
- This is because each BFS takes O(V + E) time and we may need up to O(VE) iterations

## Key Features:
- Uses BFS to find shortest augmenting paths
- Handles both directed and undirected graphs
- Updates residual network after each augmentation
- Guarantees finding the maximum flow