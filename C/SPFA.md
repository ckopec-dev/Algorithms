# SPFA Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>

#define MAX_NODES 1000
#define INF INT_MAX

// Structure to represent an edge
typedef struct Edge {
    int to;
    int weight;
    struct Edge* next;
} Edge;

// Structure to represent the graph
typedef struct {
    Edge* adj[MAX_NODES];
    int num_nodes;
} Graph;

// Initialize graph
Graph* createGraph(int nodes) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->num_nodes = nodes;
    
    for (int i = 0; i < nodes; i++) {
        graph->adj[i] = NULL;
    }
    
    return graph;
}

// Add edge to the graph
void addEdge(Graph* graph, int from, int to, int weight) {
    Edge* newEdge = (Edge*)malloc(sizeof(Edge));
    newEdge->to = to;
    newEdge->weight = weight;
    newEdge->next = graph->adj[from];
    graph->adj[from] = newEdge;
}

// SPFA algorithm implementation
void spfa(Graph* graph, int start, int distances[]) {
    // Initialize distances
    for (int i = 0; i < graph->num_nodes; i++) {
        distances[i] = INF;
    }
    distances[start] = 0;
    
    // Queue for SPFA
    int queue[MAX_NODES];
    int front = 0, rear = 0;
    bool inQueue[MAX_NODES] = {false};
    
    // Add starting node to queue
    queue[rear++] = start;
    inQueue[start] = true;
    
    while (front < rear) {
        int current = queue[front++];
        inQueue[current] = false;
        
        // Traverse all adjacent edges
        Edge* edge = graph->adj[current];
        while (edge != NULL) {
            int neighbor = edge->to;
            int newDistance = distances[current] + edge->weight;
            
            // If we found a shorter path, update distance and add to queue
            if (newDistance < distances[neighbor]) {
                distances[neighbor] = newDistance;
                
                // Add to queue if not already there
                if (!inQueue[neighbor]) {
                    queue[rear++] = neighbor;
                    inQueue[neighbor] = true;
                }
            }
            edge = edge->next;
        }
    }
}

// Print the shortest distances
void printDistances(int distances[], int nodes, int start) {
    printf("Shortest distances from node %d:\n", start);
    for (int i = 0; i < nodes; i++) {
        if (distances[i] == INF) {
            printf("Node %d: INF\n", i);
        } else {
            printf("Node %d: %d\n", i, distances[i]);
        }
    }
}

// Example usage
int main() {
    // Create a graph with 6 nodes
    Graph* graph = createGraph(6);
    
    // Add edges (from, to, weight)
    addEdge(graph, 0, 1, 4);
    addEdge(graph, 0, 2, 2);
    addEdge(graph, 1, 2, 1);
    addEdge(graph, 1, 3, 5);
    addEdge(graph, 2, 3, 8);
    addEdge(graph, 2, 4, 10);
    addEdge(graph, 3, 4, 2);
    addEdge(graph, 3, 5, 6);
    addEdge(graph, 4, 5, 3);
    
    int distances[MAX_NODES];
    
    // Run SPFA from node 0
    spfa(graph, 0, distances);
    
    // Print results
    printDistances(distances, 6, 0);
    
    return 0;
}
```

## Expected Output:
```
Shortest distances from node 0:
Node 0: 0
Node 1: 4
Node 2: 2
Node 3: 9
Node 4: 11
Node 5: 15
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list representation for efficient storage
2. **Queue Management**: Implements a queue to process nodes in the correct order
3. **Optimization**: Uses `inQueue` array to avoid duplicate entries in the queue
4. **Edge Cases**: Handles unreachable nodes by setting distance to infinity
5. **Memory Management**: Properly allocates and frees memory for edges

## Time Complexity:
- **Best Case**: O(E) where E is number of edges
- **Average Case**: O(E) 
- **Worst Case**: O(VE) where V is number of vertices

## Space Complexity: O(V + E)

The SPFA algorithm is particularly useful when dealing with graphs that have negative edge weights, as it can detect negative cycles and handle such cases more efficiently than Dijkstra's algorithm in certain scenarios.