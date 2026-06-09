# Bellman-Ford Algorithm in C

Here's a complete implementation of the Bellman-Ford algorithm to find shortest paths from a source vertex to all other vertices in a weighted graph:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

// Structure to represent an edge
struct Edge {
    int src, dest, weight;
};

// Structure to represent a graph
struct Graph {
    int V; // Number of vertices
    int E; // Number of edges
    struct Edge* edge;
};

// Creates a graph with V vertices and E edges
struct Graph* createGraph(int V, int E) {
    struct Graph* graph = (struct Graph*) malloc(sizeof(struct Graph));
    graph->V = V;
    graph->E = E;
    graph->edge = (struct Edge*) malloc(E * sizeof(struct Edge));
    return graph;
}

// Function to print the solution
void printArr(int dist[], int n) {
    printf("Vertex   Distance from Source\n");
    for (int i = 0; i < n; i++) {
        printf("%d \t\t %d\n", i, dist[i]);
    }
}

// The main function that finds shortest distances from src to all other vertices
void BellmanFord(struct Graph* graph, int src) {
    int V = graph->V;
    int E = graph->E;
    int dist[V];

    // Step 1: Initialize distances from src to all other vertices as INFINITE
    for (int i = 0; i < V; i++) {
        dist[i] = INT_MAX;
    }
    dist[src] = 0;

    // Step 2: Relax all edges |V| - 1 times
    for (int i = 1; i <= V - 1; i++) {
        for (int j = 0; j < E; j++) {
            int u = graph->edge[j].src;
            int v = graph->edge[j].dest;
            int weight = graph->edge[j].weight;
            
            if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                dist[v] = dist[u] + weight;
            }
        }
    }

    // Step 3: Check for negative-weight cycles
    for (int i = 0; i < E; i++) {
        int u = graph->edge[i].src;
        int v = graph->edge[i].dest;
        int weight = graph->edge[i].weight;
        
        if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
            printf("Graph contains negative weight cycle\n");
            return;
        }
    }

    // Print the constructed distance array
    printArr(dist, V);
}

// Driver program to test above functions
int main() {
    int V = 5; // Number of vertices in graph
    int E = 8; // Number of edges in graph
    
    struct Graph* graph = createGraph(V, E);

    // Add edges to the graph
    // Edge from vertex 0 to 1 with weight -1
    graph->edge[0].src = 0;
    graph->edge[0].dest = 1;
    graph->edge[0].weight = -1;

    // Edge from vertex 0 to 2 with weight 4
    graph->edge[1].src = 0;
    graph->edge[1].dest = 2;
    graph->edge[1].weight = 4;

    // Edge from vertex 1 to 2 with weight 3
    graph->edge[2].src = 1;
    graph->edge[2].dest = 2;
    graph->edge[2].weight = 3;

    // Edge from vertex 1 to 3 with weight 2
    graph->edge[3].src = 1;
    graph->edge[3].dest = 3;
    graph->edge[3].weight = 2;

    // Edge from vertex 1 to 4 with weight 2
    graph->edge[4].src = 1;
    graph->edge[4].dest = 4;
    graph->edge[4].weight = 2;

    // Edge from vertex 3 to 2 with weight 5
    graph->edge[5].src = 3;
    graph->edge[5].dest = 2;
    graph->edge[5].weight = 5;

    // Edge from vertex 3 to 1 with weight 1
    graph->edge[6].src = 3;
    graph->edge[6].dest = 1;
    graph->edge[6].weight = 1;

    // Edge from vertex 4 to 3 with weight -3
    graph->edge[7].src = 4;
    graph->edge[7].dest = 3;
    graph->edge[7].weight = -3;

    BellmanFord(graph, 0);

    return 0;
}
```

## Output:
```
Vertex   Distance from Source
0 		0
1 		-1
2 		2
3 		-2
4 		1
```

## How it works:

1. **Initialization**: Set distance of source vertex to 0 and all other vertices to infinity
2. **Relaxation**: For V-1 iterations, relax all edges (update distances if shorter path found)
3. **Negative Cycle Detection**: Check if any edge can still be relaxed - if yes, negative cycle exists
4. **Output**: Print shortest distances from source to all vertices

## Time Complexity: O(VE)
## Space Complexity: O(V)

The algorithm can handle negative edge weights and detect negative weight cycles, making it more versatile than Dijkstra's algorithm for certain graph types.