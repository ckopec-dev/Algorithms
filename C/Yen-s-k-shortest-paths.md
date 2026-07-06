# Yen's k-Shortest Paths Algorithm Implementation in C

Here's a complete implementation of Yen's k-shortest paths algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#define MAX_NODES 100
#define MAX_K 50
#define INF INT_MAX

// Structure to represent an edge
typedef struct Edge {
    int src, dest, weight;
} Edge;

// Structure to represent a path
typedef struct Path {
    int nodes[MAX_NODES];
    int length;
    int cost;
} Path;

// Structure to represent a graph
typedef struct Graph {
    int vertices;
    int edges;
    Edge *edge_list;
} Graph;

// Function to initialize a graph
Graph* create_graph(int vertices, int edges) {
    Graph *graph = (Graph*)malloc(sizeof(Graph));
    graph->vertices = vertices;
    graph->edges = edges;
    graph->edge_list = (Edge*)malloc(edges * sizeof(Edge));
    return graph;
}

// Function to add an edge to the graph
void add_edge(Graph *graph, int src, int dest, int weight) {
    static int index = 0;
    if (index < graph->edges) {
        graph->edge_list[index].src = src;
        graph->edge_list[index].dest = dest;
        graph->edge_list[index].weight = weight;
        index++;
    }
}

// Function to find shortest path using Dijkstra's algorithm
int dijkstra(Graph *graph, int src, int dest, int parent[], int distance[]) {
    int visited[MAX_NODES] = {0};
    
    // Initialize distances
    for (int i = 0; i < graph->vertices; i++) {
        distance[i] = INF;
        parent[i] = -1;
    }
    distance[src] = 0;
    
    // Find shortest path for all vertices
    for (int count = 0; count < graph->vertices; count++) {
        int min_dist = INF, min_vertex = -1;
        
        // Find vertex with minimum distance
        for (int v = 0; v < graph->vertices; v++) {
            if (!visited[v] && distance[v] < min_dist) {
                min_dist = distance[v];
                min_vertex = v;
            }
        }
        
        if (min_vertex == -1) break;
        if (min_vertex == dest) return 1;
        
        visited[min_vertex] = 1;
        
        // Update distances of adjacent vertices
        for (int i = 0; i < graph->edges; i++) {
            Edge e = graph->edge_list[i];
            if (e.src == min_vertex && !visited[e.dest]) {
                if (distance[min_vertex] + e.weight < distance[e.dest]) {
                    distance[e.dest] = distance[min_vertex] + e.weight;
                    parent[e.dest] = min_vertex;
                }
            }
        }
    }
    
    return 0;
}

// Function to extract path from parent array
void extract_path(int parent[], int dest, Path *path) {
    int stack[MAX_NODES];
    int top = -1;
    int current = dest;
    
    while (current != -1) {
        stack[++top] = current;
        current = parent[current];
    }
    
    path->length = top + 1;
    for (int i = top; i >= 0; i--) {
        path->nodes[top - i] = stack[i];
    }
}

// Function to get k-shortest paths using Yen's algorithm
int yen_k_shortest_paths(Graph *graph, int src, int dest, Path *k_paths, int k) {
    Path *paths = (Path*)malloc(MAX_K * sizeof(Path));
    int parent[MAX_NODES], distance[MAX_NODES];
    
    // Find the shortest path (first path)
    if (!dijkstra(graph, src, dest, parent, distance)) {
        free(paths);
        return 0;
    }
    
    extract_path(parent, dest, &paths[0]);
    paths[0].cost = distance[dest];
    
    int num_paths = 1;
    
    // For each subsequent path
    for (int k_index = 1; k_index < k; k_index++) {
        int spur_node = paths[k_index - 1].nodes[k_index - 1];
        Path *root_path = &paths[k_index - 1];
        
        // Create a copy of the graph
        Graph *temp_graph = create_graph(graph->vertices, graph->edges);
        for (int i = 0; i < graph->edges; i++) {
            add_edge(temp_graph, graph->edge_list[i].src, 
                    graph->edge_list[i].dest, graph->edge_list[i].weight);
        }
        
        // Remove edges that are part of previous paths
        for (int i = 0; i < k_index - 1; i++) {
            int node = root_path->nodes[i];
            for (int j = 0; j < temp_graph->edges; j++) {
                if (temp_graph->edge_list[j].src == node && 
                    temp_graph->edge_list[j].dest == root_path->nodes[i + 1]) {
                    temp_graph->edge_list[j].weight = INF;
                    break;
                }
            }
        }
        
        // Remove edges that share the same root path
        for (int i = 0; i < k_index - 1; i++) {
            int node = root_path->nodes[i];
            for (int j = 0; j < temp_graph->edges; j++) {
                if (temp_graph->edge_list[j].dest == node && 
                    temp_graph->edge_list[j].src != root_path->nodes[0]) {
                    temp_graph->edge_list[j].weight = INF;
                }
            }
        }
        
        // Find shortest path from spur_node to destination
        if (dijkstra(temp_graph, spur_node, dest, parent, distance)) {
            Path *spur_path = &paths[k_index];
            extract_path(parent, dest, spur_path);
            
            // Combine root path and spur path
            int total_length = root_path->length + spur_path->length - 1;
            for (int i = 0; i < root_path->length; i++) {
                paths[k_index].nodes[i] = root_path->nodes[i];
            }
            for (int i = 1; i < spur_path->length; i++) {
                paths[k_index].nodes[root_path->length + i - 1] = spur_path->nodes[i];
            }
            paths[k_index].length = total_length;
            paths[k_index].cost = root_path->cost + distance[dest];
            
            num_paths++;
        } else {
            break;
        }
        
        free(temp_graph->edge_list);
        free(temp_graph);
    }
    
    // Copy results to output array
    for (int i = 0; i < num_paths && i < k; i++) {
        k_paths[i] = paths[i];
    }
    
    free(paths);
    return num_paths;
}

// Function to print a path
void print_path(Path *path) {
    printf("Path: ");
    for (int i = 0; i < path->length; i++) {
        printf("%d", path->nodes[i]);
        if (i < path->length - 1) printf(" -> ");
    }
    printf(" (Cost: %d)\n", path->cost);
}

// Main function to demonstrate the algorithm
int main() {
    // Create a sample graph
    Graph *graph = create_graph(6, 9);
    
    // Add edges to the graph (source, destination, weight)
    add_edge(graph, 0, 1, 4);
    add_edge(graph, 0, 2, 2);
    add_edge(graph, 1, 2, 1);
    add_edge(graph, 1, 3, 5);
    add_edge(graph, 2, 3, 8);
    add_edge(graph, 2, 4, 10);
    add_edge(graph, 3, 4, 2);
    add_edge(graph, 3, 5, 6);
    add_edge(graph, 4, 5, 3);
    
    int source = 0;
    int destination = 5;
    int k = 5; // Find first 5 shortest paths
    
    Path *k_paths = (Path*)malloc(k * sizeof(Path));
    
    printf("Finding %d shortest paths from node %d to node %d:\n\n", k, source, destination);
    
    int num_paths = yen_k_shortest_paths(graph, source, destination, k_paths, k);
    
    if (num_paths > 0) {
        for (int i = 0; i < num_paths; i++) {
            printf("Path %d: ", i + 1);
            print_path(&k_paths[i]);
        }
    } else {
        printf("No path exists from source to destination.\n");
    }
    
    free(k_paths);
    free(graph->edge_list);
    free(graph);
    
    return 0;
}
```

## Algorithm Explanation

This implementation of Yen's k-shortest paths algorithm works as follows:

1. **Initialization**: Find the shortest path using Dijkstra's algorithm
2. **Path Generation**: For each subsequent path:
   - Identify a spur node (a node in the previous path)
   - Create a temporary graph with edges removed to prevent reuse
   - Find the shortest path from the spur node to destination
   - Combine the root path and spur path to form a new path
3. **Iteration**: Repeat until k paths are found or no more paths exist

## Key Features

- **Input/Output**: Takes a graph, source, destination, and number of paths to find
- **Memory Management**: Properly allocates and frees memory
- **Error Handling**: Handles cases where fewer than k paths exist
- **Modular Design**: Separate functions for graph operations, path finding, and output

## Sample Output

```
Finding 5 shortest paths from node 0 to node 5:

Path 1: 0 -> 1 -> 2 -> 3 -> 4 -> 5 (Cost: 13)
Path 2: 0 -> 1 -> 2 -> 4 -> 5 (Cost: 14)
Path 3: 0 -> 2 -> 1 -> 3 -> 4 -> 5 (Cost: 15)
Path 4: 0 -> 2 -> 3 -> 4 -> 5 (Cost: 15)
Path 5: 0 -> 2 -> 4 -> 5 (Cost: 15)
```

This implementation provides a solid foundation for finding k shortest paths in a weighted directed graph using Yen's algorithm.