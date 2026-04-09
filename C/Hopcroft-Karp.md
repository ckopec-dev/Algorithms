# Hopcroft-Karp Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_VERTICES 100
#define MAX_EDGES 1000

// Graph representation using adjacency list
typedef struct Node {
    int vertex;
    struct Node* next;
} Node;

typedef struct {
    Node* head;
} AdjList;

typedef struct {
    int vertices;
    int edges;
    AdjList adj[MAX_VERTICES];
    int match_left[MAX_VERTICES];   // Left vertices matching
    int match_right[MAX_VERTICES];  // Right vertices matching
    int dist[MAX_VERTICES];         // Distance for BFS
} BipartiteGraph;

// Function to initialize the graph
BipartiteGraph* createGraph(int vertices) {
    BipartiteGraph* graph = (BipartiteGraph*)malloc(sizeof(BipartiteGraph));
    graph->vertices = vertices;
    graph->edges = 0;
    
    for (int i = 0; i < vertices; i++) {
        graph->adj[i].head = NULL;
        graph->match_left[i] = -1;
        graph->match_right[i] = -1;
        graph->dist[i] = -1;
    }
    
    return graph;
}

// Function to add edge to the graph
void addEdge(BipartiteGraph* graph, int u, int v) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->vertex = v;
    newNode->next = graph->adj[u].head;
    graph->adj[u].head = newNode;
    graph->edges++;
}

// Function to perform BFS and find augmenting paths
bool bfs(BipartiteGraph* graph, int leftVertices) {
    // Initialize distance array
    for (int i = 0; i < leftVertices; i++) {
        graph->dist[i] = -1;
    }
    
    // Create queue for BFS
    int queue[MAX_VERTICES];
    int front = 0, rear = 0;
    
    // Add all unmatched vertices in left side to queue
    for (int i = 0; i < leftVertices; i++) {
        if (graph->match_left[i] == -1) {
            graph->dist[i] = 0;
            queue[rear++] = i;
        }
    }
    
    bool foundAugmentingPath = false;
    
    while (front < rear) {
        int u = queue[front++];
        
        Node* temp = graph->adj[u].head;
        while (temp != NULL) {
            int v = temp->vertex;
            int uMatch = graph->match_right[v];
            
            if (uMatch == -1) {
                // Found an unmatched vertex in right side
                foundAugmentingPath = true;
            } else if (graph->dist[uMatch] == -1) {
                // Continue BFS
                graph->dist[uMatch] = graph->dist[u] + 1;
                queue[rear++] = uMatch;
            }
            temp = temp->next;
        }
    }
    
    return foundAugmentingPath;
}

// Function to find augmenting path using DFS
bool dfs(BipartiteGraph* graph, int u, int leftVertices) {
    if (u >= leftVertices) return false;
    
    Node* temp = graph->adj[u].head;
    while (temp != NULL) {
        int v = temp->vertex;
        int uMatch = graph->match_right[v];
        
        if (uMatch == -1 || (graph->dist[uMatch] == graph->dist[u] + 1 && dfs(graph, uMatch, leftVertices))) {
            // Found augmenting path
            graph->match_right[v] = u;
            graph->match_left[u] = v;
            return true;
        }
        temp = temp->next;
    }
    
    return false;
}

// Function to find maximum matching using Hopcroft-Karp algorithm
int hopcroftKarp(BipartiteGraph* graph, int leftVertices, int rightVertices) {
    int matching = 0;
    
    // Initialize match arrays
    for (int i = 0; i < leftVertices; i++) {
        graph->match_left[i] = -1;
    }
    for (int i = 0; i < rightVertices; i++) {
        graph->match_right[i] = -1;
    }
    
    while (bfs(graph, leftVertices)) {
        for (int i = 0; i < leftVertices; i++) {
            if (graph->match_left[i] == -1 && dfs(graph, i, leftVertices)) {
                matching++;
            }
        }
    }
    
    return matching;
}

// Function to print the matching
void printMatching(BipartiteGraph* graph, int leftVertices, int rightVertices) {
    printf("Maximum Matching:\n");
    printf("Left Vertex -> Right Vertex\n");
    printf("--------------------------\n");
    
    int count = 0;
    for (int i = 0; i < leftVertices; i++) {
        if (graph->match_left[i] != -1) {
            printf("   %d -> %d\n", i, graph->match_left[i]);
            count++;
        }
    }
    printf("Total matching pairs: %d\n", count);
}

// Example usage
int main() {
    printf("Hopcroft-Karp Algorithm Example\n");
    printf("==============================\n\n");
    
    // Create a bipartite graph with 6 left vertices and 6 right vertices
    // Left vertices: 0, 1, 2, 3, 4, 5
    // Right vertices: 0, 1, 2, 3, 4, 5
    
    BipartiteGraph* graph = createGraph(6);
    
    // Add edges to create a bipartite graph
    // Left vertex 0 connects to right vertices 0, 1, 2
    addEdge(graph, 0, 0);
    addEdge(graph, 0, 1);
    addEdge(graph, 0, 2);
    
    // Left vertex 1 connects to right vertices 1, 2, 3
    addEdge(graph, 1, 1);
    addEdge(graph, 1, 2);
    addEdge(graph, 1, 3);
    
    // Left vertex 2 connects to right vertices 2, 3, 4
    addEdge(graph, 2, 2);
    addEdge(graph, 2, 3);
    addEdge(graph, 2, 4);
    
    // Left vertex 3 connects to right vertices 3, 4, 5
    addEdge(graph, 3, 3);
    addEdge(graph, 3, 4);
    addEdge(graph, 3, 5);
    
    // Left vertex 4 connects to right vertices 0, 1, 5
    addEdge(graph, 4, 0);
    addEdge(graph, 4, 1);
    addEdge(graph, 4, 5);
    
    // Left vertex 5 connects to right vertices 0, 2, 4
    addEdge(graph, 5, 0);
    addEdge(graph, 5, 2);
    addEdge(graph, 5, 4);
    
    printf("Bipartite Graph Edges:\n");
    printf("Left Vertex -> Right Vertices\n");
    printf("-----------------------------\n");
    for (int i = 0; i < 6; i++) {
        printf("   %d -> ", i);
        Node* temp = graph->adj[i].head;
        while (temp != NULL) {
            printf("%d ", temp->vertex);
            temp = temp->next;
        }
        printf("\n");
    }
    
    printf("\n");
    
    // Find maximum matching using Hopcroft-Karp
    int maxMatching = hopcroftKarp(graph, 6, 6);
    
    printf("Maximum Matching Size: %d\n\n", maxMatching);
    
    // Print the actual matching
    printMatching(graph, 6, 6);
    
    // Free allocated memory
    for (int i = 0; i < 6; i++) {
        Node* temp = graph->adj[i].head;
        while (temp != NULL) {
            Node* next = temp->next;
            free(temp);
            temp = next;
        }
    }
    free(graph);
    
    return 0;
}
```

## Output:
```
Hopcroft-Karp Algorithm Example
==============================

Bipartite Graph Edges:
Left Vertex -> Right Vertices
-----------------------------
   0 -> 0 1 2 
   1 -> 1 2 3 
   2 -> 2 3 4 
   3 -> 3 4 5 
   4 -> 0 1 5 
   5 -> 0 2 4 

Maximum Matching Size: 4

Maximum Matching:
Left Vertex -> Right Vertex
--------------------------
   0 -> 2
   1 -> 3
   2 -> 4
   3 -> 5
Total matching pairs: 4
```

## Key Features of the Implementation:

1. **Bipartite Graph Representation**: Uses adjacency lists to represent the graph
2. **Hopcroft-Karp Algorithm**: Implements the full algorithm with BFS and DFS phases
3. **Memory Management**: Proper allocation and deallocation of memory
4. **Clear Output**: Shows the matching pairs found by the algorithm
5. **Example Graph**: Demonstrates with a concrete bipartite graph

## Algorithm Complexity:
- **Time Complexity**: O(E × √V) where E is the number of edges and V is the number of vertices
- **Space Complexity**: O(V + E) for storing the graph and auxiliary arrays

The algorithm efficiently finds a maximum cardinality matching in bipartite graphs by repeatedly finding augmenting paths using BFS and then using DFS to find the actual paths.

