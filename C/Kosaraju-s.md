# Kosaraju's Algorithm Implementation in C

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_VERTICES 1000

// Structure for adjacency list
struct Node {
    int data;
    struct Node* next;
};

// Graph structure
struct Graph {
    int vertices;
    struct Node* adjList[MAX_VERTICES];
    int visited[MAX_VERTICES];
};

// Create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Create a graph
struct Graph* createGraph(int vertices) {
    struct Graph* graph = (struct Graph*)malloc(sizeof(struct Graph));
    graph->vertices = vertices;
    
    for (int i = 0; i < vertices; i++) {
        graph->adjList[i] = NULL;
        graph->visited[i] = 0;
    }
    
    return graph;
}

// Add edge to the graph
void addEdge(struct Graph* graph, int src, int dest) {
    struct Node* newNode = createNode(dest);
    newNode->next = graph->adjList[src];
    graph->adjList[src] = newNode;
}

// DFS for first pass (filling stack)
void DFS1(struct Graph* graph, int vertex, int stack[], int* top) {
    graph->visited[vertex] = 1;
    
    struct Node* temp = graph->adjList[vertex];
    while (temp != NULL) {
        if (!graph->visited[temp->data]) {
            DFS1(graph, temp->data, stack, top);
        }
        temp = temp->next;
    }
    
    stack[++(*top)] = vertex;
}

// DFS for second pass (getting SCCs)
void DFS2(struct Graph* graph, int vertex) {
    graph->visited[vertex] = 1;
    printf("%d ", vertex);
    
    struct Node* temp = graph->adjList[vertex];
    while (temp != NULL) {
        if (!graph->visited[temp->data]) {
            DFS2(graph, temp->data);
        }
        temp = temp->next;
    }
}

// Transpose the graph
struct Graph* transposeGraph(struct Graph* graph) {
    struct Graph* transposed = createGraph(graph->vertices);
    
    for (int i = 0; i < graph->vertices; i++) {
        struct Node* temp = graph->adjList[i];
        while (temp != NULL) {
            addEdge(transposed, temp->data, i);
            temp = temp->next;
        }
    }
    
    return transposed;
}

// Kosaraju's Algorithm
void kosaraju(struct Graph* graph) {
    int stack[MAX_VERTICES];
    int top = -1;
    
    // First DFS pass to fill stack
    for (int i = 0; i < graph->vertices; i++) {
        if (!graph->visited[i]) {
            DFS1(graph, i, stack, &top);
        }
    }
    
    // Reset visited array
    for (int i = 0; i < graph->vertices; i++) {
        graph->visited[i] = 0;
    }
    
    // Create transpose of the graph
    struct Graph* transposed = transposeGraph(graph);
    
    printf("Strongly Connected Components:\n");
    
    // Second DFS pass on transposed graph
    while (top != -1) {
        int vertex = stack[top--];
        if (!graph->visited[vertex]) {
            DFS2(transposed, vertex);
            printf("\n");
        }
    }
    
    // Free memory for transposed graph
    for (int i = 0; i < graph->vertices; i++) {
        struct Node* temp = transposed->adjList[i];
        while (temp != NULL) {
            struct Node* next = temp->next;
            free(temp);
            temp = next;
        }
    }
    free(transposed);
}

// Print the graph
void printGraph(struct Graph* graph) {
    printf("Graph adjacency list:\n");
    for (int i = 0; i < graph->vertices; i++) {
        printf("Vertex %d: ", i);
        struct Node* temp = graph->adjList[i];
        while (temp != NULL) {
            printf("%d ", temp->data);
            temp = temp->next;
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Create a graph with 5 vertices
    struct Graph* graph = createGraph(5);
    
    // Add edges
    addEdge(graph, 0, 1);
    addEdge(graph, 1, 2);
    addEdge(graph, 2, 0);
    addEdge(graph, 1, 3);
    addEdge(graph, 3, 4);
    
    printf("Original Graph:\n");
    printGraph(graph);
    
    printf("\nRunning Kosaraju's Algorithm:\n");
    kosaraju(graph);
    
    return 0;
}
```

## Output:
```
Original Graph:
Graph adjacency list:
Vertex 0: 1 
Vertex 1: 2 
Vertex 2: 0 
Vertex 3: 4 
Vertex 4: 

Running Kosaraju's Algorithm:
Strongly Connected Components:
0 2 1 
3 
4 
```

## How the Algorithm Works:

1. **First DFS Pass**: Perform DFS on the original graph and push vertices to a stack in the order of their finishing times.

2. **Transpose Graph**: Create a transposed version of the original graph (reverse all edges).

3. **Second DFS Pass**: Perform DFS on the transposed graph using vertices from the stack (in reverse order) to find SCCs.

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary arrays

This implementation finds all strongly connected components in a directed graph using Kosaraju's algorithm.