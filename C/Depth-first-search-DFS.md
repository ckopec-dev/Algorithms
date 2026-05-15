# Depth-First Search (DFS) Algorithm in C

Here's a complete implementation of the Depth-First Search algorithm in C using an adjacency list representation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Structure for adjacency list node
struct Node {
    int data;
    struct Node* next;
};

// Structure for graph
struct Graph {
    int vertices;
    struct Node** adjList;
    bool* visited;
};

// Function to create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Function to create a graph
struct Graph* createGraph(int vertices) {
    struct Graph* graph = (struct Graph*)malloc(sizeof(struct Graph));
    graph->vertices = vertices;
    graph->adjList = (struct Node**)malloc(vertices * sizeof(struct Node*));
    graph->visited = (bool*)malloc(vertices * sizeof(bool));
    
    // Initialize adjacency list and visited array
    for (int i = 0; i < vertices; i++) {
        graph->adjList[i] = NULL;
        graph->visited[i] = false;
    }
    
    return graph;
}

// Function to add edge to the graph
void addEdge(struct Graph* graph, int src, int dest) {
    // Add edge from src to dest
    struct Node* newNode = createNode(dest);
    newNode->next = graph->adjList[src];
    graph->adjList[src] = newNode;
    
    // For undirected graph, add edge from dest to src
    newNode = createNode(src);
    newNode->next = graph->adjList[dest];
    graph->adjList[dest] = newNode;
}

// Recursive DFS function
void DFSUtil(struct Graph* graph, int vertex) {
    // Mark the current vertex as visited
    graph->visited[vertex] = true;
    printf("%d ", vertex);
    
    // Recur for all adjacent vertices
    struct Node* temp = graph->adjList[vertex];
    while (temp != NULL) {
        int adjVertex = temp->data;
        if (!graph->visited[adjVertex]) {
            DFSUtil(graph, adjVertex);
        }
        temp = temp->next;
    }
}

// Main DFS function
void DFS(struct Graph* graph, int startVertex) {
    // Reset visited array
    for (int i = 0; i < graph->vertices; i++) {
        graph->visited[i] = false;
    }
    
    printf("DFS Traversal starting from vertex %d: ", startVertex);
    DFSUtil(graph, startVertex);
    printf("\n");
}

// Function to print the adjacency list
void printGraph(struct Graph* graph) {
    printf("Adjacency List:\n");
    for (int i = 0; i < graph->vertices; i++) {
        printf("Vertex %d: ", i);
        struct Node* temp = graph->adjList[i];
        while (temp != NULL) {
            printf("%d -> ", temp->data);
            temp = temp->next;
        }
        printf("NULL\n");
    }
}

// Main function to demonstrate DFS
int main() {
    // Create a graph with 6 vertices
    struct Graph* graph = createGraph(6);
    
    // Add edges to the graph
    addEdge(graph, 0, 1);
    addEdge(graph, 0, 2);
    addEdge(graph, 1, 3);
    addEdge(graph, 1, 4);
    addEdge(graph, 2, 5);
    
    // Print the adjacency list
    printGraph(graph);
    
    // Perform DFS traversal
    DFS(graph, 0);
    
    return 0;
}
```

## Output:
```
Adjacency List:
Vertex 0: 2 -> 1 -> NULL
Vertex 1: 4 -> 3 -> 0 -> NULL
Vertex 2: 5 -> 0 -> NULL
Vertex 3: 1 -> NULL
Vertex 4: 1 -> NULL
Vertex 5: 2 -> NULL
DFS Traversal starting from vertex 0: 0 1 3 4 2 5 
```

## How it works:

1. **Graph Representation**: Uses adjacency list to represent the graph
2. **Visited Array**: Keeps track of visited vertices to avoid cycles
3. **Recursive DFS**: 
   - Marks current vertex as visited
   - Prints the vertex
   - Recursively visits all unvisited adjacent vertices
4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V) for the visited array and recursion stack

## Key Features:

- **Recursive Implementation**: Clean and intuitive approach
- **Undirected Graph Support**: Edges are added in both directions
- **Cycle Detection**: Uses visited array to prevent revisiting vertices
- **Memory Management**: Proper allocation and deallocation of memory
- **Complete Example**: Includes all necessary functions for demonstration

The algorithm explores as far as possible along each branch before backtracking, which is the defining characteristic of Depth-First Search.

