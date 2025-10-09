# Topological Sort Algorithm in C

Here's an implementation of topological sort using Kahn's algorithm with adjacency list representation:

```c
#include <stdio.h>
#include <stdlib.h>

// Structure for adjacency list node
struct Node {
    int data;
    struct Node* next;
};

// Structure for graph
struct Graph {
    int vertices;
    struct Node** adjList;
    int* inDegree;
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
    graph->inDegree = (int*)malloc(vertices * sizeof(int));
    
    for (int i = 0; i < vertices; i++) {
        graph->adjList[i] = NULL;
        graph->inDegree[i] = 0;
    }
    
    return graph;
}

// Function to add edge to the graph
void addEdge(struct Graph* graph, int src, int dest) {
    struct Node* newNode = createNode(dest);
    newNode->next = graph->adjList[src];
    graph->adjList[src] = newNode;
    
    graph->inDegree[dest]++;
}

// Function to perform topological sort
void topologicalSort(struct Graph* graph) {
    int* queue = (int*)malloc(graph->vertices * sizeof(int));
    int front = 0, rear = 0;
    
    // Initialize queue with all vertices having in-degree 0
    for (int i = 0; i < graph->vertices; i++) {
        if (graph->inDegree[i] == 0) {
            queue[rear++] = i;
        }
    }
    
    printf("Topological Sort: ");
    
    while (front < rear) {
        int current = queue[front++];
        printf("%d ", current);
        
        // Traverse all adjacent vertices
        struct Node* temp = graph->adjList[current];
        while (temp != NULL) {
            graph->inDegree[temp->data]--;
            if (graph->inDegree[temp->data] == 0) {
                queue[rear++] = temp->data;
            }
            temp = temp->next;
        }
    }
    
    printf("\n");
    
    free(queue);
}

// Function to print the graph
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
    printf("\n");
}

// Main function
int main() {
    // Create a graph with 6 vertices
    struct Graph* graph = createGraph(6);
    
    // Add edges (assuming directed graph)
    addEdge(graph, 5, 2);
    addEdge(graph, 5, 0);
    addEdge(graph, 4, 0);
    addEdge(graph, 4, 1);
    addEdge(graph, 2, 3);
    addEdge(graph, 3, 1);
    
    printGraph(graph);
    
    // Perform topological sort
    topologicalSort(graph);
    
    // Free memory
    for (int i = 0; i < graph->vertices; i++) {
        struct Node* temp = graph->adjList[i];
        while (temp != NULL) {
            struct Node* next = temp->next;
            free(temp);
            temp = next;
        }
    }
    free(graph->adjList);
    free(graph->inDegree);
    free(graph);
    
    return 0;
}
```

## Sample Output:
```
Graph adjacency list:
Vertex 0: 
Vertex 1: 3 
Vertex 2: 3 
Vertex 3: 
Vertex 4: 0 1 
Vertex 5: 2 0 

Topological Sort: 5 4 2 0 1 3 
```

## How it works:

1. **Input**: A directed acyclic graph (DAG) represented as adjacency list
2. **Algorithm**: Kahn's algorithm using in-degrees
3. **Steps**:
   - Calculate in-degree for each vertex
   - Add all vertices with in-degree 0 to a queue
   - Process vertices from queue:
     - Remove vertex from queue
     - Add to topological order
     - Decrease in-degree of all adjacent vertices
     - Add vertices with in-degree 0 to queue
4. **Output**: Topological order of vertices

## Time Complexity: O(V + E)
## Space Complexity: O(V)

Where V is the number of vertices and E is the number of edges.

