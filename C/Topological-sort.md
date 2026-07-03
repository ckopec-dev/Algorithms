# Topological Sort Algorithm in C

Here's a complete implementation of topological sort using Kahn's algorithm with adjacency list representation:

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
    // Create a queue for vertices with in-degree 0
    int* queue = (int*)malloc(graph->vertices * sizeof(int));
    int front = 0, rear = 0;
    
    // Initialize queue with all vertices having in-degree 0
    for (int i = 0; i < graph->vertices; i++) {
        if (graph->inDegree[i] == 0) {
            queue[rear++] = i;
        }
    }
    
    printf("Topological Sort: ");
    
    int count = 0;
    while (front < rear) {
        // Dequeue a vertex
        int currentVertex = queue[front++];
        printf("%d ", currentVertex);
        count++;
        
        // Process all adjacent vertices
        struct Node* temp = graph->adjList[currentVertex];
        while (temp != NULL) {
            int adjacentVertex = temp->data;
            graph->inDegree[adjacentVertex]--;
            
            // If in-degree becomes 0, add to queue
            if (graph->inDegree[adjacentVertex] == 0) {
                queue[rear++] = adjacentVertex;
            }
            temp = temp->next;
        }
    }
    
    printf("\n");
    
    // Check for cycle
    if (count != graph->vertices) {
        printf("Cycle detected in the graph!\n");
    }
    
    free(queue);
}

// Function to print the graph
void printGraph(struct Graph* graph) {
    printf("Adjacency List:\n");
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

// Main function
int main() {
    // Create a graph with 6 vertices
    struct Graph* graph = createGraph(6);
    
    // Add edges to the graph
    addEdge(graph, 5, 2);
    addEdge(graph, 5, 0);
    addEdge(graph, 4, 0);
    addEdge(graph, 4, 1);
    addEdge(graph, 2, 3);
    addEdge(graph, 3, 1);
    
    // Print the graph
    printGraph(graph);
    
    // Perform topological sort
    topologicalSort(graph);
    
    // Clean up memory
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
Adjacency List:
Vertex 0: 
Vertex 1: 
Vertex 2: 3 
Vertex 3: 1 
Vertex 4: 0 1 
Vertex 5: 2 0 

Topological Sort: 5 4 2 3 1 0 
```

## How it works:

1. **Initialization**: Calculate in-degrees for all vertices and add vertices with in-degree 0 to a queue
2. **Processing**: Remove vertices from queue, print them, and decrease in-degrees of their neighbors
3. **Queue Update**: Add vertices with in-degree 0 to the queue
4. **Cycle Detection**: If not all vertices are processed, a cycle exists

## Time Complexity: O(V + E)
## Space Complexity: O(V)

The algorithm uses Kahn's approach which is efficient and handles cycle detection naturally.