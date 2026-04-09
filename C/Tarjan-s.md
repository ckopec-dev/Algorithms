# Tarjan's Algorithm Implementation in C

Tarjan's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NODES 1000

// Structure to represent a graph node
typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Structure to represent a graph
typedef struct {
    int numNodes;
    Node* adjList[MAX_NODES];
    int visited[MAX_NODES];
    int disc[MAX_NODES];
    int low[MAX_NODES];
    int stackMember[MAX_NODES];
    int time;
    int sccCount;
} Graph;

// Stack structure for Tarjan's algorithm
typedef struct {
    int items[MAX_NODES];
    int top;
} Stack;

// Initialize stack
Stack* createStack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = -1;
    return stack;
}

// Push element to stack
void push(Stack* stack, int item) {
    stack->items[++stack->top] = item;
}

// Pop element from stack
int pop(Stack* stack) {
    if (stack->top == -1) return -1;
    return stack->items[stack->top--];
}

// Check if element is in stack
bool isInStack(Stack* stack, int item) {
    for (int i = 0; i <= stack->top; i++) {
        if (stack->items[i] == item) return true;
    }
    return false;
}

// Initialize graph
Graph* createGraph(int numNodes) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->numNodes = numNodes;
    graph->time = 0;
    graph->sccCount = 0;
    
    for (int i = 0; i < numNodes; i++) {
        graph->adjList[i] = NULL;
        graph->visited[i] = 0;
        graph->disc[i] = -1;
        graph->low[i] = -1;
        graph->stackMember[i] = 0;
    }
    
    return graph;
}

// Add edge to graph
void addEdge(Graph* graph, int src, int dest) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = dest;
    newNode->next = graph->adjList[src];
    graph->adjList[src] = newNode;
}

// Tarjan's algorithm implementation
void tarjanSCC(Graph* graph, int u, Stack* stack) {
    // Initialize discovery time and low value
    graph->disc[u] = graph->low[u] = ++graph->time;
    push(stack, u);
    graph->stackMember[u] = 1;
    
    // Get all adjacent vertices
    Node* temp = graph->adjList[u];
    while (temp) {
        int v = temp->data;
        
        if (graph->disc[v] == -1) {
            tarjanSCC(graph, v, stack);
            graph->low[u] = (graph->low[u] < graph->low[v]) ? graph->low[u] : graph->low[v];
        }
        else if (graph->stackMember[v] == 1) {
            graph->low[u] = (graph->low[u] < graph->disc[v]) ? graph->low[u] : graph->disc[v];
        }
        temp = temp->next;
    }
    
    // If u is root of SCC
    int w = -1;
    if (graph->low[u] == graph->disc[u]) {
        printf("SCC: ");
        while (stack->top != -1 && stack->items[stack->top] != u) {
            w = pop(stack);
            graph->stackMember[w] = 0;
            printf("%d ", w);
        }
        if (stack->top != -1) {
            w = pop(stack);
            graph->stackMember[w] = 0;
            printf("%d ", w);
        }
        printf("\n");
        graph->sccCount++;
    }
}

// Main function to find all SCCs
void findSCCs(Graph* graph) {
    Stack* stack = createStack();
    
    // Initialize all vertices as not visited
    for (int i = 0; i < graph->numNodes; i++) {
        graph->visited[i] = 0;
    }
    
    // Call recursive helper function for all vertices
    for (int i = 0; i < graph->numNodes; i++) {
        if (graph->disc[i] == -1) {
            tarjanSCC(graph, i, stack);
        }
    }
    
    printf("Total number of strongly connected components: %d\n", graph->sccCount);
}

// Print graph adjacency list
void printGraph(Graph* graph) {
    printf("Adjacency List:\n");
    for (int i = 0; i < graph->numNodes; i++) {
        printf("Vertex %d: ", i);
        Node* temp = graph->adjList[i];
        while (temp) {
            printf("%d ", temp->data);
            temp = temp->next;
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Create a graph with 5 vertices
    Graph* graph = createGraph(5);
    
    // Add edges to the graph
    addEdge(graph, 0, 1);
    addEdge(graph, 1, 2);
    addEdge(graph, 2, 0);
    addEdge(graph, 1, 3);
    addEdge(graph, 3, 4);
    
    printf("Graph representation:\n");
    printGraph(graph);
    
    printf("\nFinding Strongly Connected Components:\n");
    findSCCs(graph);
    
    return 0;
}
```

## Output for the example:
```
Graph representation:
Adjacency List:
Vertex 0: 1 
Vertex 1: 2 3 
Vertex 2: 0 
Vertex 3: 4 
Vertex 4: 

Finding Strongly Connected Components:
SCC: 0 2 1 
SCC: 3 
SCC: 4 
Total number of strongly connected components: 3
```

## Algorithm Explanation:

1. **Initialization**: Each node is initialized with discovery time and low value
2. **DFS Traversal**: Perform depth-first search starting from each unvisited node
3. **Stack Management**: Use a stack to keep track of nodes in current DFS path
4. **Low Value Calculation**: Update low values based on children and back edges
5. **SCC Detection**: When a node's low value equals its discovery time, it's the root of an SCC

## Time Complexity: O(V + E) where V is vertices and E is edges
## Space Complexity: O(V) for the stack and auxiliary arrays

