# Iterative Deepening Depth-First Search (IDDFS) in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NODES 100
#define MAX_NEIGHBORS 10

// Graph representation using adjacency list
typedef struct {
    int vertices;
    int adj[MAX_NODES][MAX_NEIGHBORS];
    int adj_count[MAX_NODES];
} Graph;

// Node structure for the search
typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Stack structure for DFS
typedef struct {
    Node* top;
} Stack;

// Function to create a new graph
Graph* createGraph(int vertices) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->vertices = vertices;
    
    for (int i = 0; i < vertices; i++) {
        graph->adj_count[i] = 0;
    }
    
    return graph;
}

// Function to add edge to the graph
void addEdge(Graph* graph, int src, int dest) {
    graph->adj[src][graph->adj_count[src]] = dest;
    graph->adj_count[src]++;
}

// Function to create a new stack
Stack* createStack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = NULL;
    return stack;
}

// Function to push element to stack
void push(Stack* stack, int data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = data;
    newNode->next = stack->top;
    stack->top = newNode;
}

// Function to pop element from stack
int pop(Stack* stack) {
    if (stack->top == NULL) {
        return -1;
    }
    
    Node* temp = stack->top;
    int data = temp->data;
    stack->top = stack->top->next;
    free(temp);
    return data;
}

// Function to check if stack is empty
bool isEmpty(Stack* stack) {
    return stack->top == NULL;
}

// Depth-limited DFS search
bool depthLimitedDFS(Graph* graph, int start, int target, int limit, Stack* stack) {
    if (start == target) {
        return true;
    }
    
    if (limit <= 0) {
        return false;
    }
    
    // Mark current node as visited by pushing to stack
    push(stack, start);
    
    // Explore neighbors
    for (int i = 0; i < graph->adj_count[start]; i++) {
        int neighbor = graph->adj[start][i];
        
        // Check if neighbor is not already in the path
        Node* temp = stack->top;
        bool found = false;
        while (temp != NULL) {
            if (temp->data == neighbor) {
                found = true;
                break;
            }
            temp = temp->next;
        }
        
        if (!found) {
            if (depthLimitedDFS(graph, neighbor, target, limit - 1, stack)) {
                return true;
            }
        }
    }
    
    // Backtrack
    pop(stack);
    return false;
}

// Iterative Deepening Depth-First Search
bool iddfs(Graph* graph, int start, int target, int maxDepth) {
    Stack* stack = createStack();
    
    for (int depth = 0; depth <= maxDepth; depth++) {
        printf("Searching with depth limit: %d\n", depth);
        
        // Clear the stack for each iteration
        while (!isEmpty(stack)) {
            pop(stack);
        }
        
        if (depthLimitedDFS(graph, start, target, depth, stack)) {
            printf("Target found at depth %d\n", depth);
            
            // Print the path
            printf("Path: ");
            Node* temp = stack->top;
            while (temp != NULL) {
                printf("%d ", temp->data);
                temp = temp->next;
            }
            printf("%d\n", start);
            
            free(stack);
            return true;
        }
    }
    
    free(stack);
    return false;
}

// Function to print the graph
void printGraph(Graph* graph) {
    printf("Graph representation:\n");
    for (int i = 0; i < graph->vertices; i++) {
        printf("Vertex %d: ", i);
        for (int j = 0; j < graph->adj_count[i]; j++) {
            printf("%d ", graph->adj[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Main function to demonstrate IDDFS
int main() {
    // Create a sample graph
    Graph* graph = createGraph(6);
    
    // Add edges to the graph
    addEdge(graph, 0, 1);
    addEdge(graph, 0, 2);
    addEdge(graph, 1, 3);
    addEdge(graph, 1, 4);
    addEdge(graph, 2, 5);
    addEdge(graph, 3, 6);  // This creates a path from 0->1->3->6
    addEdge(graph, 4, 7);  // This creates a path from 0->1->4->7
    
    printf("Graph created:\n");
    printGraph(graph);
    
    int start = 0;
    int target = 6;  // Target node to search for
    int maxDepth = 5;  // Maximum depth to search
    
    printf("Starting IDDFS from node %d to find node %d\n", start, target);
    
    if (iddfs(graph, start, target, maxDepth)) {
        printf("\nSearch successful!\n");
    } else {
        printf("\nSearch failed - target not found within maximum depth.\n");
    }
    
    // Clean up memory
    free(graph);
    
    return 0;
}
```

## How IDDFS Works

The Iterative Deepening Depth-First Search algorithm combines the benefits of both:
1. **Depth-First Search (DFS)**: Uses little memory
2. **Breadth-First Search (BFS)**: Guarantees finding the shortest path

### Key Features:

1. **Iterative Process**: Starts with depth limit 0, then increases by 1 each iteration
2. **Memory Efficient**: Only stores the current path in memory
3. **Optimal**: Finds the shortest path to the target (like BFS)
4. **Complete**: Will find a solution if one exists

### Algorithm Steps:

1. Set initial depth limit to 0
2. Perform depth-limited DFS with current depth limit
3. If solution found, return it
4. If not found, increment depth limit and repeat
5. Continue until maximum depth is reached

### Time Complexity:
- **Worst case**: O(b^d) where b is branching factor and d is depth
- **Space complexity**: O(d) where d is maximum depth

This implementation demonstrates how IDDFS can be used to search for a target node in a graph while maintaining the memory efficiency of DFS and the completeness of BFS.

