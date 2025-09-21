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

// Function to create a new node
Node* create_node(int data) {
    Node* new_node = (Node*)malloc(sizeof(Node));
    new_node->data = data;
    new_node->next = NULL;
    return new_node;
}

// Initialize stack
Stack* create_stack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = NULL;
    return stack;
}

// Push element to stack
void push(Stack* stack, int data) {
    Node* new_node = create_node(data);
    new_node->next = stack->top;
    stack->top = new_node;
}

// Pop element from stack
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

// Check if stack is empty
bool is_stack_empty(Stack* stack) {
    return stack->top == NULL;
}

// Initialize graph
Graph* create_graph(int vertices) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->vertices = vertices;
    for (int i = 0; i < vertices; i++) {
        graph->adj_count[i] = 0;
    }
    return graph;
}

// Add edge to the graph
void add_edge(Graph* graph, int src, int dest) {
    graph->adj[src][graph->adj_count[src]++] = dest;
}

// Depth-limited DFS
bool dls_util(Graph* graph, int start, int target, int depth, Stack* stack, bool visited[]) {
    if (depth == 0 && start == target) {
        push(stack, start);
        return true;
    }
    
    if (depth <= 0) {
        return false;
    }
    
    visited[start] = true;
    push(stack, start);
    
    // Explore neighbors
    for (int i = 0; i < graph->adj_count[start]; i++) {
        int neighbor = graph->adj[start][i];
        if (!visited[neighbor]) {
            if (dls_util(graph, neighbor, target, depth - 1, stack, visited)) {
                return true;
            }
        }
    }
    
    // Backtrack
    pop(stack);
    visited[start] = false;
    return false;
}

// Iterative Deepening DFS
bool iddfs(Graph* graph, int start, int target) {
    Stack* stack = create_stack();
    bool visited[MAX_NODES];
    
    for (int depth = 0; depth < graph->vertices; depth++) {
        // Reset visited array for each depth
        for (int i = 0; i < graph->vertices; i++) {
            visited[i] = false;
        }
        
        printf("Searching with depth limit: %d\n", depth);
        
        if (dls_util(graph, start, target, depth, stack, visited)) {
            printf("Path found!\n");
            
            // Print the path
            printf("Path: ");
            Node* temp = stack->top;
            while (temp != NULL) {
                printf("%d ", temp->data);
                temp = temp->next;
            }
            printf("\n");
            
            // Clean up stack
            while (!is_stack_empty(stack)) {
                pop(stack);
            }
            return true;
        }
    }
    
    printf("No path found\n");
    return false;
}

// Print graph representation
void print_graph(Graph* graph) {
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

// Example usage
int main() {
    // Create a graph with 6 vertices
    Graph* graph = create_graph(6);
    
    // Add edges to the graph (undirected)
    add_edge(graph, 0, 1);
    add_edge(graph, 0, 2);
    add_edge(graph, 1, 3);
    add_edge(graph, 1, 4);
    add_edge(graph, 2, 5);
    add_edge(graph, 3, 5);
    add_edge(graph, 4, 5);
    
    printf("=== Iterative Deepening DFS Example ===\n\n");
    
    print_graph(graph);
    
    int start = 0;
    int target = 5;
    
    printf("Searching for path from vertex %d to vertex %d:\n", start, target);
    
    if (iddfs(graph, start, target)) {
        printf("\nSuccess: Path found using IDDFS!\n");
    } else {
        printf("\nFailure: No path exists between the vertices.\n");
    }
    
    return 0;
}
```

## How IDDFS Works

The Iterative Deepening Depth-First Search algorithm combines the benefits of both BFS and DFS:

1. **Depth-Limited Search (DLS)**: Performs DFS with a limited depth
2. **Iterative Deepening**: Gradually increases the depth limit from 0 to maximum
3. **Optimal**: Finds the shortest path like BFS but uses less memory like DFS

## Key Features of This Implementation

- **Graph Representation**: Uses adjacency list for efficient storage
- **Stack-based DFS**: Implements DFS using a stack data structure
- **Backtracking**: Properly handles backtracking during search
- **Path Reconstruction**: Records and displays the found path
- **Memory Efficient**: Only stores nodes at current depth level
- **Optimal Solution**: Guarantees shortest path for unweighted graphs

## Time and Space Complexity

- **Time Complexity**: O(b^d) where b is branching factor and d is depth
- **Space Complexity**: O(d) where d is maximum depth

This implementation demonstrates how IDDFS systematically explores the search space by incrementally increasing depth limits, making it ideal for large search spaces where memory is limited.

