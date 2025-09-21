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

// Initialize stack
Stack* create_stack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = NULL;
    return stack;
}

// Push element to stack
void push(Stack* stack, int data) {
    Node* new_node = (Node*)malloc(sizeof(Node));
    new_node->data = data;
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

// Depth-limited DFS
bool depth_limited_dfs(Graph* graph, int start, int target, int depth_limit, Stack* stack) {
    if (start == target) {
        return true;
    }
    
    if (depth_limit <= 0) {
        return false;
    }
    
    // Push current node to stack
    push(stack, start);
    
    // Explore neighbors
    for (int i = 0; i < graph->adj_count[start]; i++) {
        int neighbor = graph->adj[start][i];
        
        // Check if neighbor is already in the path (avoid cycles)
        Node* temp = stack->top;
        bool in_path = false;
        while (temp != NULL) {
            if (temp->data == neighbor) {
                in_path = true;
                break;
            }
            temp = temp->next;
        }
        
        if (!in_path) {
            if (depth_limited_dfs(graph, neighbor, target, depth_limit - 1, stack)) {
                return true;
            }
        }
    }
    
    // Backtrack
    pop(stack);
    return false;
}

// IDDFS implementation
bool iddfs(Graph* graph, int start, int target) {
    for (int depth = 0; depth < graph->vertices; depth++) {
        Stack* stack = create_stack();
        
        printf("Searching with depth limit: %d\n", depth);
        
        if (depth_limited_dfs(graph, start, target, depth, stack)) {
            printf("Path found with depth %d\n", depth);
            // Clean up stack
            while (!is_stack_empty(stack)) {
                pop(stack);
            }
            free(stack);
            return true;
        }
        
        // Clean up stack for next iteration
        while (!is_stack_empty(stack)) {
            pop(stack);
        }
        free(stack);
    }
    
    return false;
}

// Print graph
void print_graph(Graph* graph) {
    printf("Graph adjacency list:\n");
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
    // Create a sample graph
    Graph* graph = create_graph(6);
    
    // Add edges to create a graph:
    // 0 -> 1, 2
    // 1 -> 3, 4
    // 2 -> 5
    // 3 -> 
    // 4 -> 
    // 5 -> 
    
    add_edge(graph, 0, 1);
    add_edge(graph, 0, 2);
    add_edge(graph, 1, 3);
    add_edge(graph, 1, 4);
    add_edge(graph, 2, 5);
    
    printf("Graph structure:\n");
    print_graph(graph);
    
    // Test IDDFS from node 0 to node 5
    int start = 0;
    int target = 5;
    
    printf("Starting IDDFS from node %d to node %d\n", start, target);
    
    if (iddfs(graph, start, target)) {
        printf("SUCCESS: Path found from %d to %d\n", start, target);
    } else {
        printf("FAILURE: No path found from %d to %d\n", start, target);
    }
    
    // Test with a node that doesn't exist
    int target2 = 6;
    printf("\nTesting with non-existent target %d:\n", target2);
    
    if (iddfs(graph, start, target2)) {
        printf("SUCCESS: Path found from %d to %d\n", start, target2);
    } else {
        printf("FAILURE: No path found from %d to %d\n", start, target2);
    }
    
    // Clean up
    free(graph);
    
    return 0;
}
```

## How IDDFS Works

The IDDFS algorithm combines the benefits of both depth-first search and breadth-first search:

1. **Iterative Deepening**: Starts with depth limit = 0, then increases by 1 each iteration
2. **Depth-Limited DFS**: Performs regular DFS but stops when reaching the specified depth limit
3. **Complete and Optimal**: Like BFS, it's complete (finds solution if exists) and optimal for unweighted graphs

## Key Features of This Implementation

- **Graph Representation**: Uses adjacency list for efficient storage
- **Stack-based DFS**: Implements iterative DFS using a stack data structure
- **Cycle Detection**: Prevents infinite loops by checking if nodes are already in the current path
- **Depth Limiting**: Each iteration limits the search depth to prevent going too deep
- **Memory Efficient**: Only stores one path at a time, unlike BFS which stores all nodes

## Time and Space Complexity

- **Time Complexity**: O(b^d) where b is branching factor and d is depth
- **Space Complexity**: O(b*d) where b is branching factor and d is maximum depth

The algorithm is particularly useful when the solution depth is unknown or when memory is limited, as it combines the memory efficiency of DFS with the completeness of BFS.

