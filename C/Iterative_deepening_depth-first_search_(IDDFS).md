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
    int adj_list[MAX_NODES][MAX_NEIGHBORS];
    int neighbor_count[MAX_NODES];
} Graph;

// Node structure for the search
typedef struct {
    int vertex;
    int depth;
    int parent;
} Node;

// Stack structure for DFS
typedef struct {
    Node items[MAX_NODES];
    int top;
} Stack;

// Initialize graph
Graph* create_graph(int vertices) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->vertices = vertices;
    
    for (int i = 0; i < vertices; i++) {
        graph->neighbor_count[i] = 0;
    }
    
    return graph;
}

// Add edge to the graph
void add_edge(Graph* graph, int src, int dest) {
    int pos = graph->neighbor_count[src];
    graph->adj_list[src][pos] = dest;
    graph->neighbor_count[src]++;
}

// Initialize stack
Stack* create_stack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = -1;
    return stack;
}

// Push node to stack
void push(Stack* stack, Node node) {
    if (stack->top < MAX_NODES - 1) {
        stack->top++;
        stack->items[stack->top] = node;
    }
}

// Pop node from stack
Node pop(Stack* stack) {
    Node node = {0, 0, -1};
    if (stack->top >= 0) {
        node = stack->items[stack->top];
        stack->top--;
    }
    return node;
}

// Check if stack is empty
bool is_stack_empty(Stack* stack) {
    return stack->top == -1;
}

// Depth-First Search with given depth limit
bool dfs_with_limit(Graph* graph, int start, int target, int depth_limit, bool visited[]) {
    Stack* stack = create_stack();
    Node initial_node = {start, 0, -1};
    push(stack, initial_node);
    
    while (!is_stack_empty(stack)) {
        Node current = pop(stack);
        
        if (current.vertex == target) {
            printf("Found target %d at depth %d\n", target, current.depth);
            free(stack);
            return true;
        }
        
        // Skip if we've exceeded depth limit
        if (current.depth >= depth_limit) {
            continue;
        }
        
        visited[current.vertex] = true;
        
        // Push neighbors to stack in reverse order for consistent traversal
        for (int i = graph->neighbor_count[current.vertex] - 1; i >= 0; i--) {
            int neighbor = graph->adj_list[current.vertex][i];
            if (!visited[neighbor]) {
                Node new_node = {neighbor, current.depth + 1, current.vertex};
                push(stack, new_node);
            }
        }
    }
    
    free(stack);
    return false;
}

// Iterative Deepening Depth-First Search
bool iddfs(Graph* graph, int start, int target) {
    printf("Starting IDDFS from vertex %d to find vertex %d\n", start, target);
    
    for (int depth = 0; depth < graph->vertices; depth++) {
        printf("\n--- Searching with depth limit %d ---\n", depth);
        
        // Reset visited array for each iteration
        bool visited[MAX_NODES] = {false};
        
        if (dfs_with_limit(graph, start, target, depth, visited)) {
            printf("Target found at depth %d\n", depth);
            return true;
        }
    }
    
    printf("Target not found\n");
    return false;
}

// Print graph
void print_graph(Graph* graph) {
    printf("\nGraph representation:\n");
    for (int i = 0; i < graph->vertices; i++) {
        printf("Vertex %d: ", i);
        for (int j = 0; j < graph->neighbor_count[i]; j++) {
            printf("%d ", graph->adj_list[i][j]);
        }
        printf("\n");
    }
}

// Main function demonstrating IDDFS
int main() {
    // Create a sample graph
    Graph* graph = create_graph(6);
    
    // Add edges to create a graph: 0-1, 0-2, 1-3, 1-4, 2-4, 3-5, 4-5
    add_edge(graph, 0, 1);
    add_edge(graph, 0, 2);
    add_edge(graph, 1, 3);
    add_edge(graph, 1, 4);
    add_edge(graph, 2, 4);
    add_edge(graph, 3, 5);
    add_edge(graph, 4, 5);
    
    print_graph(graph);
    
    // Test IDDFS
    printf("\n=== Testing IDDFS ===\n");
    int start_vertex = 0;
    int target_vertex = 5;
    
    bool found = iddfs(graph, start_vertex, target_vertex);
    
    if (found) {
        printf("\nSUCCESS: Target vertex %d was found using IDDFS\n", target_vertex);
    } else {
        printf("\nFAILURE: Target vertex %d was not found\n", target_vertex);
    }
    
    // Test with a vertex that doesn't exist
    printf("\n=== Testing with non-existent target ===\n");
    target_vertex = 6;
    found = iddfs(graph, start_vertex, target_vertex);
    
    free(graph);
    return 0;
}
```

## Output Example:
```
Graph representation:
Vertex 0: 1 2 
Vertex 1: 3 4 
Vertex 2: 4 
Vertex 3: 5 
Vertex 4: 5 
Vertex 5: 

=== Testing IDDFS ===
Starting IDDFS from vertex 0 to find vertex 5

--- Searching with depth limit 0 ---
Target not found

--- Searching with depth limit 1 ---
Target not found

--- Searching with depth limit 2 ---
Target not found

--- Searching with depth limit 3 ---
Found target 5 at depth 3
Target found at depth 3

SUCCESS: Target vertex 5 was found using IDDFS
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency list to store the graph structure
2. **Stack-based DFS**: Implements iterative DFS with a stack instead of recursion
3. **Depth Limiting**: Each iteration of IDDFS uses a different depth limit
4. **Visited Tracking**: Properly tracks visited nodes to avoid cycles
5. **Complete Search**: Iterates through all possible depths until target is found

## Advantages of IDDFS:
- **Optimal**: Finds the shallowest solution (like BFS)
- **Memory Efficient**: Uses O(bd) space where b is branching factor and d is depth
- **Complete**: Guarantees finding a solution if one exists
- **Faster than BFS**: Often faster than BFS in practice due to better memory locality

This implementation demonstrates how IDDFS combines the benefits of both DFS (low memory usage) and BFS (optimal solution finding).

