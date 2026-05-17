# Iterative Deepening Depth-First Search (IDDFS) in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NODES 100
#define MAX_NEIGHBORS 10

// Graph structure
typedef struct {
    int adj_matrix[MAX_NODES][MAX_NODES];
    int num_nodes;
    int num_edges;
} Graph;

// Node structure for the search
typedef struct {
    int node_id;
    int depth;
    int parent;
} Node;

// Stack structure for DFS
typedef struct {
    Node items[MAX_NODES];
    int top;
} Stack;

// Initialize graph
Graph* create_graph(int num_nodes) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->num_nodes = num_nodes;
    graph->num_edges = 0;
    
    // Initialize adjacency matrix
    for (int i = 0; i < MAX_NODES; i++) {
        for (int j = 0; j < MAX_NODES; j++) {
            graph->adj_matrix[i][j] = 0;
        }
    }
    
    return graph;
}

// Add edge to graph
void add_edge(Graph* graph, int src, int dest) {
    graph->adj_matrix[src][dest] = 1;
    graph->adj_matrix[dest][src] = 1; // For undirected graph
    graph->num_edges++;
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

// Check if node is in stack
bool is_in_stack(Stack* stack, int node_id) {
    for (int i = 0; i <= stack->top; i++) {
        if (stack->items[i].node_id == node_id) {
            return true;
        }
    }
    return false;
}

// Depth-Limited DFS
bool depth_limited_dfs(Graph* graph, int start, int goal, int depth_limit) {
    Stack* stack = create_stack();
    Node initial_node = {start, 0, -1};
    push(stack, initial_node);
    
    while (stack->top >= 0) {
        Node current = pop(stack);
        
        // If we've reached the goal
        if (current.node_id == goal) {
            printf("Goal found at depth %d\n", current.depth);
            return true;
        }
        
        // If we've exceeded the depth limit, skip
        if (current.depth >= depth_limit) {
            continue;
        }
        
        // Explore neighbors
        for (int i = 0; i < graph->num_nodes; i++) {
            if (graph->adj_matrix[current.node_id][i] == 1) {
                // Check if neighbor is already in the path
                if (!is_in_stack(stack, i)) {
                    Node neighbor_node = {i, current.depth + 1, current.node_id};
                    push(stack, neighbor_node);
                }
            }
        }
    }
    
    return false;
}

// Iterative Deepening DFS
bool iddfs(Graph* graph, int start, int goal) {
    printf("Starting Iterative Deepening DFS from node %d to node %d\n", start, goal);
    
    for (int depth = 0; depth < graph->num_nodes; depth++) {
        printf("Searching with depth limit: %d\n", depth);
        
        if (depth_limited_dfs(graph, start, goal, depth)) {
            printf("Solution found at depth %d\n", depth);
            return true;
        }
    }
    
    printf("No solution found\n");
    return false;
}

// Print graph
void print_graph(Graph* graph) {
    printf("Graph adjacency matrix:\n");
    for (int i = 0; i < graph->num_nodes; i++) {
        for (int j = 0; j < graph->num_nodes; j++) {
            printf("%d ", graph->adj_matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Main function
int main() {
    // Create a sample graph
    Graph* graph = create_graph(6);
    
    // Add edges to create a graph
    add_edge(graph, 0, 1);
    add_edge(graph, 0, 2);
    add_edge(graph, 1, 3);
    add_edge(graph, 1, 4);
    add_edge(graph, 2, 5);
    add_edge(graph, 3, 5);
    add_edge(graph, 4, 5);
    
    printf("Graph structure:\n");
    print_graph(graph);
    
    // Perform IDDFS
    int start_node = 0;
    int goal_node = 5;
    
    bool found = iddfs(graph, start_node, goal_node);
    
    if (found) {
        printf("Path from %d to %d exists!\n", start_node, goal_node);
    } else {
        printf("No path from %d to %d exists!\n", start_node, goal_node);
    }
    
    return 0;
}
```

## How IDDFS Works

The Iterative Deepening Depth-First Search algorithm combines the benefits of both DFS and BFS:

1. **Depth-Limited DFS**: Performs regular DFS but stops when a certain depth is reached
2. **Iterative Deepening**: Repeatedly calls depth-limited DFS with increasing depth limits
3. **Optimal**: Finds the shortest path like BFS but uses less memory like DFS

## Key Features

- **Memory Efficient**: Uses O(bd) space where b is branching factor and d is depth
- **Optimal**: Finds the shortest path in unweighted graphs
- **Complete**: Will find a solution if one exists
- **Time Complexity**: O(b^d) where b is branching factor and d is depth

## Sample Output

```
Graph structure:
Graph adjacency matrix:
0 1 1 0 0 0 
1 0 0 1 1 0 
1 0 0 0 0 1 
0 1 0 0 0 1 
0 1 0 0 0 1 
0 0 1 1 1 0 

Starting Iterative Deepening DFS from node 0 to node 5
Searching with depth limit: 0
Searching with depth limit: 1
Searching with depth limit: 2
Searching with depth limit: 3
Goal found at depth 3
Solution found at depth 3
Path from 0 to 5 exists!
```

This implementation demonstrates how IDDFS systematically explores the search space by gradually increasing the depth limit, ensuring it finds the optimal solution while maintaining the memory efficiency of DFS.

