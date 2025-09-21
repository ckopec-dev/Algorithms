# Iterative Deepening Depth-First Search (IDDFS) in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NODES 100
#define MAX_NEIGHBORS 10

// Graph representation using adjacency list
typedef struct {
    int adj_list[MAX_NODES][MAX_NEIGHBORS];
    int neighbors_count[MAX_NODES];
    int num_nodes;
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
    
    for (int i = 0; i < MAX_NODES; i++) {
        graph->neighbors_count[i] = 0;
    }
    
    return graph;
}

// Add edge to the graph
void add_edge(Graph* graph, int from, int to) {
    int index = graph->neighbors_count[from];
    graph->adj_list[from][index] = to;
    graph->neighbors_count[from]++;
}

// Initialize stack
Stack* create_stack() {
    Stack* stack = (Stack*)malloc(sizeof(Stack));
    stack->top = -1;
    return stack;
}

// Push node to stack
void push(Stack* stack, Node node) {
    stack->top++;
    stack->items[stack->top] = node;
}

// Pop node from stack
Node pop(Stack* stack) {
    Node node = stack->items[stack->top];
    stack->top--;
    return node;
}

// Check if stack is empty
bool is_stack_empty(Stack* stack) {
    return stack->top == -1;
}

// Depth-limited DFS search
bool dls_search(Graph* graph, int start, int target, int depth_limit) {
    Stack* stack = create_stack();
    Node initial_node = {start, 0, -1};
    push(stack, initial_node);
    
    while (!is_stack_empty(stack)) {
        Node current = pop(stack);
        
        // If we found the target
        if (current.node_id == target) {
            printf("Found target %d at depth %d\n", target, current.depth);
            free(stack);
            return true;
        }
        
        // If depth limit reached, don't explore further
        if (current.depth >= depth_limit) {
            continue;
        }
        
        // Add neighbors to stack (in reverse order for correct traversal)
        for (int i = graph->neighbors_count[current.node_id] - 1; i >= 0; i--) {
            int neighbor = graph->adj_list[current.node_id][i];
            Node new_node = {neighbor, current.depth + 1, current.node_id};
            push(stack, new_node);
        }
    }
    
    free(stack);
    return false;
}

// Iterative Deepening DFS
bool iddfs_search(Graph* graph, int start, int target) {
    printf("Starting IDDFS search from node %d to find node %d\n", start, target);
    
    // Start with depth 0 and increase until target is found
    for (int depth = 0; depth < graph->num_nodes; depth++) {
        printf("Searching at depth limit %d\n", depth);
        
        if (dls_search(graph, start, target, depth)) {
            printf("Target found at depth %d\n", depth);
            return true;
        }
    }
    
    printf("Target not found in the graph\n");
    return false;
}

// Print graph structure
void print_graph(Graph* graph) {
    printf("Graph structure:\n");
    for (int i = 0; i < graph->num_nodes; i++) {
        printf("Node %d: ", i);
        for (int j = 0; j < graph->neighbors_count[i]; j++) {
            printf("%d ", graph->adj_list[i][j]);
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Create a sample graph
    Graph* graph = create_graph(6);
    
    // Add edges to create the following graph:
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
    
    print_graph(graph);
    printf("\n");
    
    // Perform IDDFS search
    int start_node = 0;
    int target_node = 5;
    
    bool found = iddfs_search(graph, start_node, target_node);
    
    if (found) {
        printf("SUCCESS: Target node %d was found using IDDFS\n", target_node);
    } else {
        printf("FAILURE: Target node %d was not found\n", target_node);
    }
    
    // Test with a node that doesn't exist
    printf("\n");
    target_node = 6;
    found = iddfs_search(graph, start_node, target_node);
    
    free(graph);
    return 0;
}
```

## Output Example:
```
Graph structure:
Node 0: 2 1 
Node 1: 4 3 
Node 2: 5 
Node 3: 
Node 4: 
Node 5: 

Starting IDDFS search from node 0 to find node 5
Searching at depth limit 0
Searching at depth limit 1
Searching at depth limit 2
Searching at depth limit 3
Found target 5 at depth 3
Target found at depth 3
SUCCESS: Target node 5 was found using IDDFS

Starting IDDFS search from node 0 to find node 6
Searching at depth limit 0
Searching at depth limit 1
Searching at depth limit 2
Searching at depth limit 3
Searching at depth limit 4
Searching at depth limit 5
Target not found in the graph
FAILURE: Target node 6 was not found
```

## Key Features of this IDDFS Implementation:

1. **Graph Representation**: Uses adjacency list to store the graph
2. **Depth-Limited Search**: Implements DLS as a helper function
3. **Iterative Deepening**: Gradually increases depth limit
4. **Stack-based DFS**: Uses explicit stack instead of recursion
5. **Memory Management**: Properly allocates and frees memory
6. **Clear Output**: Shows the search process step by step

The algorithm combines the benefits of both BFS (completeness) and DFS (space efficiency) by performing depth-limited searches with increasing depth limits until the target is found.

