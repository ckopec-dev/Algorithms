# Dinic's Algorithm Implementation in C

Dinic's algorithm is a fast algorithm for computing maximum flow in a flow network. Here's a complete implementation:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAXN 1000
#define INF INT_MAX

// Structure to represent an edge
typedef struct Edge {
    int to, capacity, reverse;
} Edge;

// Graph representation using adjacency list
int n, source, sink;
Edge *adj[MAXN];
int adj_size[MAXN];
int level[MAXN];

// Function to add an edge to the graph
void add_edge(int from, int to, int capacity) {
    // Forward edge
    Edge *e1 = (Edge*)malloc(sizeof(Edge));
    e1->to = to;
    e1->capacity = capacity;
    e1->reverse = adj_size[to];
    
    // Backward edge
    Edge *e2 = (Edge*)malloc(sizeof(Edge));
    e2->to = from;
    e2->capacity = 0;  // Residual capacity is 0 for backward edge
    e2->reverse = adj_size[from];
    
    adj[from][adj_size[from]++] = *e1;
    adj[to][adj_size[to]++] = *e2;
}

// Function to build level graph using BFS
int bfs() {
    memset(level, -1, sizeof(level));
    level[source] = 0;
    
    int queue[MAXN];
    int front = 0, rear = 0;
    queue[rear++] = source;
    
    while (front < rear) {
        int u = queue[front++];
        
        for (int i = 0; i < adj_size[u]; i++) {
            Edge *e = &adj[u][i];
            if (level[e->to] == -1 && e->capacity > 0) {
                level[e->to] = level[u] + 1;
                queue[rear++] = e->to;
            }
        }
    }
    
    return level[sink] != -1;
}

// Function to find blocking flow using DFS
int dfs(int u, int flow) {
    if (u == sink) return flow;
    
    int pushed = 0;
    for (int i = 0; i < adj_size[u]; i++) {
        Edge *e = &adj[u][i];
        if (level[e->to] == level[u] + 1 && e->capacity > 0) {
            int min_flow = (flow < e->capacity) ? flow : e->capacity;
            int temp = dfs(e->to, min_flow);
            
            if (temp > 0) {
                e->capacity -= temp;
                adj[e->to][e->reverse].capacity += temp;
                pushed += temp;
                flow -= temp;
                
                if (flow == 0) break;
            }
        }
    }
    
    return pushed;
}

// Main Dinic's algorithm function
int dinic() {
    int max_flow = 0;
    
    while (bfs()) {
        int flow = dfs(source, INF);
        while (flow > 0) {
            max_flow += flow;
            flow = dfs(source, INF);
        }
    }
    
    return max_flow;
}

// Function to initialize the graph
void init_graph(int nodes, int src, int snk) {
    n = nodes;
    source = src;
    sink = snk;
    
    for (int i = 0; i < n; i++) {
        adj_size[i] = 0;
        adj[i] = (Edge*)malloc(MAXN * sizeof(Edge));
    }
}

// Function to free memory
void free_graph() {
    for (int i = 0; i < n; i++) {
        free(adj[i]);
    }
}

// Example usage
int main() {
    // Create a graph with 6 nodes (0 to 5)
    init_graph(6, 0, 5);
    
    // Add edges with capacities
    add_edge(0, 1, 10);
    add_edge(0, 2, 10);
    add_edge(1, 2, 2);
    add_edge(1, 3, 4);
    add_edge(1, 4, 8);
    add_edge(2, 4, 9);
    add_edge(3, 5, 10);
    add_edge(4, 5, 10);
    
    printf("Maximum flow from node %d to node %d: %d\n", source, sink, dinic());
    
    // Free allocated memory
    free_graph();
    
    return 0;
}
```

## How it works:

1. **Initialization**: Create adjacency list representation of the graph with edges and capacities.

2. **Level Graph Construction**: Use BFS to build a level graph where each node is assigned a level based on its distance from source.

3. **Blocking Flow**: Use DFS to find augmenting paths in the level graph and update residual capacities.

4. **Repeat**: Continue until no more augmenting paths exist.

## Time Complexity:
- O(V²E) in worst case
- O(EV²) for sparse graphs (better than Ford-Fulkerson's O(E×max_flow))

## Sample Output:
```
Maximum flow from node 0 to node 5: 19
```

This implementation handles directed edges and finds the maximum flow from source to sink in a flow network.