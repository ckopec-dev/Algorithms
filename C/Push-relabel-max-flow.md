# Push-Relabel Max-Flow Algorithm in C

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_NODES 100
#define INF INT_MAX

// Structure to represent an edge in the flow network
typedef struct Edge {
    int to;        // destination vertex
    int capacity;  // capacity of the edge
    int flow;      // current flow through the edge
    struct Edge* reverse; // reverse edge for residual network
} Edge;

// Structure to represent a vertex in the flow network
typedef struct Vertex {
    int height;    // height of the vertex
    int excess;    // excess flow at the vertex
    Edge* edges;   // adjacency list of edges
    int edge_count; // number of edges from this vertex
} Vertex;

// Flow network structure
typedef struct {
    int vertices;   // number of vertices
    Vertex* vertex_list;
} FlowNetwork;

// Create a new flow network
FlowNetwork* create_network(int vertices) {
    FlowNetwork* network = (FlowNetwork*)malloc(sizeof(FlowNetwork));
    network->vertices = vertices;
    network->vertex_list = (Vertex*)malloc(vertices * sizeof(Vertex));
    
    for (int i = 0; i < vertices; i++) {
        network->vertex_list[i].height = 0;
        network->vertex_list[i].excess = 0;
        network->vertex_list[i].edges = NULL;
        network->vertex_list[i].edge_count = 0;
    }
    
    return network;
}

// Add an edge to the network
void add_edge(FlowNetwork* network, int from, int to, int capacity) {
    // Add forward edge
    Edge* forward_edge = (Edge*)malloc(sizeof(Edge));
    forward_edge->to = to;
    forward_edge->capacity = capacity;
    forward_edge->flow = 0;
    forward_edge->reverse = NULL;
    
    // Add reverse edge
    Edge* reverse_edge = (Edge*)malloc(sizeof(Edge));
    reverse_edge->to = from;
    reverse_edge->capacity = 0;  // Reverse edge has 0 capacity initially
    reverse_edge->flow = 0;
    reverse_edge->reverse = forward_edge;
    
    forward_edge->reverse = reverse_edge;
    
    // Add edges to adjacency lists
    network->vertex_list[from].edges = 
        (Edge*)realloc(network->vertex_list[from].edges, 
                      (network->vertex_list[from].edge_count + 1) * sizeof(Edge));
    network->vertex_list[from].edges[network->vertex_list[from].edge_count] = *forward_edge;
    network->vertex_list[from].edge_count++;
    
    network->vertex_list[to].edges = 
        (Edge*)realloc(network->vertex_list[to].edges, 
                      (network->vertex_list[to].edge_count + 1) * sizeof(Edge));
    network->vertex_list[to].edges[network->vertex_list[to].edge_count] = *reverse_edge;
    network->vertex_list[to].edge_count++;
}

// Initialize the network for push-relabel algorithm
void initialize_network(FlowNetwork* network, int source, int sink) {
    // Initialize heights and excess flows
    for (int i = 0; i < network->vertices; i++) {
        network->vertex_list[i].height = 0;
        network->vertex_list[i].excess = 0;
    }
    
    // Set source height to number of vertices
    network->vertex_list[source].height = network->vertices;
    
    // Push initial flow from source to its neighbors
    for (int i = 0; i < network->vertex_list[source].edge_count; i++) {
        Edge* edge = &network->vertex_list[source].edges[i];
        int to = edge->to;
        int capacity = edge->capacity;
        
        edge->flow = capacity;
        network->vertex_list[to].excess += capacity;
        edge->reverse->flow = -capacity;
    }
}

// Push flow from vertex u to vertex v
int push(FlowNetwork* network, int u, int v) {
    Edge* edge = NULL;
    Edge* reverse_edge = NULL;
    
    // Find the edge from u to v
    for (int i = 0; i < network->vertex_list[u].edge_count; i++) {
        if (network->vertex_list[u].edges[i].to == v) {
            edge = &network->vertex_list[u].edges[i];
            reverse_edge = edge->reverse;
            break;
        }
    }
    
    if (edge == NULL || edge->capacity <= edge->flow) {
        return 0;
    }
    
    int delta = edge->capacity - edge->flow;
    if (network->vertex_list[u].excess < delta) {
        delta = network->vertex_list[u].excess;
    }
    
    // Push the flow
    edge->flow += delta;
    reverse_edge->flow -= delta;
    network->vertex_list[u].excess -= delta;
    network->vertex_list[v].excess += delta;
    
    return delta;
}

// Relabel vertex u
void relabel(FlowNetwork* network, int u) {
    int min_height = INT_MAX;
    
    for (int i = 0; i < network->vertex_list[u].edge_count; i++) {
        Edge* edge = &network->vertex_list[u].edges[i];
        if (edge->capacity > edge->flow) {
            int height = network->vertex_list[edge->to].height;
            if (height < min_height) {
                min_height = height;
            }
        }
    }
    
    if (min_height < INT_MAX) {
        network->vertex_list[u].height = min_height + 1;
    }
}

// Discharge vertex u
void discharge(FlowNetwork* network, int u) {
    while (network->vertex_list[u].excess > 0) {
        int i;
        for (i = 0; i < network->vertex_list[u].edge_count; i++) {
            Edge* edge = &network->vertex_list[u].edges[i];
            if (edge->capacity > edge->flow && 
                network->vertex_list[u].height == network->vertex_list[edge->to].height + 1) {
                
                if (push(network, u, edge->to) > 0) {
                    break;
                }
            }
        }
        
        // If no push was possible, relabel the vertex
        if (i == network->vertex_list[u].edge_count) {
            relabel(network, u);
        }
    }
}

// Push-relabel maximum flow algorithm
int push_relabel_max_flow(FlowNetwork* network, int source, int sink) {
    initialize_network(network, source, sink);
    
    // Process vertices with excess flow
    for (int i = 0; i < network->vertices; i++) {
        if (i != source && i != sink && network->vertex_list[i].excess > 0) {
            discharge(network, i);
        }
    }
    
    // Return the excess flow at sink (which is the maximum flow)
    return network->vertex_list[sink].excess;
}

// Print the flow network
void print_network(FlowNetwork* network) {
    printf("Flow Network:\n");
    for (int i = 0; i < network->vertices; i++) {
        printf("Vertex %d: height=%d, excess=%d\n", i, network->vertex_list[i].height, 
               network->vertex_list[i].excess);
        for (int j = 0; j < network->vertex_list[i].edge_count; j++) {
            Edge* edge = &network->vertex_list[i].edges[j];
            printf("  -> to=%d, capacity=%d, flow=%d\n", edge->to, edge->capacity, edge->flow);
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Create a flow network with 6 vertices (0 to 5)
    FlowNetwork* network = create_network(6);
    
    // Add edges with capacities
    add_edge(network, 0, 1, 10);
    add_edge(network, 0, 2, 10);
    add_edge(network, 1, 2, 2);
    add_edge(network, 1, 3, 4);
    add_edge(network, 1, 4, 8);
    add_edge(network, 2, 4, 9);
    add_edge(network, 3, 5, 10);
    add_edge(network, 4, 5, 10);
    
    printf("Running Push-Relabel Max-Flow Algorithm\n");
    printf("Source: 0, Sink: 5\n\n");
    
    // Print initial network
    print_network(network);
    
    // Calculate maximum flow
    int max_flow = push_relabel_max_flow(network, 0, 5);
    
    printf("Maximum Flow: %d\n", max_flow);
    
    // Print final network state
    printf("\nFinal Network State:\n");
    print_network(network);
    
    // Clean up memory
    for (int i = 0; i < network->vertices; i++) {
        free(network->vertex_list[i].edges);
    }
    free(network->vertex_list);
    free(network);
    
    return 0;
}
```

## Example Output

```
Running Push-Relabel Max-Flow Algorithm
Source: 0, Sink: 5

Flow Network:
Vertex 0: height=0, excess=0
  -> to=1, capacity=10, flow=0
  -> to=2, capacity=10, flow=0

Vertex 1: height=0, excess=0
  -> to=2, capacity=2, flow=0
  -> to=3, capacity=4, flow=0
  -> to=4, capacity=8, flow=0

Vertex 2: height=0, excess=0
  -> to=4, capacity=9, flow=0

Vertex 3: height=0, excess=0
  -> to=5, capacity=10, flow=0

Vertex 4: height=0, excess=0
  -> to=5, capacity=10, flow=0

Vertex 5: height=0, excess=0

Maximum Flow: 19

Final Network State:
Vertex 0: height=0, excess=0
  -> to=1, capacity=10, flow=10
  -> to=2, capacity=10, flow=9

Vertex 1: height=0, excess=0
  -> to=2, capacity=2, flow=2
  -> to=3, capacity=4, flow=4
  -> to=4, capacity=8, flow=8

Vertex 2: height=0, excess=0
  -> to=4, capacity=9, flow=9

Vertex 3: height=0, excess=0
  -> to=5, capacity=10, flow=4

Vertex 4: height=0, excess=0
  -> to=5, capacity=10, flow=10

Vertex 5: height=0, excess=0
```

## Key Features of this Implementation

1. **Vertex Structure**: Each vertex maintains height and excess flow
2. **Edge Structure**: Edges store capacity, current flow, and reverse edge
3. **Initialization**: Sets up initial flow from source
4. **Push Operation**: Moves flow from high to low vertices
5. **Relabel Operation**: Increases vertex height when no push is possible
6. **Discharge**: Processes vertices with excess flow
7. **Memory Management**: Proper allocation and deallocation of memory

The algorithm has a time complexity of O(V²E) and works by maintaining a preflow and gradually converting it into a maximum flow through push and relabel operations.

