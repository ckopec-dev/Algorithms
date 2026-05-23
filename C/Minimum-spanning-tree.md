# Minimum Spanning Tree - Kruskal's Algorithm in C

Here's a complete implementation of Kruskal's algorithm for finding the Minimum Spanning Tree using Union-Find (Disjoint Set) data structure:

```c
#include <stdio.h>
#include <stdlib.h>

// Structure to represent an edge
struct Edge {
    int src, dest, weight;
};

// Structure to represent a graph
struct Graph {
    int vertices, edges;
    struct Edge* edge;
};

// Structure for Union-Find (Disjoint Set)
struct Subset {
    int parent;
    int rank;
};

// Function to create a graph
struct Graph* createGraph(int vertices, int edges) {
    struct Graph* graph = (struct Graph*)malloc(sizeof(struct Graph));
    graph->vertices = vertices;
    graph->edges = edges;
    graph->edge = (struct Edge*)malloc(edges * sizeof(struct Edge));
    return graph;
}

// Find operation with path compression
int find(struct Subset subsets[], int i) {
    if (subsets[i].parent != i)
        subsets[i].parent = find(subsets, subsets[i].parent);
    return subsets[i].parent;
}

// Union operation with union by rank
void Union(struct Subset subsets[], int x, int y) {
    int xroot = find(subsets, x);
    int yroot = find(subsets, y);
    
    if (subsets[xroot].rank < subsets[yroot].rank)
        subsets[xroot].parent = yroot;
    else if (subsets[xroot].rank > subsets[yroot].rank)
        subsets[yroot].parent = xroot;
    else {
        subsets[yroot].parent = xroot;
        subsets[xroot].rank++;
    }
}

// Compare function for sorting edges by weight
int compare(const void* a, const void* b) {
    struct Edge* edge1 = (struct Edge*)a;
    struct Edge* edge2 = (struct Edge*)b;
    return edge1->weight > edge2->weight;
}

// Function to find MST using Kruskal's algorithm
void kruskalMST(struct Graph* graph) {
    int vertices = graph->vertices;
    struct Edge result[vertices]; // Store result MST
    int e = 0; // Index for result array
    int i = 0; // Index for sorted edges
    
    // Sort all edges in non-decreasing order of weight
    qsort(graph->edge, graph->edges, sizeof(graph->edge[0]), compare);
    
    // Create subsets for Union-Find
    struct Subset* subsets = (struct Subset*)malloc(vertices * sizeof(struct Subset));
    
    // Initialize subsets
    for (i = 0; i < vertices; i++) {
        subsets[i].parent = i;
        subsets[i].rank = 0;
    }
    
    i = 0; // Index for sorted edges
    while (e < vertices - 1 && i < graph->edges) {
        struct Edge next_edge = graph->edge[i++];
        
        int x = find(subsets, next_edge.src);
        int y = find(subsets, next_edge.dest);
        
        // If including this edge doesn't cause cycle, include it
        if (x != y) {
            result[e++] = next_edge;
            Union(subsets, x, y);
        }
        // Else discard the edge
    }
    
    // Print the MST
    printf("Minimum Spanning Tree edges:\n");
    printf("Edge   Weight\n");
    for (i = 0; i < e; i++) {
        printf("%d - %d    %d\n", result[i].src, result[i].dest, result[i].weight);
    }
    
    // Calculate total weight
    int totalWeight = 0;
    for (i = 0; i < e; i++) {
        totalWeight += result[i].weight;
    }
    printf("Total Weight: %d\n", totalWeight);
    
    free(subsets);
}

// Main function
int main() {
    // Create a graph with 4 vertices and 5 edges
    int vertices = 4;
    int edges = 5;
    
    struct Graph* graph = createGraph(vertices, edges);
    
    // Add edges (source, destination, weight)
    graph->edge[0].src = 0;
    graph->edge[0].dest = 1;
    graph->edge[0].weight = 10;
    
    graph->edge[1].src = 0;
    graph->edge[1].dest = 2;
    graph->edge[1].weight = 6;
    
    graph->edge[2].src = 0;
    graph->edge[2].dest = 3;
    graph->edge[2].weight = 5;
    
    graph->edge[3].src = 1;
    graph->edge[3].dest = 3;
    graph->edge[3].weight = 15;
    
    graph->edge[4].src = 2;
    graph->edge[4].dest = 3;
    graph->edge[4].weight = 4;
    
    kruskalMST(graph);
    
    return 0;
}
```

## Output:
```
Minimum Spanning Tree edges:
Edge   Weight
2 - 3    4
0 - 3    5
0 - 1    10
Total Weight: 19
```

## How it works:

1. **Edge Sorting**: All edges are sorted by weight in ascending order
2. **Union-Find**: Uses disjoint set data structure to detect cycles
3. **Greedy Selection**: Pick the smallest edge that doesn't form a cycle
4. **Termination**: Continue until we have V-1 edges (where V is number of vertices)

## Time Complexity:
- **O(E log E)** where E is number of edges
- **O(E log V)** where V is number of vertices

## Space Complexity:
- **O(V + E)** for storing the graph and Union-Find structure

This implementation demonstrates Kruskal's algorithm, one of the most popular approaches for finding Minimum Spanning Trees.

