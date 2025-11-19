# Minimum Spanning Tree - Kruskal's Algorithm in C

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) in C:

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

// Structure to represent a subset for Union-Find
struct Subset {
    int parent;
    int rank;
};

// Function to create a graph with V vertices and E edges
struct Graph* createGraph(int vertices, int edges) {
    struct Graph* graph = (struct Graph*)malloc(sizeof(struct Graph));
    graph->vertices = vertices;
    graph->edges = edges;
    graph->edge = (struct Edge*)malloc(edges * sizeof(struct Edge));
    return graph;
}

// Find operation for Union-Find
int find(struct Subset subsets[], int i) {
    // Find root and make root as parent of i (path compression)
    if (subsets[i].parent != i)
        subsets[i].parent = find(subsets, subsets[i].parent);
    return subsets[i].parent;
}

// Union operation for Union-Find
void Union(struct Subset subsets[], int x, int y) {
    int xroot = find(subsets, x);
    int yroot = find(subsets, y);
    
    // Attach smaller rank tree under root of high rank tree
    if (subsets[xroot].rank < subsets[yroot].rank)
        subsets[xroot].parent = yroot;
    else if (subsets[xroot].rank > subsets[yroot].rank)
        subsets[yroot].parent = xroot;
    else {
        // If ranks are same, then make one as root and increment its rank
        subsets[yroot].parent = xroot;
        subsets[xroot].rank++;
    }
}

// Compare function to sort edges by weight
int compare(const void* a, const void* b) {
    struct Edge* edge1 = (struct Edge*)a;
    struct Edge* edge2 = (struct Edge*)b;
    return edge1->weight > edge2->weight;
}

// Function to find MST using Kruskal's algorithm
void kruskalMST(struct Graph* graph) {
    int vertices = graph->vertices;
    struct Edge result[vertices]; // Store result MST
    int e = 0; // Index variable for result[]
    int i = 0; // Index variable for sorted edges
    
    // Step 1: Sort all edges in non-decreasing order of weight
    qsort(graph->edge, graph->edges, sizeof(graph->edge[0]), compare);
    
    // Create subsets for Union-Find
    struct Subset* subsets = (struct Subset*)malloc(vertices * sizeof(struct Subset));
    
    // Initialize subsets
    for (i = 0; i < vertices; i++) {
        subsets[i].parent = i;
        subsets[i].rank = 0;
    }
    
    // Number of edges to be taken is equal to vertices-1
    i = 0; // Index variable for sorted edges
    while (e < vertices - 1 && i < graph->edges) {
        // Step 2: Pick the smallest edge
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
    
    // Free memory
    free(subsets);
}

// Main function
int main() {
    // Create a graph with 4 vertices and 5 edges
    int vertices = 4;
    int edges = 5;
    
    struct Graph* graph = createGraph(vertices, edges);
    
    // Add edges
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
    
    // Find and print MST
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
```

## Explanation:

1. **Data Structures Used:**
   - `Edge`: Represents an edge with source, destination, and weight
   - `Graph`: Contains vertices, edges, and array of edges
   - `Subset`: For Union-Find operations with parent and rank

2. **Algorithm Steps:**
   - Sort all edges by weight in ascending order
   - Initialize Union-Find structure for each vertex
   - For each edge in sorted order:
     - If including the edge doesn't create a cycle, add it to MST
     - Otherwise, discard it
   - Continue until we have V-1 edges in MST

3. **Time Complexity:** O(E log E) where E is number of edges
4. **Space Complexity:** O(V + E) where V is number of vertices

This implementation uses the Union-Find data structure with path compression and union by rank optimizations for efficient cycle detection.

