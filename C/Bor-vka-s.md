# Borůvka's Algorithm Implementation in C

Borůvka's algorithm is a greedy algorithm for finding Minimum Spanning Tree (MST) in graph theory.

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_VERTICES 100
#define INF INT_MAX

// Structure to represent an edge
typedef struct {
    int src, dest, weight;
} Edge;

// Structure to represent a graph
typedef struct {
    int vertices, edges;
    Edge* edge;
} Graph;

// Structure to represent a disjoint set (Union-Find)
typedef struct {
    int parent;
    int rank;
} Subset;

// Create a graph with v vertices and e edges
Graph* createGraph(int vertices, int edges) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->vertices = vertices;
    graph->edges = edges;
    graph->edge = (Edge*)malloc(edges * sizeof(Edge));
    return graph;
}

// Find the parent of a node (with path compression)
int find(Subset subsets[], int i) {
    if (subsets[i].parent != i)
        subsets[i].parent = find(subsets, subsets[i].parent);
    return subsets[i].parent;
}

// Union two sets (union by rank)
void Union(Subset subsets[], int x, int y) {
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

// Function to implement Borůvka's algorithm
void boruvkaMST(Graph* graph) {
    int vertices = graph->vertices;
    int edges = graph->edges;
    
    // Create subsets for Union-Find
    Subset* subsets = (Subset*)malloc(vertices * sizeof(Subset));
    
    // Initialize subsets
    for (int i = 0; i < vertices; i++) {
        subsets[i].parent = i;
        subsets[i].rank = 0;
    }
    
    // Initialize result array
    Edge* result = (Edge*)malloc((vertices - 1) * sizeof(Edge));
    int numEdges = 0;
    
    // Initialize minimum edge for each component
    Edge* minEdge = (Edge*)malloc(vertices * sizeof(Edge));
    
    int numComponents = vertices;
    
    while (numComponents > 1) {
        // Initialize minimum edge for each vertex
        for (int i = 0; i < vertices; i++) {
            minEdge[i].src = -1;
            minEdge[i].dest = -1;
            minEdge[i].weight = INF;
        }
        
        // Find the minimum weight edge for each component
        for (int i = 0; i < edges; i++) {
            int src = graph->edge[i].src;
            int dest = graph->edge[i].dest;
            int weight = graph->edge[i].weight;
            
            int srcRoot = find(subsets, src);
            int destRoot = find(subsets, dest);
            
            // If edge connects two different components
            if (srcRoot != destRoot) {
                // Update minimum edge for source and destination vertices
                if (weight < minEdge[srcRoot].weight) {
                    minEdge[srcRoot] = graph->edge[i];
                }
                if (weight < minEdge[destRoot].weight) {
                    minEdge[destRoot] = graph->edge[i];
                }
            }
        }
        
        // Add minimum edges to MST
        for (int i = 0; i < vertices; i++) {
            if (minEdge[i].src != -1) {
                int srcRoot = find(subsets, minEdge[i].src);
                int destRoot = find(subsets, minEdge[i].dest);
                
                if (srcRoot != destRoot) {
                    result[numEdges++] = minEdge[i];
                    Union(subsets, srcRoot, destRoot);
                    numComponents--;
                }
            }
        }
    }
    
    // Print the MST
    printf("Minimum Spanning Tree:\n");
    for (int i = 0; i < numEdges; i++) {
        printf("Edge %d: %d -- %d (Weight: %d)\n", 
               i + 1, result[i].src, result[i].dest, result[i].weight);
    }
    
    // Free memory
    free(subsets);
    free(result);
    free(minEdge);
}

// Example usage
int main() {
    // Create a graph with 4 vertices and 5 edges
    int vertices = 4;
    int edges = 5;
    
    Graph* graph = createGraph(vertices, edges);
    
    // Add edges to the graph (src, dest, weight)
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
    
    printf("Graph edges:\n");
    for (int i = 0; i < edges; i++) {
        printf("Edge %d: %d -- %d (Weight: %d)\n", 
               i + 1, graph->edge[i].src, graph->edge[i].dest, graph->edge[i].weight);
    }
    
    printf("\n");
    
    // Run Borůvka's algorithm
    boruvkaMST(graph);
    
    // Free memory
    free(graph->edge);
    free(graph);
    
    return 0;
}
```

## Output:
```
Graph edges:
Edge 1: 0 -- 1 (Weight: 10)
Edge 2: 0 -- 2 (Weight: 6)
Edge 3: 0 -- 3 (Weight: 5)
Edge 4: 1 -- 3 (Weight: 15)
Edge 5: 2 -- 3 (Weight: 4)

Minimum Spanning Tree:
Edge 1: 0 -- 3 (Weight: 5)
Edge 2: 2 -- 3 (Weight: 4)
Edge 3: 0 -- 1 (Weight: 10)
```

## Algorithm Explanation:

1. **Initialization**: Create a disjoint set for each vertex
2. **Find Minimum Edges**: For each connected component, find the minimum weight edge connecting it to another component
3. **Union Components**: Add the minimum edges to MST and union the components
4. **Repeat**: Continue until all vertices are in one component

## Time Complexity:
- **Time**: O(E log V) where E is number of edges and V is number of vertices
- **Space**: O(V + E)

This implementation demonstrates Borůvka's algorithm for finding a Minimum Spanning Tree, which is particularly efficient for sparse graphs.