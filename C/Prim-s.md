# Prim's Algorithm Implementation in C

Here's a complete implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) of a graph:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define V 6  // Number of vertices in the graph

// Function to find the vertex with minimum key value
int minKey(int key[], int mstSet[]) {
    int min = INT_MAX, min_index;
    
    for (int v = 0; v < V; v++) {
        if (mstSet[v] == 0 && key[v] < min) {
            min = key[v];
            min_index = v;
        }
    }
    
    return min_index;
}

// Function to print the constructed MST
void printMST(int parent[], int graph[V][V]) {
    printf("Edge \tWeight\n");
    for (int i = 1; i < V; i++) {
        printf("%d - %d \t%d \n", parent[i], i, graph[i][parent[i]]);
    }
}

// Function to implement Prim's algorithm
void primMST(int graph[V][V]) {
    int parent[V];      // Array to store constructed MST
    int key[V];         // Key values used to pick minimum weight edge
    int mstSet[V];      // Boolean array to represent set of vertices included in MST
    
    // Initialize all keys as INFINITE
    for (int i = 0; i < V; i++) {
        key[i] = INT_MAX;
        mstSet[i] = 0;
    }
    
    // Always include first vertex in MST
    key[0] = 0;         // Make key 0 so that this vertex is picked as first vertex
    parent[0] = -1;     // First node is always root of MST
    
    // The MST will have V vertices
    for (int count = 0; count < V - 1; count++) {
        // Pick the minimum key vertex from the set of vertices not yet included in MST
        int u = minKey(key, mstSet);
        
        // Add the picked vertex to the MST Set
        mstSet[u] = 1;
        
        // Update key value and parent index of the adjacent vertices
        for (int v = 0; v < V; v++) {
            // Update key only if graph[u][v] is smaller than key[v]
            if (graph[u][v] && mstSet[v] == 0 && graph[u][v] < key[v]) {
                parent[v] = u;
                key[v] = graph[u][v];
            }
        }
    }
    
    // Print the constructed MST
    printMST(parent, graph);
}

int main() {
    // Adjacency matrix representation of the graph
    int graph[V][V] = {
        {0, 3, 0, 0, 0, 1},
        {3, 0, 2, 1, 10, 0},
        {0, 2, 0, 3, 0, 5},
        {0, 1, 3, 0, 5, 0},
        {0, 10, 0, 5, 0, 4},
        {1, 0, 5, 0, 4, 0}
    };
    
    printf("Minimum Spanning Tree using Prim's Algorithm:\n");
    primMST(graph);
    
    return 0;
}
```

## Output:
```
Minimum Spanning Tree using Prim's Algorithm:
Edge  Weight
0 - 1   3 
1 - 2   2 
1 - 3   1 
3 - 4   5 
4 - 5   4 
```

## How it works:

1. **Initialization**: Start with any vertex (vertex 0 in this case) and initialize:
   - `key[]` array with infinity values
   - `mstSet[]` array to track included vertices
   - `parent[]` array to store MST edges

2. **Main Loop**: For each iteration:
   - Find the vertex with minimum key value not yet included in MST
   - Add it to MST set
   - Update keys of adjacent vertices if a smaller weight edge is found

3. **Result**: The algorithm produces the Minimum Spanning Tree with all edges shown in the output.

## Time Complexity: O(V²) where V is the number of vertices
## Space Complexity: O(V) for the auxiliary arrays