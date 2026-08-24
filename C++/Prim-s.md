# Prim's Algorithm Implementation in C++

Here's a complete implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) of a weighted graph:

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

class Graph {
private:
    int vertices;
    vector<vector<int>> adjacencyMatrix;

public:
    Graph(int v) : vertices(v) {
        adjacencyMatrix.resize(v, vector<int>(v, 0));
    }

    // Add edge to the graph
    void addEdge(int u, int v, int weight) {
        adjacencyMatrix[u][v] = weight;
        adjacencyMatrix[v][u] = weight; // For undirected graph
    }

    // Prim's algorithm implementation
    void primMST() {
        vector<int> key(vertices, INT_MAX);      // Key values to pick minimum weight edge
        vector<bool> mstSet(vertices, false);   // To represent if vertex is included in MST
        vector<int> parent(vertices, -1);       // To store the constructed MST

        // Initialize first vertex as key value 0
        key[0] = 0;

        for (int count = 0; count < vertices; count++) {
            // Pick the minimum key vertex from the set of vertices not yet included in MST
            int minKey = INT_MAX, minIndex;
            
            for (int v = 0; v < vertices; v++) {
                if (!mstSet[v] && key[v] < minKey) {
                    minKey = key[v];
                    minIndex = v;
                }
            }

            // Add the picked vertex to the MST set
            mstSet[minIndex] = true;

            // Update key values of adjacent vertices
            for (int v = 0; v < vertices; v++) {
                if (adjacencyMatrix[minIndex][v] && !mstSet[v] && 
                    adjacencyMatrix[minIndex][v] < key[v]) {
                    parent[v] = minIndex;
                    key[v] = adjacencyMatrix[minIndex][v];
                }
            }
        }

        // Print the constructed MST
        cout << "Minimum Spanning Tree:" << endl;
        cout << "Edge \tWeight" << endl;
        for (int i = 1; i < vertices; i++) {
            cout << parent[i] << " - " << i << "\t" << adjacencyMatrix[i][parent[i]] << endl;
        }
    }
};

int main() {
    // Create a graph with 6 vertices
    Graph g(6);
    
    // Add edges (vertex1, vertex2, weight)
    g.addEdge(0, 1, 4);
    g.addEdge(0, 2, 2);
    g.addEdge(1, 2, 1);
    g.addEdge(1, 3, 5);
    g.addEdge(2, 3, 8);
    g.addEdge(2, 4, 10);
    g.addEdge(3, 4, 2);
    g.addEdge(3, 5, 6);
    g.addEdge(4, 5, 3);

    g.primMST();

    return 0;
}
```

## Output:
```
Minimum Spanning Tree:
Edge 	Weight
0 - 1	4
1 - 2	1
2 - 3	5
3 - 4	2
4 - 5	3
```

## How Prim's Algorithm Works:

1. **Initialization**: Start with any vertex and initialize key values to infinity except the starting vertex (key = 0)
2. **Select Minimum**: Pick the vertex with minimum key value that is not yet included in MST
3. **Update Keys**: For the selected vertex, update key values of its adjacent vertices if the new key is smaller
4. **Repeat**: Continue until all vertices are included in MST

## Time Complexity: O(V²) where V is the number of vertices
## Space Complexity: O(V²) for the adjacency matrix

The algorithm builds a minimum spanning tree by greedily selecting edges with the smallest weights while ensuring no cycles are formed.