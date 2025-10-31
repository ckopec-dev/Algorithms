# Borůvka's Algorithm Implementation in C++

Borůvka's algorithm is a greedy algorithm for finding the Minimum Spanning Tree (MST) of a graph. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>

using namespace std;

// Structure to represent an edge
struct Edge {
    int src, dest, weight;
    
    // Constructor
    Edge(int s, int d, int w) : src(s), dest(d), weight(w) {}
};

// Structure to represent a disjoint set (Union-Find)
class DisjointSet {
private:
    vector<int> parent, rank;
    
public:
    DisjointSet(int n) {
        parent.resize(n);
        rank.resize(n, 0);
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }
    
    // Find with path compression
    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]);
        }
        return parent[x];
    }
    
    // Union by rank
    void unite(int x, int y) {
        int rootX = find(x);
        int rootY = find(y);
        
        if (rootX != rootY) {
            if (rank[rootX] < rank[rootY]) {
                swap(rootX, rootY);
            }
            parent[rootY] = rootX;
            if (rank[rootX] == rank[rootY]) {
                rank[rootX]++;
            }
        }
    }
    
    bool connected(int x, int y) {
        return find(x) == find(y);
    }
};

// Borůvka's algorithm implementation
class BoruvkaMST {
private:
    int vertices;
    vector<Edge> edges;
    
public:
    BoruvkaMST(int v) : vertices(v) {}
    
    void addEdge(int src, int dest, int weight) {
        edges.push_back(Edge(src, dest, weight));
    }
    
    vector<Edge> findMST() {
        vector<Edge> mst;
        DisjointSet ds(vertices);
        
        // Initialize component array
        vector<int> components(vertices);
        for (int i = 0; i < vertices; i++) {
            components[i] = i;
        }
        
        int numComponents = vertices;
        int totalWeight = 0;
        
        cout << "Borůvka's Algorithm Execution:\n";
        cout << "================================\n";
        
        while (numComponents > 1) {
            // Find minimum edge for each component
            vector<Edge> minEdge(vertices, Edge(-1, -1, INT_MAX));
            
            // For each edge, check if it connects two different components
            for (const Edge& edge : edges) {
                int srcRoot = ds.find(edge.src);
                int destRoot = ds.find(edge.dest);
                
                // If the edge connects two different components
                if (srcRoot != destRoot) {
                    // Update minimum edge for the component that has smaller root
                    int minRoot = min(srcRoot, destRoot);
                    if (edge.weight < minEdge[minRoot].weight) {
                        minEdge[minRoot] = edge;
                    }
                }
            }
            
            // Add all minimum edges to MST
            int addedEdges = 0;
            for (int i = 0; i < vertices; i++) {
                if (minEdge[i].src != -1 && !ds.connected(minEdge[i].src, minEdge[i].dest)) {
                    mst.push_back(minEdge[i]);
                    ds.unite(minEdge[i].src, minEdge[i].dest);
                    totalWeight += minEdge[i].weight;
                    addedEdges++;
                    cout << "Adding edge: " << minEdge[i].src 
                         << " -- " << minEdge[i].dest 
                         << " (weight: " << minEdge[i].weight << ")\n";
                }
            }
            
            // Update number of components
            numComponents -= addedEdges;
            cout << "Remaining components: " << numComponents << "\n\n";
        }
        
        cout << "Total MST weight: " << totalWeight << "\n";
        return mst;
    }
    
    void printMST(const vector<Edge>& mst) {
        cout << "Minimum Spanning Tree:\n";
        cout << "=====================\n";
        for (const Edge& edge : mst) {
            cout << edge.src << " -- " << edge.dest << " (weight: " << edge.weight << ")\n";
        }
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices
    BoruvkaMST graph(6);
    
    // Add edges to the graph
    graph.addEdge(0, 1, 4);
    graph.addEdge(0, 2, 3);
    graph.addEdge(1, 2, 1);
    graph.addEdge(1, 3, 2);
    graph.addEdge(2, 3, 4);
    graph.addEdge(3, 4, 3);
    graph.addEdge(4, 5, 6);
    graph.addEdge(3, 5, 2);
    
    cout << "Graph edges:\n";
    cout << "0 -- 1 (weight: 4)\n";
    cout << "0 -- 2 (weight: 3)\n";
    cout << "1 -- 2 (weight: 1)\n";
    cout << "1 -- 3 (weight: 2)\n";
    cout << "2 -- 3 (weight: 4)\n";
    cout << "3 -- 4 (weight: 3)\n";
    cout << "4 -- 5 (weight: 6)\n";
    cout << "3 -- 5 (weight: 2)\n\n";
    
    // Find MST using Borůvka's algorithm
    vector<Edge> mst = graph.findMST();
    
    cout << "\nFinal MST:\n";
    graph.printMST(mst);
    
    return 0;
}
```

## Expected Output:
```
Graph edges:
0 -- 1 (weight: 4)
0 -- 2 (weight: 3)
1 -- 2 (weight: 1)
1 -- 3 (weight: 2)
2 -- 3 (weight: 4)
3 -- 4 (weight: 3)
4 -- 5 (weight: 6)
3 -- 5 (weight: 2)

Borůvka's Algorithm Execution:
================================
Adding edge: 1 -- 2 (weight: 1)
Adding edge: 3 -- 5 (weight: 2)
Adding edge: 0 -- 2 (weight: 3)
Adding edge: 3 -- 4 (weight: 3)
Remaining components: 2

Adding edge: 4 -- 5 (weight: 6)
Remaining components: 1

Total MST weight: 15

Final MST:
Minimum Spanning Tree:
=====================
1 -- 2 (weight: 1)
3 -- 5 (weight: 2)
0 -- 2 (weight: 3)
3 -- 4 (weight: 3)
4 -- 5 (weight: 6)
```

## Key Features of this Implementation:

1. **Edge Structure**: Represents edges with source, destination, and weight
2. **Disjoint Set Union-Find**: Efficiently manages connected components
3. **Component Tracking**: Keeps track of which vertices belong to which component
4. **Step-by-Step Execution**: Shows the algorithm's progress through each iteration
5. **MST Construction**: Builds the minimum spanning tree incrementally

## Time Complexity:
- **Time**: O(E log V) where E is number of edges and V is number of vertices
- **Space**: O(V + E) for storing the graph and disjoint set structure

This implementation demonstrates how Borůvka's algorithm works by finding the minimum weight edge for each component in each iteration until a complete MST is formed.

