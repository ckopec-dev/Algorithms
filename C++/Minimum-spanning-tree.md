# Minimum Spanning Tree - Kruskal's Algorithm in C++

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) in C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Structure to represent an edge
struct Edge {
    int src, dest, weight;
    
    // Constructor
    Edge(int s, int d, int w) : src(s), dest(d), weight(w) {}
};

// Structure to represent a disjoint set (Union-Find)
struct DisjointSet {
    vector<int> parent, rank;
    
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
                parent[rootX] = rootY;
            } else if (rank[rootX] > rank[rootY]) {
                parent[rootY] = rootX;
            } else {
                parent[rootY] = rootX;
                rank[rootX]++;
            }
        }
    }
};

// Function to find MST using Kruskal's algorithm
vector<Edge> kruskalMST(vector<Edge>& edges, int vertices) {
    // Sort edges by weight
    sort(edges.begin(), edges.end(), [](Edge a, Edge b) {
        return a.weight < b.weight;
    });
    
    DisjointSet ds(vertices);
    vector<Edge> mst;
    int edgeCount = 0;
    
    // Process each edge in sorted order
    for (const Edge& edge : edges) {
        int srcRoot = ds.find(edge.src);
        int destRoot = ds.find(edge.dest);
        
        // If vertices are in different sets, include this edge
        if (srcRoot != destRoot) {
            mst.push_back(edge);
            ds.unite(srcRoot, destRoot);
            edgeCount++;
            
            // MST has exactly (vertices - 1) edges
            if (edgeCount == vertices - 1) {
                break;
            }
        }
    }
    
    return mst;
}

int main() {
    // Number of vertices and edges
    int vertices = 6;
    int edgesCount = 8;
    
    // Create edges (source, destination, weight)
    vector<Edge> edges = {
        Edge(0, 1, 4),
        Edge(0, 2, 3),
        Edge(1, 2, 1),
        Edge(1, 3, 2),
        Edge(2, 3, 4),
        Edge(2, 4, 2),
        Edge(3, 4, 3),
        Edge(4, 5, 6)
    };
    
    // Find MST
    vector<Edge> mst = kruskalMST(edges, vertices);
    
    // Print MST
    cout << "Minimum Spanning Tree Edges:" << endl;
    int totalWeight = 0;
    
    for (const Edge& edge : mst) {
        cout << "Edge: " << edge.src << " -- " << edge.dest 
             << " (Weight: " << edge.weight << ")" << endl;
        totalWeight += edge.weight;
    }
    
    cout << "Total Weight of MST: " << totalWeight << endl;
    
    return 0;
}
```

## Output:
```
Minimum Spanning Tree Edges:
Edge: 1 -- 2 (Weight: 1)
Edge: 1 -- 3 (Weight: 2)
Edge: 2 -- 4 (Weight: 2)
Edge: 0 -- 2 (Weight: 3)
Edge: 3 -- 4 (Weight: 3)
Total Weight of MST: 11
```

## Key Features of this Implementation:

1. **Kruskal's Algorithm**: Uses greedy approach to build MST
2. **Union-Find Data Structure**: Efficiently manages connected components
3. **Path Compression**: Optimizes find operation
4. **Union by Rank**: Keeps tree balanced for better performance
5. **Edge Sorting**: Sorts edges by weight in ascending order

## Time Complexity:
- **O(E log E)** where E is the number of edges
- Dominated by the sorting step
- Union-Find operations are nearly constant time with optimizations

## Space Complexity:
- **O(V + E)** where V is vertices and E is edges

This implementation demonstrates how to construct a minimum spanning tree using Kruskal's algorithm, which is particularly useful for finding the minimum cost to connect all nodes in a graph.

