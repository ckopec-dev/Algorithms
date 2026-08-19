# Kruskal's Algorithm Implementation in C++

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
                swap(rootX, rootY);
            }
            parent[rootY] = rootX;
            if (rank[rootX] == rank[rootY]) {
                rank[rootX]++;
            }
        }
    }
};

// Kruskal's algorithm implementation
vector<Edge> kruskalMST(vector<Edge>& edges, int vertices) {
    // Sort edges by weight
    sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b) {
        return a.weight < b.weight;
    });
    
    DisjointSet ds(vertices);
    vector<Edge> mst;
    int edgeCount = 0;
    
    // Process each edge in sorted order
    for (const Edge& edge : edges) {
        int rootSrc = ds.find(edge.src);
        int rootDest = ds.find(edge.dest);
        
        // If vertices are in different sets, include this edge
        if (rootSrc != rootDest) {
            mst.push_back(edge);
            ds.unite(rootSrc, rootDest);
            edgeCount++;
            
            // MST has exactly V-1 edges
            if (edgeCount == vertices - 1) {
                break;
            }
        }
    }
    
    return mst;
}

int main() {
    // Example graph with 4 vertices and 5 edges
    int vertices = 4;
    vector<Edge> edges = {
        Edge(0, 1, 10),  // Edge from 0 to 1 with weight 10
        Edge(0, 2, 6),   // Edge from 0 to 2 with weight 6
        Edge(0, 3, 5),   // Edge from 0 to 3 with weight 5
        Edge(1, 3, 15),  // Edge from 1 to 3 with weight 15
        Edge(2, 3, 4)    // Edge from 2 to 3 with weight 4
    };
    
    cout << "Original edges (sorted by weight):" << endl;
    for (const Edge& edge : edges) {
        cout << "Edge " << edge.src << " -- " << edge.dest 
             << " : weight = " << edge.weight << endl;
    }
    
    // Apply Kruskal's algorithm
    vector<Edge> mst = kruskalMST(edges, vertices);
    
    cout << "\nMinimum Spanning Tree edges:" << endl;
    int totalWeight = 0;
    for (const Edge& edge : mst) {
        cout << "Edge " << edge.src << " -- " << edge.dest 
             << " : weight = " << edge.weight << endl;
        totalWeight += edge.weight;
    }
    
    cout << "\nTotal weight of MST: " << totalWeight << endl;
    
    return 0;
}
```

## Output:
```
Original edges (sorted by weight):
Edge 0 -- 1 : weight = 10
Edge 0 -- 2 : weight = 6
Edge 0 -- 3 : weight = 5
Edge 1 -- 3 : weight = 15
Edge 2 -- 3 : weight = 4

Minimum Spanning Tree edges:
Edge 2 -- 3 : weight = 4
Edge 0 -- 3 : weight = 5
Edge 0 -- 1 : weight = 10

Total weight of MST: 19
```

## Algorithm Explanation:

1. **Sort Edges**: All edges are sorted by weight in ascending order
2. **Initialize DSU**: Create a disjoint set for each vertex
3. **Process Edges**: For each edge in sorted order:
   - Check if the vertices belong to different sets
   - If yes, include the edge in MST and unite the sets
   - Stop when we have V-1 edges
4. **Return MST**: The collected edges form the minimum spanning tree

## Time Complexity: O(E log E) where E is the number of edges
## Space Complexity: O(V + E) where V is the number of vertices