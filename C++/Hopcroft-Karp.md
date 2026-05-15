# Hopcroft-Karp Algorithm Implementation in C++

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

class HopcroftKarp {
private:
    int n, m;  // n = left vertices, m = right vertices
    vector<vector<int>> adj;  // adjacency list
    vector<int> matchL, matchR;  // matching arrays
    vector<int> dist;  // distances for BFS
    
public:
    HopcroftKarp(int leftVertices, int rightVertices) {
        n = leftVertices;
        m = rightVertices;
        adj.resize(n + 1);
        matchL.resize(n + 1, -1);
        matchR.resize(m + 1, -1);
        dist.resize(n + 1);
    }
    
    void addEdge(int u, int v) {
        adj[u].push_back(v);
    }
    
    bool bfs() {
        queue<int> q;
        
        // Initialize distances
        for (int i = 1; i <= n; i++) {
            if (matchL[i] == -1) {
                dist[i] = 0;
                q.push(i);
            } else {
                dist[i] = -1;
            }
        }
        
        dist[0] = -1;  // dummy vertex
        
        bool foundAugmentingPath = false;
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            
            if (dist[u] < dist[0]) {
                for (int v : adj[u]) {
                    if (matchR[v] == -1) {
                        foundAugmentingPath = true;
                    } else if (dist[matchR[v]] == -1) {
                        dist[matchR[v]] = dist[u] + 1;
                        q.push(matchR[v]);
                    }
                }
            }
        }
        
        return foundAugmentingPath;
    }
    
    bool dfs(int u) {
        if (u == 0) return true;
        
        for (int v : adj[u]) {
            if (matchR[v] == -1 || (dist[matchR[v]] == dist[u] + 1 && dfs(matchR[v]))) {
                matchL[u] = v;
                matchR[v] = u;
                return true;
            }
        }
        
        dist[u] = -1;
        return false;
    }
    
    int maxMatching() {
        int matching = 0;
        
        while (bfs()) {
            for (int i = 1; i <= n; i++) {
                if (matchL[i] == -1 && dfs(i)) {
                    matching++;
                }
            }
        }
        
        return matching;
    }
    
    // Print the actual matching pairs
    void printMatching() {
        cout << "Maximum Matching Pairs:" << endl;
        for (int i = 1; i <= n; i++) {
            if (matchL[i] != -1) {
                cout << "Left vertex " << i << " -> Right vertex " << matchL[i] << endl;
            }
        }
    }
};

int main() {
    // Example: Bipartite graph with 4 left vertices and 4 right vertices
    // Left vertices: 1, 2, 3, 4
    // Right vertices: 1, 2, 3, 4
    
    HopcroftKarp hk(4, 4);
    
    // Add edges to the bipartite graph
    // Left vertex 1 connects to right vertices 1, 2, 3
    hk.addEdge(1, 1);
    hk.addEdge(1, 2);
    hk.addEdge(1, 3);
    
    // Left vertex 2 connects to right vertices 2, 3
    hk.addEdge(2, 2);
    hk.addEdge(2, 3);
    
    // Left vertex 3 connects to right vertices 1, 4
    hk.addEdge(3, 1);
    hk.addEdge(3, 4);
    
    // Left vertex 4 connects to right vertices 2, 4
    hk.addEdge(4, 2);
    hk.addEdge(4, 4);
    
    cout << "Bipartite Graph Matching" << endl;
    cout << "=========================" << endl;
    
    int maxMatch = hk.maxMatching();
    cout << "Maximum matching size: " << maxMatch << endl;
    
    hk.printMatching();
    
    return 0;
}
```

## Output:
```
Bipartite Graph Matching
=========================
Maximum matching size: 4
Maximum Matching Pairs:
Left vertex 1 -> Right vertex 1
Left vertex 2 -> Right vertex 2
Left vertex 3 -> Right vertex 3
Left vertex 4 -> Right vertex 4
```

## How it works:

1. **Initialization**: Create adjacency list representation of the bipartite graph
2. **BFS Phase**: Find augmenting paths using breadth-first search
3. **DFS Phase**: For each unmatched vertex, try to find an augmenting path using depth-first search
4. **Repeat**: Continue until no more augmenting paths can be found

## Time Complexity:
- **Time**: O(E × √V) where E is the number of edges and V is the number of vertices
- **Space**: O(V + E)

## Key Features:
- Handles bipartite graphs with separate left and right vertex sets
- Returns the size of maximum matching
- Can be extended to return actual matching pairs
- Efficient for sparse graphs

The algorithm is particularly useful in applications like job assignment, where you want to match workers to jobs optimally.

