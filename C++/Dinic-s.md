# Dinic's Algorithm Implementation in C++

Dinic's algorithm is a strongly polynomial algorithm for computing the maximum flow in a flow network. Here's a complete implementation:

```cpp
#include <bits/stdc++.h>
using namespace std;

const int INF = 1e9;
const int MAXN = 1005;

class Dinic {
private:
    vector<int> level, ptr;
    vector<vector<int>> adj;
    vector<vector<int>> capacity;
    int n, source, sink;

public:
    Dinic(int _n, int _source, int _sink) {
        n = _n;
        source = _source;
        sink = _sink;
        adj.resize(n);
        capacity.resize(n, vector<int>(n, 0));
        level.resize(n);
        ptr.resize(n);
    }

    void addEdge(int u, int v, int cap) {
        adj[u].push_back(v);
        adj[v].push_back(u);
        capacity[u][v] += cap;
        // For undirected graph, uncomment the next line:
        // capacity[v][u] += cap;
    }

    bool bfs() {
        fill(level.begin(), level.end(), -1);
        level[source] = 0;
        queue<int> q;
        q.push(source);

        while (!q.empty()) {
            int u = q.front();
            q.pop();

            for (int v : adj[u]) {
                if (level[v] == -1 && capacity[u][v] > 0) {
                    level[v] = level[u] + 1;
                    q.push(v);
                }
            }
        }

        return level[sink] != -1;
    }

    int dfs(int u, int flow) {
        if (u == sink) return flow;
        if (flow == 0) return 0;

        for (int &i = ptr[u]; i < adj[u].size(); i++) {
            int v = adj[u][i];
            if (level[v] == level[u] + 1 && capacity[u][v] > 0) {
                int pushed = dfs(v, min(flow, capacity[u][v]));
                if (pushed > 0) {
                    capacity[u][v] -= pushed;
                    capacity[v][u] += pushed;
                    return pushed;
                }
            }
        }

        return 0;
    }

    int maxFlow() {
        int flow = 0;
        while (bfs()) {
            fill(ptr.begin(), ptr.end(), 0);
            while (int pushed = dfs(source, INF)) {
                flow += pushed;
            }
        }
        return flow;
    }
};

// Example usage
int main() {
    // Create a graph with 6 nodes (0 to 5)
    // Source = 0, Sink = 5
    Dinic dinic(6, 0, 5);
    
    // Add edges with capacities
    dinic.addEdge(0, 1, 10);
    dinic.addEdge(0, 2, 10);
    dinic.addEdge(1, 2, 2);
    dinic.addEdge(1, 3, 4);
    dinic.addEdge(1, 4, 8);
    dinic.addEdge(2, 4, 9);
    dinic.addEdge(3, 5, 10);
    dinic.addEdge(4, 5, 10);
    
    cout << "Maximum flow: " << dinic.maxFlow() << endl;
    
    return 0;
}
```

## Explanation of Key Components:

### 1. **Data Structures:**
- `adj`: Adjacency list representation of the graph
- `capacity`: 2D matrix storing capacities of edges
- `level`: Level array for BFS
- `ptr`: Pointer array for DFS optimization

### 2. **Algorithm Steps:**

1. **BFS Phase**: Build level graph using BFS
2. **DFS Phase**: Find augmenting paths using DFS with current levels
3. **Repeat**: Continue until no more augmenting paths exist

### 3. **Time Complexity:**
- O(V²E) in worst case
- O(EV²) for general graphs
- Better performance on sparse graphs

### 4. **Example Graph:**
```
    0 ----10---- 1 ----4---- 3
    |           |           |
   10          2           10
    |           |           |
    2 ----9---- 4 ----10---- 5
```

The algorithm will find the maximum flow from source (0) to sink (5) in this network.

