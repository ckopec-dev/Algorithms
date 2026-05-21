# Edmonds-Karp Algorithm Implementation in C++

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
using namespace std;

class Graph {
private:
    int V; // Number of vertices
    vector<vector<int>> capacity; // Capacity matrix
    
public:
    Graph(int vertices) {
        V = vertices;
        capacity = vector<vector<int>>(V, vector<int>(V, 0));
    }
    
    // Add edge to the graph
    void addEdge(int u, int v, int cap) {
        capacity[u][v] = cap;
    }
    
    // BFS to find augmenting path
    bool bfs(int source, int sink, vector<int>& parent) {
        vector<bool> visited(V, false);
        queue<int> q;
        
        q.push(source);
        visited[source] = true;
        parent[source] = -1;
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            
            for (int v = 0; v < V; v++) {
                if (!visited[v] && capacity[u][v] > 0) {
                    visited[v] = true;
                    parent[v] = u;
                    q.push(v);
                    
                    if (v == sink) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    
    // Edmonds-Karp algorithm
    int maxFlow(int source, int sink) {
        int max_flow = 0;
        vector<int> parent(V, -1);
        
        // Augment the flow while there is a path from source to sink
        while (bfs(source, sink, parent)) {
            // Find minimum capacity along the path
            int path_flow = INT_MAX;
            int current = sink;
            
            while (current != source) {
                int previous = parent[current];
                path_flow = min(path_flow, capacity[previous][current]);
                current = previous;
            }
            
            // Update residual capacities
            current = sink;
            while (current != source) {
                int previous = parent[current];
                capacity[previous][current] -= path_flow;
                capacity[current][previous] += path_flow;
                current = previous;
            }
            
            max_flow += path_flow;
        }
        
        return max_flow;
    }
};

int main() {
    // Create a graph with 6 vertices (0 to 5)
    Graph g(6);
    
    // Add edges with capacities
    g.addEdge(0, 1, 16);
    g.addEdge(0, 2, 13);
    g.addEdge(1, 2, 10);
    g.addEdge(1, 3, 12);
    g.addEdge(2, 1, 4);
    g.addEdge(2, 4, 14);
    g.addEdge(3, 2, 9);
    g.addEdge(3, 5, 20);
    g.addEdge(4, 3, 7);
    g.addEdge(4, 5, 4);
    
    int source = 0;
    int sink = 5;
    
    cout << "Maximum flow from " << source << " to " << sink << " is: " 
         << g.maxFlow(source, sink) << endl;
    
    return 0;
}
```

## Example Output:
```
Maximum flow from 0 to 5 is: 23
```

## How it works:

1. **Initialization**: Create a capacity matrix representing the flow network
2. **BFS Search**: Use BFS to find an augmenting path from source to sink
3. **Path Augmentation**: Find the minimum capacity along the path and update residual capacities
4. **Repeat**: Continue until no more augmenting paths exist
5. **Return**: The sum of all flow increments is the maximum flow

## Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V²) for the capacity matrix

## Key Features:
- Uses BFS to find the shortest augmenting path
- Handles multiple paths from source to sink
- Updates residual graph after each augmentation
- Guarantees termination with integer capacities

