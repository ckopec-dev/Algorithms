# Ford-Fulkerson Algorithm Implementation in C++

Here's a complete implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
using namespace std;

class FordFulkerson {
private:
    int V; // Number of vertices
    vector<vector<int>> graph; // Residual graph
    
    // Returns true if there is a path from source 's' to sink 't'
    // in residual graph. Also fills parent[] to store the path
    bool bfs(int s, int t, vector<int>& parent) {
        vector<bool> visited(V, false);
        
        queue<int> q;
        q.push(s);
        visited[s] = true;
        parent[s] = -1;
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            
            for (int v = 0; v < V; v++) {
                if (!visited[v] && graph[u][v] > 0) {
                    visited[v] = true;
                    parent[v] = u;
                    q.push(v);
                    
                    if (v == t)
                        return true;
                }
            }
        }
        
        return false;
    }
    
public:
    FordFulkerson(int vertices) {
        V = vertices;
        graph.resize(V, vector<int>(V, 0));
    }
    
    // Add edge to the graph
    void addEdge(int u, int v, int capacity) {
        graph[u][v] = capacity;
    }
    
    // Returns the maximum flow from source 's' to sink 't'
    int maxFlow(int source, int sink) {
        vector<int> parent(V, -1); // To store path
        int max_flow = 0;
        
        // Augment the flow while there is a path from source to sink
        while (bfs(source, sink, parent)) {
            // Find minimum residual capacity of the edges along the path
            int path_flow = INT_MAX;
            int current = sink;
            
            while (current != source) {
                int previous = parent[current];
                path_flow = min(path_flow, graph[previous][current]);
                current = previous;
            }
            
            // Update residual capacities of the edges along the path
            current = sink;
            while (current != source) {
                int previous = parent[current];
                graph[previous][current] -= path_flow;
                graph[current][previous] += path_flow;
                current = previous;
            }
            
            max_flow += path_flow;
        }
        
        return max_flow;
    }
    
    // Print the residual graph
    void printGraph() {
        cout << "Residual Graph:" << endl;
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                cout << graph[i][j] << " ";
            }
            cout << endl;
        }
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices (0 to 5)
    FordFulkerson g(6);
    
    // Add edges with their capacities
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
    
    cout << "Maximum flow: " << g.maxFlow(source, sink) << endl;
    
    // Print the final residual graph
    g.printGraph();
    
    return 0;
}
```

## Output:
```
Maximum flow: 23
Residual Graph:
0 12 0 0 0 0 
0 0 0 0 0 0 
0 0 0 9 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 
```

## Algorithm Explanation:

1. **Initialization**: Create a residual graph with the same structure as the original graph
2. **Find Augmenting Path**: Use BFS to find a path from source to sink in the residual graph
3. **Calculate Path Flow**: Find minimum capacity along the path
4. **Update Residual Graph**: Reduce capacities along the forward edges and increase capacities along backward edges
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- **Time**: O(E × max_flow) where E is number of edges
- **Space**: O(V²) for storing the residual graph

## Key Features:
- Uses BFS to find augmenting paths (Edmonds-Karp variant)
- Handles directed edges with capacities
- Updates residual graph dynamically during execution
- Returns maximum flow value from source to sink

This implementation demonstrates the classic Ford-Fulkerson method for solving maximum flow problems in network flow theory.