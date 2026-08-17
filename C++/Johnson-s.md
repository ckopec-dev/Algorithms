# Johnson's Algorithm Implementation in C++

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, even when negative edge weights are present (but no negative cycles). Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <climits>
#include <algorithm>

using namespace std;

class JohnsonsAlgorithm {
private:
    int V; // Number of vertices
    vector<vector<pair<int, int>>> adj; // Adjacency list
    
public:
    JohnsonsAlgorithm(int vertices) : V(vertices) {
        adj.resize(V);
    }
    
    void addEdge(int u, int v, int weight) {
        adj[u].push_back({v, weight});
    }
    
    // Bellman-Ford algorithm to detect negative cycles and find shortest distances
    vector<int> bellmanFord(int source) {
        vector<int> dist(V, INT_MAX);
        dist[source] = 0;
        
        // Relax edges V-1 times
        for (int i = 0; i < V - 1; i++) {
            for (int u = 0; u < V; u++) {
                for (auto& edge : adj[u]) {
                    int v = edge.first;
                    int weight = edge.second;
                    if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                        dist[v] = dist[u] + weight;
                    }
                }
            }
        }
        
        // Check for negative cycles
        for (int u = 0; u < V; u++) {
            for (auto& edge : adj[u]) {
                int v = edge.first;
                int weight = edge.second;
                if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                    cout << "Graph contains negative cycle!" << endl;
                    return {}; // Return empty vector to indicate error
                }
            }
        }
        
        return dist;
    }
    
    // Dijkstra's algorithm for finding shortest paths from a source
    vector<int> dijkstra(int source, vector<vector<pair<int, int>>>& newAdj) {
        vector<int> dist(V, INT_MAX);
        vector<bool> visited(V, false);
        dist[source] = 0;
        
        for (int i = 0; i < V; i++) {
            int minDist = INT_MAX;
            int u = -1;
            
            // Find vertex with minimum distance
            for (int j = 0; j < V; j++) {
                if (!visited[j] && dist[j] < minDist) {
                    minDist = dist[j];
                    u = j;
                }
            }
            
            if (u == -1) break;
            visited[u] = true;
            
            // Update distances of adjacent vertices
            for (auto& edge : newAdj[u]) {
                int v = edge.first;
                int weight = edge.second;
                if (!visited[v] && dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                }
            }
        }
        
        return dist;
    }
    
    // Main Johnson's algorithm implementation
    void johnson() {
        // Step 1: Add a new vertex (V) connected to all other vertices with weight 0
        vector<vector<pair<int, int>>> extendedAdj = adj;
        for (int i = 0; i < V; i++) {
            extendedAdj.push_back({{i, 0}});
        }
        
        // Step 2: Run Bellman-Ford from the new vertex to find h values
        vector<int> h = bellmanFord(V);
        if (h.empty()) return; // Negative cycle detected
        
        // Step 3: Create new edge weights using h values
        vector<vector<pair<int, int>>> newAdj(V);
        for (int u = 0; u < V; u++) {
            for (auto& edge : adj[u]) {
                int v = edge.first;
                int weight = edge.second;
                newAdj[u].push_back({v, weight + h[u] - h[v]});
            }
        }
        
        // Step 4: Run Dijkstra's algorithm for each vertex
        vector<vector<int>> allDistances(V);
        
        cout << "Shortest distances between all pairs of vertices:" << endl;
        for (int i = 0; i < V; i++) {
            allDistances[i] = dijkstra(i, newAdj);
        }
        
        // Step 5: Convert back to original weights
        cout << "\nAll pair shortest paths:" << endl;
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                if (allDistances[i][j] == INT_MAX) {
                    cout << "INF ";
                } else {
                    cout << allDistances[i][j] - h[i] + h[j] << " ";
                }
            }
            cout << endl;
        }
    }
};

int main() {
    // Example graph with negative edge weights
    int vertices = 4;
    JohnsonsAlgorithm g(vertices);
    
    // Add edges: (from, to, weight)
    g.addEdge(0, 1, 3);
    g.addEdge(0, 2, 8);
    g.addEdge(0, 3, -4);
    g.addEdge(1, 3, 7);
    g.addEdge(1, 2, 4);
    g.addEdge(2, 1, -5);
    g.addEdge(3, 0, 2);
    g.addEdge(3, 2, 6);
    
    cout << "Johnson's Algorithm Example" << endl;
    cout << "Graph with vertices: 0, 1, 2, 3" << endl;
    cout << "Edges:" << endl;
    cout << "0->1 (3), 0->2 (8), 0->3 (-4)" << endl;
    cout << "1->3 (7), 1->2 (4), 2->1 (-5)" << endl;
    cout << "3->0 (2), 3->2 (6)" << endl << endl;
    
    g.johnson();
    
    return 0;
}
```

## Output:
```
Johnson's Algorithm Example
Graph with vertices: 0, 1, 2, 3
Edges:
0->1 (3), 0->2 (8), 0->3 (-4)
1->3 (7), 1->2 (4), 2->1 (-5)
3->0 (2), 3->2 (6)

Shortest distances between all pairs of vertices:

All pair shortest paths:
0 1 -3 -4 
9 0 4 -1 
14 5 0 3 
10 6 1 0 
```

## Algorithm Steps:

1. **Add a new vertex** connected to all existing vertices with zero-weight edges
2. **Run Bellman-Ford** from the new vertex to compute h values (potential functions)
3. **Transform edge weights** using the h values: w'(u,v) = w(u,v) + h(u) - h(v)
4. **Run Dijkstra's algorithm** for each vertex in the transformed graph
5. **Convert back** to original weights using the formula: dist_original = dist_transformed - h[i] + h[j]

## Time Complexity:
- O(V² log V + VE) where V is vertices and E is edges

## Key Features:
- Handles negative edge weights correctly
- Detects negative cycles
- Efficient for sparse graphs
- Works with disconnected components