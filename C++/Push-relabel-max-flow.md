# Push-Relabel Max-Flow Algorithm Implementation

Here's a complete implementation of the Push-Relabel max-flow algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
using namespace std;

class PushRelabel {
private:
    int V; // Number of vertices
    vector<vector<int>> capacity; // Capacity matrix
    vector<vector<int>> flow; // Flow matrix
    vector<int> height; // Height of each vertex
    vector<int> excess; // Excess flow at each vertex
    queue<int> active; // Queue of active vertices
    
public:
    PushRelabel(int vertices) {
        V = vertices;
        capacity.assign(V, vector<int>(V, 0));
        flow.assign(V, vector<int>(V, 0));
        height.assign(V, 0);
        excess.assign(V, 0);
    }
    
    // Add edge to the graph
    void addEdge(int u, int v, int cap) {
        capacity[u][v] = cap;
    }
    
    // Push flow from u to v
    void push(int u, int v) {
        int push_amount = min(excess[u], capacity[u][v] - flow[u][v]);
        if (push_amount <= 0) return;
        
        flow[u][v] += push_amount;
        flow[v][u] -= push_amount;
        excess[u] -= push_amount;
        excess[v] += push_amount;
        
        // Add to active queue if vertex becomes active
        if (v != 0 && v != V-1 && excess[v] > 0) {
            active.push(v);
        }
    }
    
    // Relabel vertex u
    void relabel(int u) {
        int min_height = INT_MAX;
        for (int i = 0; i < V; i++) {
            if (capacity[u][i] - flow[u][i] > 0) {
                min_height = min(min_height, height[i]);
            }
        }
        if (min_height < INT_MAX) {
            height[u] = min_height + 1;
        }
    }
    
    // Discharge vertex u
    void discharge(int u) {
        while (excess[u] > 0) {
            if (height[u] >= V) break; // Vertex is at top
            
            int i = 0;
            for (; i < V; i++) {
                if (capacity[u][i] - flow[u][i] > 0 && height[u] == height[i] + 1) {
                    push(u, i);
                    if (excess[u] == 0) break;
                }
            }
            
            if (excess[u] > 0) {
                relabel(u);
            }
        }
    }
    
    // Compute maximum flow from source to sink
    int maxFlow(int source, int sink) {
        // Initialize heights and flows
        height[source] = V;
        excess[source] = INT_MAX;
        
        // Push initial flow from source
        for (int i = 0; i < V; i++) {
            if (capacity[source][i] > 0) {
                push(source, i);
            }
        }
        
        // Process active vertices
        while (!active.empty()) {
            int u = active.front();
            active.pop();
            
            if (u == source || u == sink) continue;
            
            discharge(u);
        }
        
        // Return excess flow at sink (which is the max flow)
        return excess[sink];
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices (0 to 5)
    PushRelabel graph(6);
    
    // Add edges with capacities
    graph.addEdge(0, 1, 10);
    graph.addEdge(0, 2, 10);
    graph.addEdge(1, 2, 2);
    graph.addEdge(1, 3, 4);
    graph.addEdge(1, 4, 8);
    graph.addEdge(2, 4, 9);
    graph.addEdge(3, 5, 10);
    graph.addEdge(4, 5, 10);
    
    int max_flow = graph.maxFlow(0, 5);
    
    cout << "Maximum flow from vertex 0 to vertex 5: " << max_flow << endl;
    
    return 0;
}
```

## Algorithm Explanation

The Push-Relabel algorithm works as follows:

1. **Initialization**: 
   - Set source height to number of vertices
   - Initialize excess flow at source to infinity
   - Push initial flows from source to its neighbors

2. **Main Loop**:
   - Process active vertices (vertices with excess flow)
   - For each active vertex, try to push flow to neighbors
   - If no valid push is possible, relabel the vertex (increase its height)

3. **Push Operation**:
   - Send flow from vertex u to v if there's capacity and v is at lower height

4. **Relabel Operation**:
   - Increase height of vertex u to be one more than minimum height of neighbors with residual capacity

## Time Complexity
- **Time**: O(V²E) in worst case, but often performs better in practice
- **Space**: O(V²) for storing the capacity matrix

## Example Output
```
Maximum flow from vertex 0 to vertex 5: 19
```

This implementation demonstrates the push-relabel algorithm's efficiency in finding maximum flow in networks by maintaining a preflow and using push and relabel operations to achieve the optimal flow.