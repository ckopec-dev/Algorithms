# Bellman-Ford Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

// Structure to represent an edge
struct Edge {
    int src, dest, weight;
    Edge(int s, int d, int w) : src(s), dest(d), weight(w) {}
};

// Structure to represent a graph
struct Graph {
    int V, E;  // V = number of vertices, E = number of edges
    vector<Edge> edges;
    
    Graph(int vertices, int edges) : V(vertices), E(edges) {}
    
    void addEdge(int src, int dest, int weight) {
        edges.push_back(Edge(src, dest, weight));
    }
};

// Bellman-Ford algorithm implementation
vector<int> bellmanFord(Graph& graph, int source) {
    int V = graph.V;
    vector<int> dist(V, INT_MAX);  // Initialize distances to infinity
    dist[source] = 0;              // Distance from source to itself is 0
    
    // Relax all edges V-1 times
    for (int i = 0; i < V - 1; i++) {
        for (int j = 0; j < graph.E; j++) {
            int u = graph.edges[j].src;
            int v = graph.edges[j].dest;
            int weight = graph.edges[j].weight;
            
            // If we can find a shorter path, update the distance
            if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                dist[v] = dist[u] + weight;
            }
        }
    }
    
    // Check for negative weight cycles
    for (int i = 0; i < graph.E; i++) {
        int u = graph.edges[i].src;
        int v = graph.edges[i].dest;
        int weight = graph.edges[i].weight;
        
        if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
            cout << "Graph contains negative weight cycle!" << endl;
            return {};  // Return empty vector to indicate error
        }
    }
    
    return dist;
}

// Function to print the shortest distances
void printDistances(const vector<int>& dist, int source) {
    cout << "Vertex\tDistance from Source " << source << endl;
    for (int i = 0; i < dist.size(); i++) {
        if (dist[i] == INT_MAX)
            cout << i << "\tINF" << endl;
        else
            cout << i << "\t" << dist[i] << endl;
    }
}

// Example usage
int main() {
    // Create a graph with 5 vertices and 8 edges
    Graph graph(5, 8);
    
    // Add edges (source, destination, weight)
    graph.addEdge(0, 1, -1);
    graph.addEdge(0, 2, 4);
    graph.addEdge(1, 2, 3);
    graph.addEdge(1, 3, 2);
    graph.addEdge(1, 4, 2);
    graph.addEdge(3, 2, 5);
    graph.addEdge(3, 1, 1);
    graph.addEdge(4, 3, -3);
    
    cout << "Running Bellman-Ford algorithm..." << endl;
    
    // Find shortest paths from vertex 0
    vector<int> distances = bellmanFord(graph, 0);
    
    if (!distances.empty()) {
        printDistances(distances, 0);
    }
    
    return 0;
}
```

## Output:
```
Running Bellman-Ford algorithm...
Vertex  Distance from Source 0
0       0
1       -1
2       2
3       -2
4       1
```

## Key Features of this Implementation:

1. **Edge Structure**: Represents weighted edges with source, destination, and weight
2. **Graph Structure**: Contains vertices count, edges count, and edge list
3. **Main Algorithm**: 
   - Relax all edges V-1 times
   - Check for negative cycles
4. **Time Complexity**: O(VE) where V is vertices and E is edges
5. **Space Complexity**: O(V)
6. **Negative Cycle Detection**: Detects and reports if a negative weight cycle exists

## Advantages:
- Can handle negative edge weights
- Detects negative weight cycles
- Works on directed graphs
- Finds shortest paths from single source to all other vertices

## Disadvantages:
- Slower than Dijkstra's algorithm (O(VE) vs O(E + V log V))
- Not suitable for graphs with negative cycles when you need to find all shortest paths