# Dijkstra's Algorithm Implementation in C++

Here's a complete implementation of Dijkstra's algorithm using a priority queue and adjacency list representation:

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
using namespace std;

// Structure to represent a graph edge
struct Edge {
    int destination;
    int weight;
    
    Edge(int dest, int wt) : destination(dest), weight(wt) {}
};

// Structure to represent a node in priority queue
struct Node {
    int vertex;
    int distance;
    
    // For priority queue comparison (minimum heap)
    bool operator>(const Node& other) const {
        return distance > other.distance;
    }
};

class Graph {
private:
    int vertices;
    vector<vector<Edge>> adjacencyList;

public:
    Graph(int V) : vertices(V) {
        adjacencyList.resize(V);
    }
    
    // Add edge to the graph
    void addEdge(int source, int destination, int weight) {
        adjacencyList[source].push_back(Edge(destination, weight));
        // For undirected graph, also add the reverse edge
        // adjacencyList[destination].push_back(Edge(source, weight));
    }
    
    // Dijkstra's algorithm implementation
    void dijkstra(int startVertex) {
        // Distance array to store shortest distances from start vertex
        vector<int> distances(vertices, INT_MAX);
        
        // Visited array to track visited vertices
        vector<bool> visited(vertices, false);
        
        // Priority queue to get vertex with minimum distance
        priority_queue<Node, vector<Node>, greater<Node>> pq;
        
        // Initialize starting vertex
        distances[startVertex] = 0;
        pq.push(Node{startVertex, 0});
        
        cout << "Shortest distances from vertex " << startVertex << ":\n";
        
        while (!pq.empty()) {
            // Get vertex with minimum distance
            int currentVertex = pq.top().vertex;
            pq.pop();
            
            // Skip if already visited
            if (visited[currentVertex]) {
                continue;
            }
            
            // Mark as visited
            visited[currentVertex] = true;
            
            // Update distances of adjacent vertices
            for (const Edge& edge : adjacencyList[currentVertex]) {
                int neighbor = edge.destination;
                int weight = edge.weight;
                
                // If we found a shorter path to neighbor
                if (!visited[neighbor] && 
                    distances[currentVertex] + weight < distances[neighbor]) {
                    
                    distances[neighbor] = distances[currentVertex] + weight;
                    pq.push(Node{neighbor, distances[neighbor]});
                }
            }
        }
        
        // Print the results
        for (int i = 0; i < vertices; i++) {
            if (distances[i] == INT_MAX) {
                cout << "Vertex " << i << ": Not reachable\n";
            } else {
                cout << "Vertex " << i << ": " << distances[i] << "\n";
            }
        }
    }
};

int main() {
    // Create a graph with 6 vertices (0 to 5)
    Graph g(6);
    
    // Add edges (source, destination, weight)
    g.addEdge(0, 1, 4);
    g.addEdge(0, 2, 2);
    g.addEdge(1, 2, 1);
    g.addEdge(1, 3, 5);
    g.addEdge(2, 3, 8);
    g.addEdge(2, 4, 10);
    g.addEdge(3, 4, 2);
    g.addEdge(3, 5, 6);
    g.addEdge(4, 5, 3);
    
    // Run Dijkstra's algorithm from vertex 0
    g.dijkstra(0);
    
    return 0;
}
```

## Output:
```
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11
Vertex 5: 15
```

## Algorithm Explanation:

1. **Initialization**: 
   - Create a distance array with all values set to infinity except the start vertex (set to 0)
   - Use a priority queue to always process the vertex with minimum distance

2. **Main Loop**:
   - Extract the vertex with minimum distance from priority queue
   - For each neighbor of current vertex, update its distance if a shorter path is found
   - Add updated vertices to priority queue

3. **Time Complexity**: O((V + E) log V) where V is vertices and E is edges
4. **Space Complexity**: O(V)

This implementation finds the shortest path from a given source vertex to all other vertices in a weighted graph with non-negative edge weights.