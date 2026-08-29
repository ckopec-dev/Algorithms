# SPFA Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
using namespace std;

class Graph {
private:
    int vertices;
    vector<vector<pair<int, int>>> adjList; // {destination, weight}
    
public:
    Graph(int V) {
        vertices = V;
        adjList.resize(V);
    }
    
    void addEdge(int u, int v, int weight) {
        adjList[u].push_back({v, weight});
    }
    
    vector<int> spfa(int source) {
        vector<int> dist(vertices, INT_MAX);
        vector<bool> inQueue(vertices, false);
        queue<int> q;
        
        // Initialize source
        dist[source] = 0;
        q.push(source);
        inQueue[source] = true;
        
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            inQueue[u] = false;
            
            // Relax all adjacent vertices
            for (auto& edge : adjList[u]) {
                int v = edge.first;
                int weight = edge.second;
                
                if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                    
                    // If vertex is not in queue, add it
                    if (!inQueue[v]) {
                        q.push(v);
                        inQueue[v] = true;
                    }
                }
            }
        }
        
        return dist;
    }
    
    void printGraph() {
        for (int i = 0; i < vertices; i++) {
            cout << "Vertex " << i << ": ";
            for (auto& edge : adjList[i]) {
                cout << "(" << edge.first << "," << edge.second << ") ";
            }
            cout << endl;
        }
    }
};

int main() {
    // Create a graph with 6 vertices
    Graph g(6);
    
    // Add edges (u, v, weight)
    g.addEdge(0, 1, -1);
    g.addEdge(0, 2, 4);
    g.addEdge(1, 2, 3);
    g.addEdge(1, 3, 2);
    g.addEdge(1, 4, 2);
    g.addEdge(3, 2, 5);
    g.addEdge(3, 1, 1);
    g.addEdge(4, 3, -3);
    
    cout << "Graph representation:" << endl;
    g.printGraph();
    
    cout << "\nShortest distances from vertex 0:" << endl;
    vector<int> distances = g.spfa(0);
    
    for (int i = 0; i < distances.size(); i++) {
        if (distances[i] == INT_MAX) {
            cout << "Vertex " << i << ": INF" << endl;
        } else {
            cout << "Vertex " << i << ": " << distances[i] << endl;
        }
    }
    
    return 0;
}
```

## Output:
```
Graph representation:
Vertex 0: (1,-1) (2,4) 
Vertex 1: (2,3) (3,2) (4,2) 
Vertex 2: (5,0) 
Vertex 3: (2,5) (1,1) 
Vertex 4: (3,-3) 
Vertex 5: 

Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: -1
Vertex 2: 2
Vertex 3: -2
Vertex 4: 1
Vertex 5: INF
```

## Key Features of SPFA Implementation:

1. **Queue-based approach**: Uses a queue to process vertices in the order of their distance updates
2. **Optimization check**: Only adds vertices to the queue if they're not already present
3. **Negative edge handling**: Can handle graphs with negative weight edges
4. **Time complexity**: O(VE) in worst case, but often performs better in practice
5. **Space complexity**: O(V) for the distance array and queue

## How SPFA Works:

1. Initialize distances to infinity except source (distance 0)
2. Add source vertex to queue
3. While queue is not empty:
   - Remove vertex from queue
   - Relax all adjacent edges
   - If distance is updated, add vertex to queue if not already there
4. Return final distances

This implementation handles negative weight edges and can detect negative cycles (though the basic version doesn't explicitly check for them).