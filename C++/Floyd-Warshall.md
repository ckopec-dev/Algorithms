# Floyd-Warshall Algorithm in C++

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

## Code Implementation

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

class FloydWarshall {
private:
    int V; // Number of vertices
    vector<vector<int>> dist; // Distance matrix
    
public:
    FloydWarshall(int vertices) {
        V = vertices;
        dist = vector<vector<int>>(V, vector<int>(V, INT_MAX));
    }
    
    // Add edge to the graph
    void addEdge(int u, int v, int weight) {
        dist[u][v] = weight;
    }
    
    // Floyd-Warshall algorithm
    void floydWarshall() {
        // Initialize diagonal elements to 0
        for (int i = 0; i < V; i++) {
            dist[i][i] = 0;
        }
        
        // Floyd-Warshall algorithm
        for (int k = 0; k < V; k++) {
            for (int i = 0; i < V; i++) {
                for (int j = 0; j < V; j++) {
                    if (dist[i][k] != INT_MAX && dist[k][j] != INT_MAX) {
                        if (dist[i][k] + dist[k][j] < dist[i][j]) {
                            dist[i][j] = dist[i][k] + dist[k][j];
                        }
                    }
                }
            }
        }
    }
    
    // Print the shortest distance matrix
    void printSolution() {
        cout << "Shortest distances between every pair of vertices:\n";
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                if (dist[i][j] == INT_MAX)
                    cout << "INF ";
                else
                    cout << dist[i][j] << " ";
            }
            cout << endl;
        }
    }
};

// Example usage
int main() {
    // Create a graph with 4 vertices
    FloydWarshall g(4);
    
    // Add edges with weights
    g.addEdge(0, 1, 5);
    g.addEdge(0, 3, 10);
    g.addEdge(1, 2, 3);
    g.addEdge(2, 3, 1);
    
    cout << "Original graph edges:\n";
    cout << "0 -> 1: weight 5\n";
    cout << "0 -> 3: weight 10\n";
    cout << "1 -> 2: weight 3\n";
    cout << "2 -> 3: weight 1\n";
    
    cout << "\nApplying Floyd-Warshall algorithm...\n";
    
    g.floydWarshall();
    
    g.printSolution();
    
    return 0;
}
```

## Output

```
Original graph edges:
0 -> 1: weight 5
0 -> 3: weight 10
1 -> 2: weight 3
2 -> 3: weight 1

Applying Floyd-Warshall algorithm...
Shortest distances between every pair of vertices:
0 5 8 9 
INF 0 3 4 
INF INF 0 1 
INF INF INF 0 
```

## How it works:

1. **Initialization**: Create a distance matrix where `dist[i][j]` represents the shortest distance from vertex `i` to vertex `j`
2. **Algorithm**: For each intermediate vertex `k`, check if going through `k` gives a shorter path between any two vertices `i` and `j`
3. **Update**: If a shorter path is found, update the distance matrix
4. **Result**: After all iterations, `dist[i][j]` contains the shortest distance from vertex `i` to vertex `j`

## Time Complexity: O(V³)
## Space Complexity: O(V²)

The algorithm works for both directed and undirected graphs and can detect negative cycles.

