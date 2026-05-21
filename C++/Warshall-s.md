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
                        dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]);
                    }
                }
            }
        }
    }
    
    // Print the shortest distances
    void printSolution() {
        cout << "Shortest distances between all pairs of vertices:\n";
        for (int i = 0; i < V; i++) {
            for (int j = 0; j < V; j++) {
                if (dist[i][j] == INT_MAX)
                    cout << "INF ";
                else
                    cout << dist[i][j] << "   ";
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
    cout << "2 -> 3: weight 1\n\n";
    
    // Run Floyd-Warshall algorithm
    g.floydWarshall();
    
    // Print the result
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

Shortest distances between all pairs of vertices:
0   5   8   9   
INF   0   3   4   
INF   INF   0   1   
INF   INF   INF   0   
```

## How it works:

1. **Initialization**: Create a distance matrix where `dist[i][j]` represents the weight of edge from vertex `i` to vertex `j`
2. **Diagonal elements**: Set `dist[i][i] = 0` for all vertices
3. **Triple nested loop**: For each intermediate vertex `k`, update all pairs `(i,j)` if a shorter path exists through `k`
4. **Final result**: The matrix contains shortest distances between all pairs of vertices

## Time Complexity: O(V³)
## Space Complexity: O(V²)

The algorithm is particularly useful for dense graphs and when you need to find shortest paths between all pairs of vertices efficiently.

