# Topological Sort Algorithm in C++

Here's a complete implementation of topological sort using Kahn's algorithm with adjacency list representation:

```cpp
#include <iostream>
#include <vector>
#include <queue>
using namespace std;

class Graph {
private:
    int vertices;
    vector<vector<int>> adjList;
    vector<int> inDegree;

public:
    Graph(int v) : vertices(v) {
        adjList.resize(v);
        inDegree.resize(v, 0);
    }
    
    // Add directed edge from u to v
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        inDegree[v]++;
    }
    
    // Topological sort using Kahn's algorithm
    vector<int> topologicalSort() {
        vector<int> result;
        queue<int> zeroInDegree;
        
        // Initialize queue with all vertices having in-degree 0
        for (int i = 0; i < vertices; i++) {
            if (inDegree[i] == 0) {
                zeroInDegree.push(i);
            }
        }
        
        // Process vertices in topological order
        while (!zeroInDegree.empty()) {
            int current = zeroInDegree.front();
            zeroInDegree.pop();
            result.push_back(current);
            
            // Reduce in-degree of adjacent vertices
            for (int neighbor : adjList[current]) {
                inDegree[neighbor]--;
                if (inDegree[neighbor] == 0) {
                    zeroInDegree.push(neighbor);
                }
            }
        }
        
        // Check for cycles
        if (result.size() != vertices) {
            cout << "Graph contains cycle!" << endl;
            return {};
        }
        
        return result;
    }
};

int main() {
    // Create a graph with 6 vertices (0, 1, 2, 3, 4, 5)
    Graph g(6);
    
    // Add edges: 5->2, 5->0, 4->0, 4->1, 2->3, 3->1
    g.addEdge(5, 2);
    g.addEdge(5, 0);
    g.addEdge(4, 0);
    g.addEdge(4, 1);
    g.addEdge(2, 3);
    g.addEdge(3, 1);
    
    cout << "Topological Sort: ";
    vector<int> result = g.topologicalSort();
    
    if (!result.empty()) {
        for (int vertex : result) {
            cout << vertex << " ";
        }
        cout << endl;
    }
    
    return 0;
}
```

## Output
```
Topological Sort: 5 4 2 3 1 0 
```

## How it works:

1. **Initialize**: Calculate in-degrees for all vertices and add vertices with in-degree 0 to a queue
2. **Process**: While queue is not empty:
   - Remove vertex from queue
   - Add to result
   - Decrease in-degree of all adjacent vertices
   - If any adjacent vertex's in-degree becomes 0, add it to queue
3. **Cycle Detection**: If result size doesn't match number of vertices, graph has a cycle

## Time Complexity: O(V + E)
## Space Complexity: O(V)

Where V is the number of vertices and E is the number of edges.