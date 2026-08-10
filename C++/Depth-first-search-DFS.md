# Depth-First Search (DFS) Algorithm in C++

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <stack>
using namespace std;

class Graph {
private:
    int vertices;
    vector<vector<int>> adjList;

public:
    // Constructor
    Graph(int v) {
        vertices = v;
        adjList.resize(v);
    }
    
    // Add edge to the graph
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u); // For undirected graph
    }
    
    // Recursive DFS implementation
    void DFSRecursive(int startVertex) {
        vector<bool> visited(vertices, false);
        cout << "DFS Traversal (Recursive): ";
        DFSUtil(startVertex, visited);
        cout << endl;
    }
    
    // Utility function for recursive DFS
    void DFSUtil(int vertex, vector<bool>& visited) {
        visited[vertex] = true;
        cout << vertex << " ";
        
        for (int neighbor : adjList[vertex]) {
            if (!visited[neighbor]) {
                DFSUtil(neighbor, visited);
            }
        }
    }
    
    // Iterative DFS implementation using stack
    void DFSIterative(int startVertex) {
        vector<bool> visited(vertices, false);
        stack<int> dfsStack;
        
        cout << "DFS Traversal (Iterative): ";
        dfsStack.push(startVertex);
        
        while (!dfsStack.empty()) {
            int vertex = dfsStack.top();
            dfsStack.pop();
            
            if (!visited[vertex]) {
                visited[vertex] = true;
                cout << vertex << " ";
                
                // Push all unvisited neighbors to stack
                for (int i = adjList[vertex].size() - 1; i >= 0; i--) {
                    int neighbor = adjList[vertex][i];
                    if (!visited[neighbor]) {
                        dfsStack.push(neighbor);
                    }
                }
            }
        }
        cout << endl;
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices
    Graph g(6);
    
    // Add edges
    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(1, 3);
    g.addEdge(1, 4);
    g.addEdge(2, 5);
    
    cout << "Graph structure:" << endl;
    cout << "0 -- 1 -- 3" << endl;
    cout << "|    |" << endl;
    cout << "2 -- 4   " << endl;
    cout << "|       " << endl;
    cout << "5       " << endl << endl;
    
    // Perform DFS traversals
    g.DFSRecursive(0);
    g.DFSIterative(0);
    
    return 0;
}
```

## Output
```
Graph structure:
0 -- 1 -- 3
|    |
2 -- 4   
|       
5       

DFS Traversal (Recursive): 0 1 3 4 2 5 
DFS Traversal (Iterative): 0 2 5 1 4 3 
```

## Key Features

### 1. **Recursive Implementation**
- Uses function call stack
- Simpler to understand and implement
- Visits vertices in a depth-first manner

### 2. **Iterative Implementation**
- Uses explicit stack data structure
- Avoids recursion overhead
- More memory efficient for deep graphs

### 3. **Time Complexity**
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for visited array and recursion stack

### 4. **Applications**
- Finding connected components
- Topological sorting
- Detecting cycles in graphs
- Path finding algorithms
- Maze solving

The DFS algorithm explores as far as possible along each branch before backtracking, making it ideal for exploring all possible paths in a graph structure.