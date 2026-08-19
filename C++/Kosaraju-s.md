# Kosaraju's Algorithm Implementation in C++

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <stack>
using namespace std;

class Graph {
private:
    int vertices;
    vector<vector<int>> adjList;
    vector<vector<int>> reverseAdjList;

public:
    Graph(int v) : vertices(v) {
        adjList.resize(v);
        reverseAdjList.resize(v);
    }

    // Add edge to the graph
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        reverseAdjList[v].push_back(u);
    }

    // First DFS to get finish order
    void firstDFS(int vertex, vector<bool>& visited, stack<int>& finishStack) {
        visited[vertex] = true;
        
        for (int neighbor : adjList[vertex]) {
            if (!visited[neighbor]) {
                firstDFS(neighbor, visited, finishStack);
            }
        }
        
        finishStack.push(vertex);
    }

    // Second DFS on reversed graph
    void secondDFS(int vertex, vector<bool>& visited, vector<int>& component) {
        visited[vertex] = true;
        component.push_back(vertex);
        
        for (int neighbor : reverseAdjList[vertex]) {
            if (!visited[neighbor]) {
                secondDFS(neighbor, visited, component);
            }
        }
    }

    // Find all strongly connected components
    void findSCCs() {
        stack<int> finishStack;
        vector<bool> visited(vertices, false);
        
        // Step 1: Get finish order using first DFS
        for (int i = 0; i < vertices; i++) {
            if (!visited[i]) {
                firstDFS(i, visited, finishStack);
            }
        }
        
        // Step 2: Reset visited array for second DFS
        fill(visited.begin(), visited.end(), false);
        
        cout << "Strongly Connected Components:" << endl;
        
        // Step 3: Process vertices in reverse finish order
        while (!finishStack.empty()) {
            int vertex = finishStack.top();
            finishStack.pop();
            
            if (!visited[vertex]) {
                vector<int> component;
                secondDFS(vertex, visited, component);
                
                // Print the component
                for (int node : component) {
                    cout << node << " ";
                }
                cout << endl;
            }
        }
    }
};

// Example usage
int main() {
    // Create a graph with 5 vertices
    Graph g(5);
    
    // Add edges
    g.addEdge(0, 1);
    g.addEdge(1, 2);
    g.addEdge(2, 0);
    g.addEdge(1, 3);
    g.addEdge(3, 4);
    
    cout << "Graph edges:" << endl;
    cout << "0 -> 1" << endl;
    cout << "1 -> 2" << endl;
    cout << "2 -> 0" << endl;
    cout << "1 -> 3" << endl;
    cout << "3 -> 4" << endl;
    
    cout << "\n";
    
    // Find and print strongly connected components
    g.findSCCs();
    
    return 0;
}
```

## Output:
```
Graph edges:
0 -> 1
1 -> 2
2 -> 0
1 -> 3
3 -> 4

Strongly Connected Components:
0 2 1 
3 
4 
```

## Algorithm Explanation:

**Step 1: First DFS**
- Perform DFS on the original graph
- Keep track of finish times using a stack
- The vertex that finishes last is pushed first into the stack

**Step 2: Reverse Graph**
- Create a reversed version of the original graph

**Step 3: Second DFS**
- Process vertices in reverse order of finish times (from stack)
- For each unvisited vertex, perform DFS on reversed graph
- All vertices visited in one DFS call form a strongly connected component

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing adjacency lists and auxiliary data structures

## Key Points:
1. Uses two DFS traversals
2. First DFS determines the finish order
3. Second DFS on reversed graph finds actual SCCs
4. Efficient for finding all strongly connected components in a directed graph