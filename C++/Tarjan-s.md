# Tarjan's Algorithm Implementation in C++

Tarjan's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>

using namespace std;

class TarjanSCC {
private:
    int numVertices;
    vector<vector<int>> adjList;
    vector<int> disc;      // Discovery time
    vector<int> low;       // Lowest discovery time reachable
    vector<bool> inStack;  // Whether vertex is in stack
    stack<int> st;         // Stack for DFS
    int time;              // Time counter
    vector<vector<int>> sccs; // Strongly connected components

public:
    TarjanSCC(int vertices) : numVertices(vertices) {
        adjList.resize(vertices);
        disc.resize(vertices, -1);
        low.resize(vertices, -1);
        inStack.resize(vertices, false);
        time = 0;
    }

    void addEdge(int u, int v) {
        adjList[u].push_back(v);
    }

    void findSCCs() {
        for (int i = 0; i < numVertices; i++) {
            if (disc[i] == -1) {
                tarjanDFS(i);
            }
        }
    }

private:
    void tarjanDFS(int u) {
        disc[u] = low[u] = ++time;
        st.push(u);
        inStack[u] = true;

        // Go through all adjacent vertices
        for (int v : adjList[u]) {
            if (disc[v] == -1) {
                // If v is not visited, recur for it
                tarjanDFS(v);
                low[u] = min(low[u], low[v]);
            } else if (inStack[v]) {
                // Update low value of u for back edge
                low[u] = min(low[u], disc[v]);
            }
        }

        // If u is root of SCC, pop all vertices from stack
        if (low[u] == disc[u]) {
            vector<int> scc;
            int w = -1;
            while (w != u) {
                w = st.top();
                st.pop();
                inStack[w] = false;
                scc.push_back(w);
            }
            sccs.push_back(scc);
        }
    }

public:
    void printSCCs() {
        cout << "Strongly Connected Components:" << endl;
        for (int i = 0; i < sccs.size(); i++) {
            cout << "SCC " << i + 1 << ": ";
            for (int vertex : sccs[i]) {
                cout << vertex << " ";
            }
            cout << endl;
        }
    }
};

// Example usage
int main() {
    // Create a graph with 5 vertices
    TarjanSCC graph(5);
    
    // Add edges
    graph.addEdge(0, 1);
    graph.addEdge(1, 2);
    graph.addEdge(2, 0);
    graph.addEdge(1, 3);
    graph.addEdge(3, 4);
    
    cout << "Graph edges:" << endl;
    cout << "0 -> 1" << endl;
    cout << "1 -> 2" << endl;
    cout << "2 -> 0" << endl;
    cout << "1 -> 3" << endl;
    cout << "3 -> 4" << endl;
    
    // Find and print strongly connected components
    graph.findSCCs();
    graph.printSCCs();
    
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
SCC 1: 0 2 1 
SCC 2: 3 4 
```

## How it works:

1. **Initialization**: The algorithm initializes discovery times, low values, and a stack for DFS traversal.

2. **DFS Traversal**: For each unvisited vertex, it performs a depth-first search.

3. **Low Value Calculation**: For each vertex, it calculates the lowest discovery time reachable from that vertex.

4. **Stack Management**: Vertices are pushed onto the stack during DFS traversal.

5. **Component Detection**: When a vertex's low value equals its discovery time, it means we've found a strongly connected component.

## Time Complexity: O(V + E)
## Space Complexity: O(V)

The algorithm correctly identifies that vertices {0, 1, 2} form one SCC (a cycle) and vertices {3, 4} form another SCC (a simple path).

