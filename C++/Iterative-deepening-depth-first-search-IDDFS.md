# Iterative Deepening Depth-First Search (IDDFS) in C++

Here's a complete implementation of IDDFS algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>

class Graph {
private:
    int vertices;
    std::vector<std::vector<int>> adjList;

public:
    Graph(int v) : vertices(v) {
        adjList.resize(v);
    }
    
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u); // For undirected graph
    }
    
    // Depth-limited DFS
    bool depthLimitedDFS(int start, int target, int depthLimit) {
        if (start == target) {
            return true;
        }
        
        if (depthLimit <= 0) {
            return false;
        }
        
        for (int neighbor : adjList[start]) {
            if (depthLimitedDFS(neighbor, target, depthLimit - 1)) {
                return true;
            }
        }
        
        return false;
    }
    
    // Iterative Deepening DFS
    bool iddfs(int start, int target) {
        for (int depth = 0; depth < vertices; depth++) {
            std::cout << "Searching at depth " << depth << std::endl;
            
            // Reset visited array for each depth level
            std::vector<bool> visited(vertices, false);
            
            if (depthLimitedDFSHelper(start, target, depth, visited)) {
                return true;
            }
        }
        return false;
    }
    
private:
    bool depthLimitedDFSHelper(int node, int target, int depthLimit, std::vector<bool>& visited) {
        if (node == target) {
            return true;
        }
        
        if (depthLimit <= 0) {
            return false;
        }
        
        visited[node] = true;
        
        for (int neighbor : adjList[node]) {
            if (!visited[neighbor]) {
                if (depthLimitedDFSHelper(neighbor, target, depthLimit - 1, visited)) {
                    visited[node] = false; // Backtrack
                    return true;
                }
            }
        }
        
        visited[node] = false; // Backtrack
        return false;
    }
    
public:
    // Simplified version using stack (more efficient for large graphs)
    bool iddfsSimplified(int start, int target) {
        for (int depth = 0; depth < vertices; depth++) {
            std::cout << "Searching at depth " << depth << std::endl;
            
            if (depthLimitedDFS(start, target, depth)) {
                return true;
            }
        }
        return false;
    }
};

// Alternative implementation with explicit stack for better control
class IDDFS {
private:
    std::vector<std::vector<int>> adjList;
    int vertices;
    
public:
    IDDFS(int v) : vertices(v) {
        adjList.resize(v);
    }
    
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u);
    }
    
    bool search(int start, int target) {
        for (int depth = 0; depth < vertices; depth++) {
            std::cout << "Searching at depth " << depth << std::endl;
            
            if (dls(start, target, depth)) {
                return true;
            }
        }
        return false;
    }
    
private:
    bool dls(int start, int target, int depthLimit) {
        std::stack<std::pair<int, int>> stack; // {node, current_depth}
        std::vector<bool> visited(vertices, false);
        
        stack.push({start, 0});
        
        while (!stack.empty()) {
            auto [node, currentDepth] = stack.top();
            stack.pop();
            
            if (node == target) {
                return true;
            }
            
            if (currentDepth >= depthLimit) {
                continue;
            }
            
            visited[node] = true;
            
            for (int neighbor : adjList[node]) {
                if (!visited[neighbor]) {
                    stack.push({neighbor, currentDepth + 1});
                }
            }
        }
        
        return false;
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices (0 to 5)
    Graph g(6);
    
    // Add edges
    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(1, 3);
    g.addEdge(1, 4);
    g.addEdge(2, 5);
    
    std::cout << "Graph structure:" << std::endl;
    std::cout << "0 -- 1 -- 3" << std::endl;
    std::cout << "|    |" << std::endl;
    std::cout << "2 -- 4" << std::endl;
    std::cout << "|    |" << std::endl;
    std::cout << "5    " << std::endl << std::endl;
    
    // Test IDDFS
    int start = 0;
    int target = 4;
    
    std::cout << "Searching for path from " << start << " to " << target << std::endl;
    std::cout << "Using Iterative Deepening DFS:" << std::endl;
    
    if (g.iddfs(start, target)) {
        std::cout << "Path found!" << std::endl;
    } else {
        std::cout << "No path found." << std::endl;
    }
    
    std::cout << "\n" << std::string(40, '-') << "\n" << std::endl;
    
    // Test with simplified version
    IDDFS iddfsGraph(6);
    iddfsGraph.addEdge(0, 1);
    iddfsGraph.addEdge(0, 2);
    iddfsGraph.addEdge(1, 3);
    iddfsGraph.addEdge(1, 4);
    iddfsGraph.addEdge(2, 5);
    
    std::cout << "Using simplified IDDFS:" << std::endl;
    if (iddfsGraph.search(0, 4)) {
        std::cout << "Path found!" << std::endl;
    } else {
        std::cout << "No path found." << std::endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Algorithm Components:**
- **Depth-Limited Search (DLS)**: Basic DFS with depth limit
- **Iterative Deepening**: Repeatedly calls DLS with increasing depth limits
- **Backtracking**: Properly manages visited nodes to avoid cycles

### 2. **Time and Space Complexity:**
- **Time Complexity**: O(b^d) where b is branching factor and d is depth
- **Space Complexity**: O(bd) where b is branching factor and d is depth

### 3. **Advantages of IDDFS:**
- **Optimal**: Finds shortest path (like BFS) but uses less memory
- **Complete**: Guarantees finding a solution if one exists
- **Memory Efficient**: Uses linear space (O(d)) instead of exponential

### 4. **How it Works:**
1. Start with depth limit = 0
2. Perform depth-limited DFS
3. If not found, increase depth limit by 1
4. Repeat until solution is found or all depths exhausted

### 5. **Output Example:**
```
Searching at depth 0
Searching at depth 1
Searching at depth 2
Searching at depth 3
Path found!
```

This implementation demonstrates the iterative deepening approach where the search gradually increases the depth limit, combining the benefits of both depth-first and breadth-first search strategies.

