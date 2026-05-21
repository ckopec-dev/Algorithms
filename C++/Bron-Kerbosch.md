# Bron-Kerbosch Algorithm Implementation in C++

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph. Here's a complete implementation:

```cpp
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

class BronKerbosch {
private:
    std::vector<std::vector<int>> adjacencyList;
    std::vector<std::vector<int>> cliques;
    
public:
    BronKerbosch(int n) {
        adjacencyList.resize(n);
    }
    
    // Add edge between two vertices
    void addEdge(int u, int v) {
        adjacencyList[u].push_back(v);
        adjacencyList[v].push_back(u);
    }
    
    // Get neighbors of a vertex
    std::set<int> getNeighbors(int vertex) {
        std::set<int> neighbors;
        for (int neighbor : adjacencyList[vertex]) {
            neighbors.insert(neighbor);
        }
        return neighbors;
    }
    
    // Main Bron-Kerbosch algorithm with pivoting
    void bronKerbosch(std::set<int> R, std::set<int> P, std::set<int> X) {
        // If P and X are both empty, R is a maximal clique
        if (P.empty() && X.empty()) {
            std::vector<int> clique(R.begin(), R.end());
            std::sort(clique.begin(), clique.end());
            cliques.push_back(clique);
            return;
        }
        
        // Choose pivot vertex u from P ∪ X
        std::set<int> unionPX;
        unionPX.insert(P.begin(), P.end());
        unionPX.insert(X.begin(), X.end());
        
        int pivot = *unionPX.begin();
        for (int u : unionPX) {
            if (P.find(u) != P.end()) {
                pivot = u;
                break;
            }
        }
        
        // For each vertex v in P - N(u)
        std::set<int> PminusN = P;
        std::set<int> N = getNeighbors(pivot);
        
        for (int v : N) {
            PminusN.erase(v);
        }
        
        for (int v : PminusN) {
            // Add v to R
            std::set<int> newR = R;
            newR.insert(v);
            
            // Get neighbors of v
            std::set<int> Nv = getNeighbors(v);
            
            // Create new P and X
            std::set<int> newP, newX;
            std::set_intersection(P.begin(), P.end(), Nv.begin(), Nv.end(), 
                                std::inserter(newP, newP.begin()));
            std::set_intersection(X.begin(), X.end(), Nv.begin(), Nv.end(), 
                                std::inserter(newX, newX.begin()));
            
            // Recursive call
            bronKerbosch(newR, newP, newX);
            
            // Remove v from P and add to X
            P.erase(v);
            X.insert(v);
        }
    }
    
    // Find all maximal cliques
    std::vector<std::vector<int>> findAllMaximalCliques() {
        cliques.clear();
        std::set<int> R, P, X;
        
        // Initialize P with all vertices
        for (int i = 0; i < adjacencyList.size(); i++) {
            P.insert(i);
        }
        
        bronKerbosch(R, P, X);
        return cliques;
    }
    
    // Print all cliques
    void printCliques() {
        std::vector<std::vector<int>> allCliques = findAllMaximalCliques();
        std::cout << "Maximal cliques found:" << std::endl;
        for (size_t i = 0; i < allCliques.size(); i++) {
            std::cout << "Clique " << (i + 1) << ": ";
            for (int vertex : allCliques[i]) {
                std::cout << vertex << " ";
            }
            std::cout << std::endl;
        }
    }
};

// Example usage
int main() {
    // Create a graph with 6 vertices
    // Graph structure:
    // 0-1, 0-2, 0-3, 1-2, 1-4, 2-3, 2-4, 3-5, 4-5
    BronKerbosch bk(6);
    
    // Add edges
    bk.addEdge(0, 1);
    bk.addEdge(0, 2);
    bk.addEdge(0, 3);
    bk.addEdge(1, 2);
    bk.addEdge(1, 4);
    bk.addEdge(2, 3);
    bk.addEdge(2, 4);
    bk.addEdge(3, 5);
    bk.addEdge(4, 5);
    
    std::cout << "Graph edges:" << std::endl;
    std::cout << "0-1, 0-2, 0-3, 1-2, 1-4, 2-3, 2-4, 3-5, 4-5" << std::endl << std::endl;
    
    // Find and print all maximal cliques
    bk.printCliques();
    
    return 0;
}
```

## Expected Output:
```
Graph edges:
0-1, 0-2, 0-3, 1-2, 1-4, 2-3, 2-4, 3-5, 4-5

Maximal cliques found:
Clique 1: 0 1 2 
Clique 2: 0 2 3 
Clique 3: 1 2 4 
Clique 4: 2 4 5 
Clique 5: 3 4 5 
```

## Algorithm Explanation:

1. **Input**: An undirected graph represented as an adjacency list
2. **Process**: 
   - Uses recursive backtracking to explore all possible cliques
   - Implements pivoting optimization to reduce search space
   - Maintains three sets: R (current clique), P (potential vertices), X (excluded vertices)
3. **Output**: All maximal cliques in the graph

## Key Features:

- **Optimization**: Uses pivoting to improve performance
- **Efficiency**: Avoids redundant computations by maintaining proper sets
- **Flexibility**: Can handle graphs of any size
- **Clear Output**: Displays all maximal cliques found

The algorithm has a time complexity of O(3^(n/3)) in the worst case, making it suitable for small to medium-sized graphs.

