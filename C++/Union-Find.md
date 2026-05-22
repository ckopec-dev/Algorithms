# Union-Find Algorithm in C++

Here's a complete implementation of the Union-Find (Disjoint Set Union) algorithm with path compression and union by rank optimizations:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class UnionFind {
private:
    vector<int> parent;
    vector<int> rank;
    int components;

public:
    // Constructor to initialize Union-Find structure
    UnionFind(int n) {
        parent.resize(n);
        rank.resize(n, 0);
        components = n;
        
        // Initially, each element is its own parent
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }
    
    // Find with path compression
    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]);  // Path compression
        }
        return parent[x];
    }
    
    // Union with union by rank
    void unite(int x, int y) {
        int rootX = find(x);
        int rootY = find(y);
        
        if (rootX != rootY) {
            // Union by rank
            if (rank[rootX] < rank[rootY]) {
                parent[rootX] = rootY;
            } else if (rank[rootX] > rank[rootY]) {
                parent[rootY] = rootX;
            } else {
                parent[rootY] = rootX;
                rank[rootX]++;
            }
            components--;
        }
    }
    
    // Check if two elements are in the same set
    bool connected(int x, int y) {
        return find(x) == find(y);
    }
    
    // Get number of connected components
    int getComponents() {
        return components;
    }
};

// Example usage
int main() {
    // Create a Union-Find structure for 6 elements (0-5)
    UnionFind uf(6);
    
    cout << "Initial components: " << uf.getComponents() << endl;
    
    // Perform some unions
    uf.unite(0, 1);
    cout << "After uniting 0 and 1: " << uf.getComponents() << " components" << endl;
    
    uf.unite(2, 3);
    cout << "After uniting 2 and 3: " << uf.getComponents() << " components" << endl;
    
    uf.unite(1, 3);
    cout << "After uniting 1 and 3: " << uf.getComponents() << " components" << endl;
    
    // Check connections
    cout << "Are 0 and 2 connected? " << (uf.connected(0, 2) ? "Yes" : "No") << endl;
    cout << "Are 0 and 4 connected? " << (uf.connected(0, 4) ? "Yes" : "No") << endl;
    
    // More unions
    uf.unite(4, 5);
    cout << "After uniting 4 and 5: " << uf.getComponents() << " components" << endl;
    
    uf.unite(3, 5);
    cout << "After uniting 3 and 5: " << uf.getComponents() << " components" << endl;
    
    cout << "Are 0 and 5 connected? " << (uf.connected(0, 5) ? "Yes" : "No") << endl;
    
    return 0;
}
```

## Output:
```
Initial components: 6
After uniting 0 and 1: 5 components
After uniting 2 and 3: 4 components
After uniting 1 and 3: 3 components
Are 0 and 2 connected? Yes
Are 0 and 4 connected? No
After uniting 4 and 5: 2 components
After uniting 3 and 5: 1 components
Are 0 and 5 connected? Yes
```

## Key Features:

1. **Path Compression**: In the `find` operation, we make every node point directly to the root
2. **Union by Rank**: We always attach the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `find` and `unite` operations have nearly O(1) amortized time complexity

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

This implementation is commonly used in problems like:
- Finding connected components in graphs
- Kruskal's algorithm for minimum spanning tree
- Detecting cycles in graphs
- Dynamic connectivity problems

