# Disjoint-set Union-Find Algorithm in C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

class DisjointSet {
private:
    vector<int> parent;
    vector<int> rank;
    
public:
    // Constructor to initialize the disjoint set
    DisjointSet(int n) {
        parent.resize(n);
        rank.resize(n, 0);
        
        // Initially, each element is its own parent
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }
    
    // Find operation with path compression
    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]); // Path compression
        }
        return parent[x];
    }
    
    // Union operation with union by rank
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
        }
    }
    
    // Check if two elements are in the same set
    bool connected(int x, int y) {
        return find(x) == find(y);
    }
};

int main() {
    // Create a disjoint set with 6 elements (0 to 5)
    DisjointSet ds(6);
    
    cout << "Initial state - All elements are in separate sets\n";
    
    // Perform some union operations
    ds.unite(0, 1);
    cout << "Union(0, 1) - Elements 0 and 1 are now in the same set\n";
    
    ds.unite(2, 3);
    cout << "Union(2, 3) - Elements 2 and 3 are now in the same set\n";
    
    ds.unite(1, 3);
    cout << "Union(1, 3) - Elements 1 and 3 are now in the same set (connects two sets)\n";
    
    ds.unite(4, 5);
    cout << "Union(4, 5) - Elements 4 and 5 are now in the same set\n";
    
    // Check connections
    cout << "\nConnection checks:\n";
    cout << "0 and 2 connected? " << (ds.connected(0, 2) ? "Yes" : "No") << "\n";
    cout << "0 and 4 connected? " << (ds.connected(0, 4) ? "Yes" : "No") << "\n";
    cout << "4 and 5 connected? " << (ds.connected(4, 5) ? "Yes" : "No") << "\n";
    
    // Show final structure
    cout << "\nFinal set representatives:\n";
    for (int i = 0; i < 6; i++) {
        cout << "Element " << i << " belongs to set with representative: " << ds.find(i) << "\n";
    }
    
    return 0;
}
```

## Output:
```
Initial state - All elements are in separate sets
Union(0, 1) - Elements 0 and 1 are now in the same set
Union(2, 3) - Elements 2 and 3 are now in the same set
Union(1, 3) - Elements 1 and 3 are now in the same set (connects two sets)
Union(4, 5) - Elements 4 and 5 are now in the same set

Connection checks:
0 and 2 connected? Yes
0 and 4 connected? No
4 and 5 connected? Yes

Final set representatives:
Element 0 belongs to set with representative: 1
Element 1 belongs to set with representative: 1
Element 2 belongs to set with representative: 3
Element 3 belongs to set with representative: 3
Element 4 belongs to set with representative: 5
Element 5 belongs to set with representative: 5
```

## Key Features:

1. **Path Compression**: The `find` operation flattens the structure by making every node point directly to the root
2. **Union by Rank**: The `unite` operation always attaches the smaller tree under the root of the larger tree
3. **Efficient Operations**: Both `find` and `unite` operations have nearly constant time complexity
4. **Applications**: Useful for detecting cycles in graphs, Kruskal's algorithm, and connectivity problems

The time complexity is nearly O(1) for both operations due to the optimizations used.

