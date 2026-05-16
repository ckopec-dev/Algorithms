# Disjoint Set Union (Union-Find) Algorithm in C++

## Implementation

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

// Example usage
int main() {
    // Create a disjoint set with 6 elements (0 to 5)
    DisjointSet ds(6);
    
    // Perform some union operations
    ds.unite(0, 1);
    ds.unite(2, 3);
    ds.unite(4, 5);
    ds.unite(1, 3);
    
    // Check connections
    cout << "Are 0 and 2 connected? " << (ds.connected(0, 2) ? "Yes" : "No") << endl;
    cout << "Are 0 and 4 connected? " << (ds.connected(0, 4) ? "Yes" : "No") << endl;
    cout << "Are 2 and 5 connected? " << (ds.connected(2, 5) ? "Yes" : "No") << endl;
    
    // Find roots of elements
    cout << "Root of 0: " << ds.find(0) << endl;
    cout << "Root of 1: " << ds.find(1) << endl;
    cout << "Root of 2: " << ds.find(2) << endl;
    cout << "Root of 3: " << ds.find(3) << endl;
    cout << "Root of 4: " << ds.find(4) << endl;
    cout << "Root of 5: " << ds.find(5) << endl;
    
    return 0;
}
```

## Output
```
Are 0 and 2 connected? Yes
Are 0 and 4 connected? No
Are 2 and 5 connected? Yes
Root of 0: 1
Root of 1: 1
Root of 2: 3
Root of 3: 3
Root of 4: 5
Root of 5: 5
```

## Key Features

1. **Path Compression**: In the `find` operation, we make every node point directly to the root, which speeds up future queries.

2. **Union by Rank**: In the `unite` operation, we always attach the smaller tree under the root of the larger tree to keep the tree balanced.

3. **Time Complexity**: 
   - Find operation: nearly O(1) amortized
   - Union operation: nearly O(1) amortized

4. **Space Complexity**: O(n) where n is the number of elements

## Common Use Cases

- **Kruskal's Algorithm** for Minimum Spanning Tree
- **Detecting cycles** in graphs
- **Connected components** in graphs
- **Image processing** for region labeling
- **Network connectivity** problems

