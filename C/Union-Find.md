# Union-Find Algorithm in C

Here's a complete implementation of the Union-Find (Disjoint Set) data structure in C with path compression and union by rank optimizations:

```c
#include <stdio.h>
#include <stdlib.h>

// Structure to represent a Union-Find set
typedef struct {
    int *parent;
    int *rank;
    int size;
} UnionFind;

// Create a new Union-Find structure with n elements
UnionFind* createUnionFind(int n) {
    UnionFind *uf = (UnionFind*)malloc(sizeof(UnionFind));
    uf->size = n;
    uf->parent = (int*)malloc(n * sizeof(int));
    uf->rank = (int*)malloc(n * sizeof(int));
    
    // Initialize each element to be its own parent
    for (int i = 0; i < n; i++) {
        uf->parent[i] = i;
        uf->rank[i] = 0;
    }
    
    return uf;
}

// Find the root of element x with path compression
int find(UnionFind *uf, int x) {
    if (uf->parent[x] != x) {
        // Path compression: make every node point directly to root
        uf->parent[x] = find(uf, uf->parent[x]);
    }
    return uf->parent[x];
}

// Union two sets containing elements x and y
void unionSets(UnionFind *uf, int x, int y) {
    int rootX = find(uf, x);
    int rootY = find(uf, y);
    
    if (rootX != rootY) {
        // Union by rank: attach smaller tree under root of larger tree
        if (uf->rank[rootX] < uf->rank[rootY]) {
            uf->parent[rootX] = rootY;
        } else if (uf->rank[rootX] > uf->rank[rootY]) {
            uf->parent[rootY] = rootX;
        } else {
            // If ranks are equal, choose one as root and increment its rank
            uf->parent[rootY] = rootX;
            uf->rank[rootX]++;
        }
    }
}

// Check if two elements are in the same set
int connected(UnionFind *uf, int x, int y) {
    return find(uf, x) == find(uf, y);
}

// Free memory allocated for Union-Find structure
void freeUnionFind(UnionFind *uf) {
    free(uf->parent);
    free(uf->rank);
    free(uf);
}

// Example usage
int main() {
    // Create a Union-Find structure with 6 elements (0-5)
    UnionFind *uf = createUnionFind(6);
    
    printf("Initial state - All elements in separate sets:\n");
    for (int i = 0; i < 6; i++) {
        printf("Element %d is in set with root %d\n", i, find(uf, i));
    }
    
    // Perform some union operations
    printf("\nPerforming union operations:\n");
    unionSets(uf, 0, 1);
    printf("Union(0, 1) - Elements 0 and 1 are now connected\n");
    
    unionSets(uf, 2, 3);
    printf("Union(2, 3) - Elements 2 and 3 are now connected\n");
    
    unionSets(uf, 4, 5);
    printf("Union(4, 5) - Elements 4 and 5 are now connected\n");
    
    unionSets(uf, 1, 3);
    printf("Union(1, 3) - Sets containing 1 and 3 are now merged\n");
    
    // Check connectivity
    printf("\nConnectivity check:\n");
    printf("Are 0 and 2 connected? %s\n", connected(uf, 0, 2) ? "Yes" : "No");
    printf("Are 0 and 4 connected? %s\n", connected(uf, 0, 4) ? "Yes" : "No");
    
    printf("\nFinal state:\n");
    for (int i = 0; i < 6; i++) {
        printf("Element %d is in set with root %d\n", i, find(uf, i));
    }
    
    // Clean up
    freeUnionFind(uf);
    return 0;
}
```

## Output:
```
Initial state - All elements in separate sets:
Element 0 is in set with root 0
Element 1 is in set with root 1
Element 2 is in set with root 2
Element 3 is in set with root 3
Element 4 is in set with root 4
Element 5 is in set with root 5

Performing union operations:
Union(0, 1) - Elements 0 and 1 are now connected
Union(2, 3) - Elements 2 and 3 are now connected
Union(4, 5) - Elements 4 and 5 are now connected
Union(1, 3) - Sets containing 1 and 3 are now merged

Connectivity check:
Are 0 and 2 connected? Yes
Are 0 and 4 connected? No

Final state:
Element 0 is in set with root 0
Element 1 is in set with root 0
Element 2 is in set with root 0
Element 3 is in set with root 0
Element 4 is in set with root 4
Element 5 is in set with root 4
```

## Key Features:

1. **Path Compression**: The `find` operation flattens the structure during traversal, making future operations faster
2. **Union by Rank**: The `union` operation always attaches the smaller tree under the root of the larger tree to keep the structure balanced
3. **Time Complexity**: Nearly constant time for both operations (amortized O(α(n)) where α is the inverse Ackermann function)
4. **Space Complexity**: O(n) for storing parent and rank arrays

This implementation is commonly used in algorithms like Kruskal's Minimum Spanning Tree algorithm and detecting cycles in graphs.