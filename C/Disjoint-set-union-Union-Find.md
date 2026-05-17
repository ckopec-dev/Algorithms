# Disjoint Set Union (Union-Find) Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>

// Structure to represent a disjoint set
struct DisjointSet {
    int *parent;
    int *rank;
    int size;
};

// Create a new disjoint set
struct DisjointSet* createSet(int n) {
    struct DisjointSet* ds = (struct DisjointSet*)malloc(sizeof(struct DisjointSet));
    ds->size = n;
    ds->parent = (int*)malloc(n * sizeof(int));
    ds->rank = (int*)malloc(n * sizeof(int));
    
    // Initialize each element as its own parent
    for (int i = 0; i < n; i++) {
        ds->parent[i] = i;
        ds->rank[i] = 0;
    }
    
    return ds;
}

// Find the root of element x with path compression
int find(struct DisjointSet* ds, int x) {
    if (ds->parent[x] != x) {
        // Path compression: make every node point directly to root
        ds->parent[x] = find(ds, ds->parent[x]);
    }
    return ds->parent[x];
}

// Union two sets containing elements x and y
void unionSets(struct DisjointSet* ds, int x, int y) {
    int rootX = find(ds, x);
    int rootY = find(ds, y);
    
    if (rootX != rootY) {
        // Union by rank: attach smaller tree under root of larger tree
        if (ds->rank[rootX] < ds->rank[rootY]) {
            ds->parent[rootX] = rootY;
        } else if (ds->rank[rootX] > ds->rank[rootY]) {
            ds->parent[rootY] = rootX;
        } else {
            // If ranks are equal, make one root and increment its rank
            ds->parent[rootY] = rootX;
            ds->rank[rootX]++;
        }
    }
}

// Check if two elements are in the same set
int connected(struct DisjointSet* ds, int x, int y) {
    return find(ds, x) == find(ds, y);
}

// Free memory allocated for disjoint set
void freeSet(struct DisjointSet* ds) {
    free(ds->parent);
    free(ds->rank);
    free(ds);
}

// Example usage
int main() {
    // Create a disjoint set with 6 elements (0 to 5)
    struct DisjointSet* ds = createSet(6);
    
    printf("Initial state - All elements in separate sets:\n");
    for (int i = 0; i < 6; i++) {
        printf("Element %d is in set with root %d\n", i, find(ds, i));
    }
    
    // Perform some union operations
    printf("\nPerforming union operations:\n");
    unionSets(ds, 0, 1);
    printf("Union(0, 1) - Root of 0: %d, Root of 1: %d\n", find(ds, 0), find(ds, 1));
    
    unionSets(ds, 2, 3);
    printf("Union(2, 3) - Root of 2: %d, Root of 3: %d\n", find(ds, 2), find(ds, 3));
    
    unionSets(ds, 1, 3);
    printf("Union(1, 3) - Root of 1: %d, Root of 3: %d\n", find(ds, 1), find(ds, 3));
    
    unionSets(ds, 4, 5);
    printf("Union(4, 5) - Root of 4: %d, Root of 5: %d\n", find(ds, 4), find(ds, 5));
    
    // Check connections
    printf("\nChecking connections:\n");
    printf("Are 0 and 3 connected? %s\n", connected(ds, 0, 3) ? "Yes" : "No");
    printf("Are 0 and 4 connected? %s\n", connected(ds, 0, 4) ? "Yes" : "No");
    printf("Are 4 and 5 connected? %s\n", connected(ds, 4, 5) ? "Yes" : "No");
    
    // Final state
    printf("\nFinal state:\n");
    for (int i = 0; i < 6; i++) {
        printf("Element %d is in set with root %d\n", i, find(ds, i));
    }
    
    // Clean up
    freeSet(ds);
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
Union(0, 1) - Root of 0: 0, Root of 1: 0
Union(2, 3) - Root of 2: 2, Root of 3: 2
Union(1, 3) - Root of 1: 0, Root of 3: 0
Union(4, 5) - Root of 4: 4, Root of 5: 4

Checking connections:
Are 0 and 3 connected? Yes
Are 0 and 4 connected? No
Are 4 and 5 connected? Yes

Final state:
Element 0 is in set with root 0
Element 1 is in set with root 0
Element 2 is in set with root 0
Element 3 is in set with root 0
Element 4 is in set with root 4
Element 5 is in set with root 4
```

## Key Features:

1. **Path Compression**: `find()` function uses path compression to optimize future queries
2. **Union by Rank**: `unionSets()` function uses rank to keep trees balanced
3. **Time Complexity**: Nearly O(1) amortized time for both operations
4. **Space Complexity**: O(n) where n is the number of elements

## Common Use Cases:
- Finding connected components in graphs
- Kruskal's algorithm for minimum spanning tree
- Detecting cycles in graphs
- Image processing (connected component labeling)

