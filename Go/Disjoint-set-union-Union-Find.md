# Disjoint Set Union (Union-Find) Algorithm in Go

```go
package main

import "fmt"

// DisjointSet represents a disjoint set data structure
type DisjointSet struct {
    parent []int
    rank   []int
    size   int
}

// NewDisjointSet creates a new disjoint set with n elements
func NewDisjointSet(n int) *DisjointSet {
    parent := make([]int, n)
    rank := make([]int, n)
    
    // Initialize each element to be its own parent
    for i := 0; i < n; i++ {
        parent[i] = i
        rank[i] = 0
    }
    
    return &DisjointSet{
        parent: parent,
        rank:   rank,
        size:   n,
    }
}

// Find finds the root of the set containing element x with path compression
func (ds *DisjointSet) Find(x int) int {
    if ds.parent[x] != x {
        // Path compression: make every node point directly to the root
        ds.parent[x] = ds.Find(ds.parent[x])
    }
    return ds.parent[x]
}

// Union unites the sets containing elements x and y
func (ds *DisjointSet) Union(x, y int) bool {
    rootX := ds.Find(x)
    rootY := ds.Find(y)
    
    // If they're already in the same set, return false
    if rootX == rootY {
        return false
    }
    
    // Union by rank: attach smaller tree under root of larger tree
    if ds.rank[rootX] < ds.rank[rootY] {
        ds.parent[rootX] = rootY
    } else if ds.rank[rootX] > ds.rank[rootY] {
        ds.parent[rootY] = rootX
    } else {
        // If ranks are equal, choose one as root and increment its rank
        ds.parent[rootY] = rootX
        ds.rank[rootX]++
    }
    
    return true
}

// Connected checks if elements x and y are in the same set
func (ds *DisjointSet) Connected(x, y int) bool {
    return ds.Find(x) == ds.Find(y)
}

// GetSetSize returns the size of the set containing element x
func (ds *DisjointSet) GetSetSize(x int) int {
    root := ds.Find(x)
    // Count all elements that have the same root
    count := 0
    for i := 0; i < ds.size; i++ {
        if ds.Find(i) == root {
            count++
        }
    }
    return count
}

func main() {
    // Create a disjoint set with 6 elements (0-5)
    ds := NewDisjointSet(6)
    
    fmt.Println("Initial state:")
    for i := 0; i < 6; i++ {
        fmt.Printf("Element %d is in set with root %d\n", i, ds.Find(i))
    }
    
    // Union some sets
    fmt.Println("\nUnion operations:")
    fmt.Printf("Union(0, 1): %t\n", ds.Union(0, 1))
    fmt.Printf("Union(2, 3): %t\n", ds.Union(2, 3))
    fmt.Printf("Union(4, 5): %t\n", ds.Union(4, 5))
    fmt.Printf("Union(1, 3): %t\n", ds.Union(1, 3))
    
    fmt.Println("\nAfter unions:")
    for i := 0; i < 6; i++ {
        fmt.Printf("Element %d is in set with root %d\n", i, ds.Find(i))
    }
    
    // Check connectivity
    fmt.Println("\nConnectivity checks:")
    fmt.Printf("Are 0 and 3 connected? %t\n", ds.Connected(0, 3))
    fmt.Printf("Are 0 and 4 connected? %t\n", ds.Connected(0, 4))
    
    // Get set sizes
    fmt.Println("\nSet sizes:")
    for i := 0; i < 6; i++ {
        fmt.Printf("Size of set containing %d: %d\n", i, ds.GetSetSize(i))
    }
    
    // Try to union already connected elements
    fmt.Printf("\nUnion(0, 1) again (should return false): %t\n", ds.Union(0, 1))
}
```

## Output:
```
Initial state:
Element 0 is in set with root 0
Element 1 is in set with root 1
Element 2 is in set with root 2
Element 3 is in set with root 3
Element 4 is in set with root 4
Element 5 is in set with root 5

Union operations:
Union(0, 1): true
Union(2, 3): true
Union(4, 5): true
Union(1, 3): true

After unions:
Element 0 is in set with root 0
Element 1 is in set with root 0
Element 2 is in set with root 0
Element 3 is in set with root 0
Element 4 is in set with root 4
Element 5 is in set with root 4

Connectivity checks:
Are 0 and 3 connected? true
Are 0 and 4 connected? false

Set sizes:
Size of set containing 0: 4
Size of set containing 1: 4
Size of set containing 2: 4
Size of set containing 3: 4
Size of set containing 4: 2
Size of set containing 5: 2

Union(0, 1) again (should return false): false
```

## Key Features:

1. **Path Compression**: The `Find` operation compresses the path, making future operations faster
2. **Union by Rank**: The `Union` operation keeps the tree balanced by attaching smaller trees under larger ones
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity
4. **Memory Efficient**: Uses only O(n) space for n elements

This implementation is commonly used in algorithms like Kruskal's minimum spanning tree algorithm and detecting cycles in graphs.

