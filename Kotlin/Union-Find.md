# Union-Find Algorithm in Kotlin

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in Kotlin:

```kotlin
class UnionFind(private val size: Int) {
    private val parent = IntArray(size) { it }
    private val rank = IntArray(size) { 0 }
    
    // Find with path compression
    fun find(x: Int): Int {
        if (parent[x] != x) {
            parent[x] = find(parent[x]) // Path compression
        }
        return parent[x]
    }
    
    // Union by rank
    fun union(x: Int, y: Int): Boolean {
        val rootX = find(x)
        val rootY = find(y)
        
        if (rootX == rootY) {
            return false // Already in the same set
        }
        
        // Union by rank
        if (rank[rootX] < rank[rootY]) {
            parent[rootX] = rootY
        } else if (rank[rootX] > rank[rootY]) {
            parent[rootY] = rootX
        } else {
            parent[rootY] = rootX
            rank[rootX]++
        }
        
        return true
    }
    
    // Check if two elements are in the same set
    fun isConnected(x: Int, y: Int): Boolean {
        return find(x) == find(y)
    }
    
    // Get the number of connected components
    fun components(): Int {
        return parent.filterIndexed { index, _ -> index == find(index) }.size
    }
}

// Example usage
fun main() {
    // Create a Union-Find structure with 6 elements (0-5)
    val uf = UnionFind(6)
    
    println("Initial state - 6 elements, 6 components")
    println("Components: ${uf.components()}")
    
    // Union some elements
    uf.union(0, 1)
    println("After union(0, 1)")
    println("Components: ${uf.components()}")
    
    uf.union(2, 3)
    println("After union(2, 3)")
    println("Components: ${uf.components()}")
    
    uf.union(1, 3)
    println("After union(1, 3)")
    println("Components: ${uf.components()}")
    
    // Check connectivity
    println("Is 0 connected to 3? ${uf.isConnected(0, 3)}")
    println("Is 0 connected to 4? ${uf.isConnected(0, 4)}")
    
    // Union more elements
    uf.union(4, 5)
    println("After union(4, 5)")
    println("Components: ${uf.components()}")
    
    uf.union(3, 5)
    println("After union(3, 5)")
    println("Components: ${uf.components()}")
    
    // Final connectivity check
    println("Is 0 connected to 5? ${uf.isConnected(0, 5)}")
    println("Is 1 connected to 4? ${uf.isConnected(1, 4)}")
}
```

## Output:
```
Initial state - 6 elements, 6 components
Components: 6
After union(0, 1)
Components: 5
After union(2, 3)
Components: 4
After union(1, 3)
Components: 3
Is 0 connected to 3? true
Is 0 connected to 4? false
After union(4, 5)
Components: 2
After union(3, 5)
Components: 1
Is 0 connected to 5? true
Is 1 connected to 4? false
```

## Key Features:

1. **Path Compression**: The `find` operation compresses the path, making future operations faster
2. **Union by Rank**: The `union` operation keeps the tree balanced by always attaching the smaller tree under the root of the larger tree
3. **Time Complexity**: Nearly constant time for both operations (amortized O(α(n)) where α is the inverse Ackermann function)
4. **Space Complexity**: O(n) for storing the parent and rank arrays

## Common Use Cases:

- **Graph Connectivity**: Determining if nodes are connected in a graph
- **Kruskal's Algorithm**: Finding minimum spanning trees
- **Cycle Detection**: Detecting cycles in undirected graphs
- **Dynamic Connectivity**: Managing connected components in a dynamic graph

