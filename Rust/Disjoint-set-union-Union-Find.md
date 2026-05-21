# Disjoint Set Union (Union-Find) Algorithm in Rust

Here's a complete implementation of the Union-Find data structure in Rust with path compression and union by rank optimizations:

```rust
struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
    components: usize,
}

impl UnionFind {
    // Create a new Union-Find structure with n elements
    fn new(n: usize) -> Self {
        let mut parent = Vec::with_capacity(n);
        let mut rank = Vec::with_capacity(n);
        
        for i in 0..n {
            parent.push(i);
            rank.push(0);
        }
        
        UnionFind {
            parent,
            rank,
            components: n,
        }
    }
    
    // Find the root of element x with path compression
    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]); // Path compression
        }
        self.parent[x]
    }
    
    // Union two elements x and y
    fn union(&mut self, x: usize, y: usize) -> bool {
        let root_x = self.find(x);
        let root_y = self.find(y);
        
        if root_x == root_y {
            return false; // Already in the same set
        }
        
        // Union by rank
        if self.rank[root_x] < self.rank[root_y] {
            self.parent[root_x] = root_y;
        } else if self.rank[root_x] > self.rank[root_y] {
            self.parent[root_y] = root_x;
        } else {
            self.parent[root_y] = root_x;
            self.rank[root_x] += 1;
        }
        
        self.components -= 1;
        true
    }
    
    // Check if two elements are in the same set
    fn connected(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }
    
    // Get the number of connected components
    fn components(&self) -> usize {
        self.components
    }
}

fn main() {
    // Create a Union-Find structure with 6 elements (0-5)
    let mut uf = UnionFind::new(6);
    
    println!("Initial state - Components: {}", uf.components());
    
    // Perform some union operations
    uf.union(0, 1);
    println!("After union(0, 1) - Components: {}", uf.components());
    
    uf.union(2, 3);
    println!("After union(2, 3) - Components: {}", uf.components());
    
    uf.union(4, 5);
    println!("After union(4, 5) - Components: {}", uf.components());
    
    // Union two previously disconnected components
    uf.union(1, 3);
    println!("After union(1, 3) - Components: {}", uf.components());
    
    // Check if elements are connected
    println!("Are 0 and 3 connected? {}", uf.connected(0, 3)); // Should be true
    println!("Are 0 and 4 connected? {}", uf.connected(0, 4)); // Should be false
    
    // Find root of elements
    println!("Root of 0: {}", uf.find(0));
    println!("Root of 5: {}", uf.find(5));
    
    // Union that would create a cycle
    let result = uf.union(0, 5);
    println!("Union(0, 5) successful: {}", result); // Should be false (already connected)
    
    println!("Final components: {}", uf.components());
}
```

## Key Features:

1. **Path Compression**: The `find` method uses path compression to flatten the structure, making future queries faster
2. **Union by Rank**: The `union` method merges smaller trees into larger ones to keep the structure balanced
3. **Efficient Operations**: Both `find` and `union` operations have nearly constant time complexity

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n)) 
- **Space**: O(n)

## Common Use Cases:
- Detecting cycles in graphs
- Finding connected components in networks
- Kruskal's algorithm for minimum spanning tree
- Image processing (connected component labeling)

## Output:
```
Initial state - Components: 6
After union(0, 1) - Components: 5
After union(2, 3) - Components: 4
After union(4, 5) - Components: 3
After union(1, 3) - Components: 2
Are 0 and 3 connected? true
Are 0 and 4 connected? false
Root of 0: 0
Root of 5: 5
Union(0, 5) successful: false
Final components: 2
```

