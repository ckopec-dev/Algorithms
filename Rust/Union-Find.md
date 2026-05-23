# Union-Find Algorithm in Rust

Here's a complete implementation of the Union-Find (Disjoint Set Union) algorithm in Rust with example usage:

```rust
struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
    components: usize,
}

impl UnionFind {
    // Create a new UnionFind structure with n elements
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
            return false; // Already in the same component
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
    
    // Check if two elements are in the same component
    fn connected(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }
    
    // Get the number of connected components
    fn components(&self) -> usize {
        self.components
    }
}

fn main() {
    // Create a UnionFind structure with 6 elements (0-5)
    let mut uf = UnionFind::new(6);
    
    println!("Initial state - Components: {}", uf.components());
    
    // Perform some union operations
    uf.union(0, 1);
    println!("After union(0, 1) - Components: {}", uf.components());
    
    uf.union(2, 3);
    println!("After union(2, 3) - Components: {}", uf.components());
    
    uf.union(4, 5);
    println!("After union(4, 5) - Components: {}", uf.components());
    
    uf.union(1, 3);
    println!("After union(1, 3) - Components: {}", uf.components());
    
    // Check connections
    println!("Are 0 and 2 connected? {}", uf.connected(0, 2)); // Should be true
    println!("Are 0 and 4 connected? {}", uf.connected(0, 4)); // Should be false
    
    // Union 0 and 4 (should create a new connection)
    uf.union(0, 4);
    println!("After union(0, 4) - Components: {}", uf.components());
    println!("Are 0 and 4 connected now? {}", uf.connected(0, 4)); // Should be true
    
    // Show final state
    println!("\nFinal state:");
    for i in 0..6 {
        println!("Element {} belongs to component root {}", i, uf.find(i));
    }
}
```

## Output
```
Initial state - Components: 6
After union(0, 1) - Components: 5
After union(2, 3) - Components: 4
After union(4, 5) - Components: 3
After union(1, 3) - Components: 2
Are 0 and 2 connected? true
Are 0 and 4 connected? false
After union(0, 4) - Components: 1
Are 0 and 4 connected now? true

Final state:
Element 0 belongs to component root 0
Element 1 belongs to component root 0
Element 2 belongs to component root 0
Element 3 belongs to component root 0
Element 4 belongs to component root 0
Element 5 belongs to component root 0
```

## Key Features

1. **Path Compression**: The `find` operation compresses the path to the root for better performance
2. **Union by Rank**: The `union` operation maintains balance between trees
3. **Efficient Operations**: Both `find` and `union` operations are nearly O(1) amortized
4. **Component Tracking**: Keeps track of the number of connected components

## Time Complexity
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

This implementation is commonly used for:
- Detecting cycles in graphs
- Kruskal's algorithm for minimum spanning trees
- Connected components in graphs
- Dynamic connectivity problems

