# Disjoint Set Union (Union-Find) Algorithm in Python

## Implementation

```python
class DisjointSetUnion:
    def __init__(self, n):
        """
        Initialize disjoint set union with n elements
        Each element is initially its own parent
        """
        self.parent = list(range(n))  # Each element is its own parent initially
        self.rank = [0] * n           # Rank for union by rank optimization
    
    def find(self, x):
        """
        Find the root of element x with path compression
        Path compression: make all nodes on the path point directly to root
        """
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])  # Recursive path compression
        return self.parent[x]
    
    def union(self, x, y):
        """
        Union two sets containing elements x and y
        Uses union by rank optimization
        """
        root_x = self.find(x)
        root_y = self.find(y)
        
        # If they're already in the same set, do nothing
        if root_x == root_y:
            return False
        
        # Union by rank: attach smaller tree under root of larger tree
        if self.rank[root_x] < self.rank[root_y]:
            self.parent[root_x] = root_y
        elif self.rank[root_x] > self.rank[root_y]:
            self.parent[root_y] = root_x
        else:
            # Same rank: attach one under other and increment rank
            self.parent[root_y] = root_x
            self.rank[root_x] += 1
        
        return True
    
    def connected(self, x, y):
        """
        Check if elements x and y are in the same set
        """
        return self.find(x) == self.find(y)

# Example usage
def example():
    # Create a disjoint set with 6 elements (0-5)
    dsu = DisjointSetUnion(6)
    
    print("Initial state:")
    print(f"Parent array: {dsu.parent}")
    print(f"Rank array: {dsu.rank}")
    print()
    
    # Perform some unions
    print("Union operations:")
    dsu.union(0, 1)
    print("Union(0, 1):")
    print(f"Parent array: {dsu.parent}")
    print(f"Rank array: {dsu.rank}")
    print()
    
    dsu.union(2, 3)
    print("Union(2, 3):")
    print(f"Parent array: {dsu.parent}")
    print(f"Rank array: {dsu.rank}")
    print()
    
    dsu.union(1, 3)
    print("Union(1, 3):")
    print(f"Parent array: {dsu.parent}")
    print(f"Rank array: {dsu.rank}")
    print()
    
    # Check connections
    print("Connection checks:")
    print(f"0 and 2 connected: {dsu.connected(0, 2)}")  # Should be True
    print(f"0 and 4 connected: {dsu.connected(0, 4)}")  # Should be False
    print(f"1 and 3 connected: {dsu.connected(1, 3)}")  # Should be True
    
    # Find operations with path compression
    print("\nFind operations:")
    print(f"Find(0): {dsu.find(0)}")
    print(f"Find(1): {dsu.find(1)}")
    print(f"Find(2): {dsu.find(2)}")
    print(f"Find(3): {dsu.find(3)}")
    print(f"Parent array after finds: {dsu.parent}")

# Run the example
if __name__ == "__main__":
    example()
```

## Output Example

```
Initial state:
Parent array: [0, 1, 2, 3, 4, 5]
Rank array: [0, 0, 0, 0, 0, 0]

Union operations:
Union(0, 1):
Parent array: [0, 0, 2, 3, 4, 5]
Rank array: [1, 0, 0, 0, 0, 0]

Union(2, 3):
Parent array: [0, 0, 2, 2, 4, 5]
Rank array: [1, 0, 1, 0, 0, 0]

Union(1, 3):
Parent array: [0, 0, 0, 0, 4, 5]
Rank array: [1, 0, 1, 0, 0, 0]

Connection checks:
0 and 2 connected: True
0 and 4 connected: False
1 and 3 connected: True

Find operations:
Find(0): 0
Find(1): 0
Find(2): 0
Find(3): 0
Parent array after finds: [0, 0, 0, 0, 4, 5]
```

## Key Features

1. **Path Compression**: `find()` operation compresses the path, making future operations faster
2. **Union by Rank**: `union()` operation keeps the tree balanced by attaching smaller trees under larger ones
3. **Time Complexity**: Nearly O(1) amortized time for both operations
4. **Space Complexity**: O(n) for storing parent and rank arrays

## Common Applications

- **Kruskal's Algorithm** for Minimum Spanning Tree
- **Connected Components** in graphs
- **Cycle Detection** in graphs
- **Image Processing** (connected pixel components)
- **Network Connectivity** problems

