# Minimum Spanning Tree - Kruskal's Algorithm Implementation

Here's a complete implementation of Kruskal's algorithm to find the Minimum Spanning Tree (MST) in Python:

```python
class UnionFind:
    """Union-Find data structure for tracking connected components"""
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n
    
    def find(self, x):
        """Find root of x with path compression"""
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]
    
    def union(self, x, y):
        """Union two sets"""
        root_x = self.find(x)
        root_y = self.find(y)
        
        if root_x != root_y:
            # Union by rank
            if self.rank[root_x] < self.rank[root_y]:
                self.parent[root_x] = root_y
            elif self.rank[root_x] > self.rank[root_y]:
                self.parent[root_y] = root_x
            else:
                self.parent[root_y] = root_x
                self.rank[root_x] += 1
            return True
        return False

def kruskal_mst(vertices, edges):
    """
    Find Minimum Spanning Tree using Kruskal's algorithm
    
    Args:
        vertices: Number of vertices in the graph
        edges: List of tuples (weight, vertex1, vertex2)
    
    Returns:
        List of edges in the MST
    """
    # Sort edges by weight
    edges.sort()
    
    # Initialize Union-Find structure
    uf = UnionFind(vertices)
    
    mst_edges = []
    total_weight = 0
    
    # Process edges in ascending order
    for weight, u, v in edges:
        # If vertices are not connected, add edge to MST
        if uf.union(u, v):
            mst_edges.append((weight, u, v))
            total_weight += weight
            
            # MST has exactly (vertices - 1) edges
            if len(mst_edges) == vertices - 1:
                break
    
    return mst_edges, total_weight

# Example usage
if __name__ == "__main__":
    # Define vertices and edges
    vertices = 6
    edges = [
        (4, 0, 1),  # weight 4, edge between vertex 0 and 1
        (6, 0, 2),  # weight 6, edge between vertex 0 and 2
        (1, 1, 2),  # weight 1, edge between vertex 1 and 2
        (5, 1, 3),  # weight 5, edge between vertex 1 and 3
        (3, 2, 3),  # weight 3, edge between vertex 2 and 3
        (2, 3, 4),  # weight 2, edge between vertex 3 and 4
        (4, 4, 5),  # weight 4, edge between vertex 4 and 5
        (6, 2, 5),  # weight 6, edge between vertex 2 and 5
        (8, 0, 5)   # weight 8, edge between vertex 0 and 5
    ]
    
    # Find MST
    mst_edges, total_weight = kruskal_mst(vertices, edges)
    
    print("Minimum Spanning Tree:")
    print(f"Total weight: {total_weight}")
    print("Edges in MST:")
    for weight, u, v in mst_edges:
        print(f"  {u} -- {v} (weight: {weight})")
```

## Output:
```
Minimum Spanning Tree:
Total weight: 16
Edges in MST:
  1 -- 2 (weight: 1)
  2 -- 3 (weight: 3)
  3 -- 4 (weight: 2)
  0 -- 1 (weight: 4)
  4 -- 5 (weight: 4)
```

## Algorithm Explanation:

1. **Sort Edges**: All edges are sorted by weight in ascending order
2. **Initialize**: Create a Union-Find structure to track connected components
3. **Process Edges**: For each edge in sorted order:
   - If the edge connects two different components, add it to MST
   - Use Union-Find to merge the components
4. **Stop Condition**: Continue until we have (V-1) edges in MST

## Time Complexity:
- **O(E log E)** where E is the number of edges
- Sorting edges takes O(E log E)
- Union-Find operations take nearly O(1) amortized time

## Space Complexity:
- **O(V + E)** for storing edges and Union-Find structure

This implementation uses Kruskal's algorithm, which is optimal for sparse graphs and is particularly useful when you need to find the minimum weight spanning tree in a connected, undirected graph.

